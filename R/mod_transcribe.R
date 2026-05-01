mod_transcribe_ui <- function(id) {
  ns      <- shiny::NS(id)
  js_file <- system.file("app", "recording.js", package = "saturate")

  shiny::tagList(
    shiny::includeScript(js_file),
    shiny::actionButton(
      ns("btn_open"),
      shiny::tagList(shiny::icon("microphone"), " Record & transcribe…"),
      class = "btn-outline-secondary w-100"
    )
  )
}

mod_transcribe_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lv <- shiny::reactiveValues(
      audio_path  = NULL,
      audio_ext   = "webm",
      audio_ready = FALSE
    )

    shiny::observeEvent(input$btn_open, {
      lv$audio_path  <- NULL
      lv$audio_ready <- FALSE
      shiny::showModal(.rec_modal(ns, .source_type_options(rv$project)))
      session$sendCustomMessage("qc_rec_init", list(ns = ns("")))
    })

    # Browser-recorded audio arrives as a base64 data URL
    shiny::observeEvent(input$audio_dataurl, {
      shiny::req(nzchar(input$audio_dataurl %||% ""))
      if (!is.null(lv$audio_path)) unlink(lv$audio_path)
      path <- tryCatch(
        .decode_audio_dataurl(input$audio_dataurl),
        error = function(e) {
          shiny::showNotification(
            paste0("Audio decode error: ", conditionMessage(e)), type = "error")
          NULL
        }
      )
      lv$audio_path  <- path
      lv$audio_ext   <- if (!is.null(path)) tolower(fs::path_ext(path)) else "webm"
      lv$audio_ready <- !is.null(path)
    })

    # Uploaded audio file
    shiny::observeEvent(input$audio_file, {
      f <- input$audio_file
      shiny::req(f)
      if (!is.null(lv$audio_path)) unlink(lv$audio_path)
      ext <- tolower(fs::path_ext(f$name))
      tmp <- tempfile(fileext = paste0(".", ext))
      file.copy(f$datapath, tmp)
      lv$audio_path  <- tmp
      lv$audio_ext   <- ext
      lv$audio_ready <- TRUE
    })

    # Enable/disable Transcribe and Save buttons together
    shiny::observe({
      if (isTRUE(lv$audio_ready)) {
        shinyjs::enable("btn_transcribe")
        shinyjs::enable("btn_dl_audio")
      } else {
        shinyjs::disable("btn_transcribe")
        shinyjs::disable("btn_dl_audio")
      }
    })

    # Download the raw recording
    output$btn_dl_audio <- shiny::downloadHandler(
      filename = function() {
        nm  <- trimws(input$doc_name %||% "")
        base <- if (nzchar(nm)) gsub("[^A-Za-z0-9_-]", "_", nm) else "recording"
        paste0(base, ".", lv$audio_ext)
      },
      content = function(file) {
        shiny::req(lv$audio_path)
        file.copy(lv$audio_path, file)
      }
    )

    # Export transcript as plain text or docx
    output$dl_transcript_txt <- shiny::downloadHandler(
      filename = function() {
        nm <- trimws(input$doc_name %||% "")
        paste0(if (nzchar(nm)) gsub("[^A-Za-z0-9_-]", "_", nm) else "transcript", ".txt")
      },
      content = function(file) writeLines(input$transcript %||% "", file, useBytes = FALSE)
    )

    output$dl_transcript_docx <- shiny::downloadHandler(
      filename = function() {
        nm <- trimws(input$doc_name %||% "")
        paste0(if (nzchar(nm)) gsub("[^A-Za-z0-9_-]", "_", nm) else "transcript", ".docx")
      },
      content = function(file) .export_as_docx(input$transcript %||% "", file)
    )

    shiny::observeEvent(input$btn_transcribe, {
      shiny::req(lv$audio_path)
      model    <- input$model    %||% "tiny"
      language <- trimws(input$language %||% "")

      result <- tryCatch(
        shiny::withProgress(
          message = "Starting transcription…",
          detail  = "",
          value   = 0,
          .transcribe_audio(
            lv$audio_path,
            model    = model,
            language = if (nzchar(language)) language else NULL,
            .progress = function(msg, detail) {
              shiny::setProgress(message = msg, detail = detail)
            }
          )
        ),
        error = function(e) {
          shiny::showNotification(
            paste0("Transcription failed: ", conditionMessage(e)),
            type = "error", duration = NULL)
          NULL
        }
      )
      shiny::req(!is.null(result))

      # result$segments (start/end in seconds) is the current API;
      # result$data (from/to in ms) was an older layout — handle both.
      seg_data <- result$segments %||% result$data
      if (!is.null(seg_data) && "start" %in% names(seg_data)) {
        ts_col  <- "start"
        ts_mult <- 1000   # seconds → ms for .format_ms
      } else if (!is.null(seg_data) && "from" %in% names(seg_data)) {
        ts_col  <- "from"
        ts_mult <- 1      # already ms
      } else {
        ts_col  <- NA_character_
        ts_mult <- 1
      }
      use_ts <- isTRUE(input$timestamps) &&
                !is.null(seg_data) &&
                !is.na(ts_col) &&
                "text" %in% names(seg_data)
      txt <- if (!is.null(seg_data) && "text" %in% names(seg_data)) {
        if (use_ts) {
          paste(
            mapply(function(ts, t) paste(.format_ms(ts * ts_mult), trimws(t)),
                   seg_data[[ts_col]], seg_data$text),
            collapse = "\n"
          )
        } else {
          paste(trimws(seg_data$text), collapse = "\n")
        }
      } else {
        result$text %||% ""
      }
      shiny::updateTextAreaInput(session, "transcript", value = txt)
      shinyjs::enable("btn_import")
      shinyjs::enable("dl_transcript_txt")
      shinyjs::enable("dl_transcript_docx")
    })

    shiny::observeEvent(input$btn_import, {
      shiny::req(rv$project)
      content <- input$transcript %||% ""
      nm      <- trimws(input$doc_name %||% "")
      sty     <- trimws(input$source_type %||% "")
      memo_v  <- input$doc_memo %||% ""
      if (nchar(trimws(content)) == 0L) {
        shiny::showNotification("Transcript is empty.", type = "warning")
        return()
      }
      if (nchar(nm) == 0L) {
        shiny::showNotification("Display name is required.", type = "warning")
        return()
      }
      tryCatch({
        qc_import_document(rv$project,
          content     = content,
          name        = nm,
          source_type = sty,
          memo        = memo_v)
        rv$refresh_docs <- rv$refresh_docs + 1L
        if (!is.null(lv$audio_path)) { unlink(lv$audio_path); lv$audio_path <- NULL }
        lv$audio_ready <- FALSE
        shiny::removeModal()
        shiny::showNotification("Transcript imported.", type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })
  })
}

.rec_modal <- function(ns, source_types = c("interview", "focus_group", "survey", "observation", "document")) {
  shiny::modalDialog(
    title     = shiny::tagList(shiny::icon("microphone"), " Record & Transcribe"),
    size      = "l",
    easyClose = FALSE,

    bslib::navset_tab(
      id = ns("tabs"),

      bslib::nav_panel(
        shiny::tagList(shiny::icon("microphone"), " Record"),
        shiny::div(
          class     = "qc-recorder mt-3",
          `data-ns` = ns(""),
          shiny::div(
            class = "qc-rec-controls",
            shiny::actionButton(ns("btn_rec_start"),
              shiny::tagList(shiny::icon("circle"), " Record"),
              class = "qc-rec-start btn btn-outline-danger"),
            shiny::tags$button(
              class    = "qc-rec-pause btn btn-outline-secondary",
              type     = "button",
              disabled = NA,
              shiny::tags$span(
                class = "qc-rec-pause-label",
                shiny::icon("pause"), " Pause"
              ),
              shiny::tags$span(
                class = "qc-rec-resume-label",
                style = "display:none",
                shiny::icon("play"), " Resume"
              )
            ),
            shiny::actionButton(ns("btn_rec_stop"),
              shiny::tagList(shiny::icon("stop"), " Stop"),
              class = "qc-rec-stop btn btn-secondary",
              disabled = NA),
            shiny::span(class = "qc-rec-timer font-monospace fw-bold", "0:00"),
            shiny::span(class = "qc-rec-status text-muted small", "")
          ),
          shiny::tags$canvas(
            class         = "qc-rec-waveform w-100",
            `aria-hidden` = "true",
            height        = "48"
          )
        )
      ),

      bslib::nav_panel(
        shiny::tagList(shiny::icon("upload"), " Upload audio"),
        shiny::div(
          class = "mt-3",
          shiny::fileInput(ns("audio_file"), "Audio file",
            accept      = c(".webm", ".ogg", ".mp4", ".mp3", ".wav", ".m4a"),
            buttonLabel = "Choose…",
            width       = "100%")
        )
      )
    ),

    shiny::div(
      class     = "qc-audio-player mt-3",
      `data-ns` = ns(""),
      shiny::tags$audio(class = "qc-audio-el", preload = "metadata"),
      shiny::div(
        class = "qc-audio-controls",
        shiny::tags$button(
          type     = "button",
          class    = "qc-audio-play btn btn-outline-secondary btn-sm",
          disabled = NA,
          shiny::icon("play")
        ),
        shiny::tags$button(
          type     = "button",
          class    = "qc-audio-stop btn btn-outline-secondary btn-sm",
          disabled = NA,
          shiny::icon("stop")
        ),
        shiny::span(class = "qc-audio-current font-monospace", "0:00"),
        shiny::tags$input(
          class    = "qc-audio-timeline",
          type     = "range",
          min      = "0",
          max      = "0",
          step     = "0.01",
          value    = "0",
          disabled = NA,
          `aria-label` = "Audio timeline"
        ),
        shiny::span(class = "qc-audio-duration font-monospace", "0:00")
      ),
      shiny::div(class = "qc-audio-status text-muted small",
        "Record or upload audio to enable playback.")
    ),

    shiny::hr(class = "my-2"),

    shiny::div(
      class = "d-flex flex-wrap gap-3 align-items-end mb-3",
      shiny::div(
        shiny::tags$label("Model", class = "form-label mb-1"),
        shiny::selectInput(ns("model"), NULL,
          choices  = c(
            "tiny — 74 MB"    = "tiny",
            "base — 142 MB"   = "base",
            "small — 466 MB"  = "small",
            "medium — 1.5 GB" = "medium"),
          selected = "tiny",
          width    = "160px")
      ),
      shiny::div(
        shiny::tags$label(
          "Language",
          shiny::tags$small(class = "text-muted ms-1", "(optional, e.g. en, fr)"),
          class = "form-label mb-1"),
        shiny::textInput(ns("language"), NULL,
          placeholder = "auto-detect",
          width       = "150px")
      ),
      shiny::div(
        class = "mt-auto",
        shiny::checkboxInput(ns("timestamps"), "Timestamps", value = TRUE)
      ),
      shiny::div(
        class = "mt-auto d-flex gap-2",
        shinyjs::disabled(
          shiny::downloadButton(ns("btn_dl_audio"),
            shiny::tagList(shiny::icon("download"), " Save audio"),
            class = "btn-outline-secondary")
        ),
        shinyjs::disabled(
          shiny::actionButton(ns("btn_transcribe"),
            shiny::tagList(shiny::icon("wand-magic-sparkles"), " Transcribe"),
            class = "btn-primary")
        )
      )
    ),

    shiny::div(
      shiny::div(
        class = "d-flex justify-content-between align-items-baseline mb-1",
        shiny::tags$label(
          "Transcript",
          shiny::tags$small(class = "text-muted ms-2", "Edit before importing if needed"),
          class = "form-label mb-0"),
        shiny::div(
          class = "d-flex gap-1",
          shinyjs::disabled(
            shiny::downloadButton(ns("dl_transcript_txt"),
              shiny::tagList(shiny::icon("download"), " .txt"),
              class = "btn-outline-secondary btn-sm",
              title = "Download transcript as plain text")
          ),
          shinyjs::disabled(
            shiny::downloadButton(ns("dl_transcript_docx"),
              shiny::tagList(shiny::icon("download"), " .docx"),
              class = "btn-outline-secondary btn-sm",
              title = "Download transcript as Word document")
          )
        )
      ),
      shiny::textAreaInput(ns("transcript"), NULL,
        value       = "",
        rows        = 8,
        width       = "100%",
        placeholder = "Transcript will appear here after transcription…")
    ),

    shiny::hr(class = "my-2"),

    shiny::div(
      class = "row g-2",
      shiny::div(
        class = "col-md-5",
        shiny::textInput(ns("doc_name"), "Document name",
          placeholder = "Required")
      ),
      shiny::div(
        class = "col-md-4",
        shiny::tags$label("Source type", class = "form-label"),
        shiny::tags$input(
          id          = ns("source_type"),
          class       = "form-control form-control-sm",
          type        = "text",
          list        = ns("source_type_list"),
          placeholder = "interview, survey, …"
        ),
        do.call(shiny::tags$datalist, c(
          list(id = ns("source_type_list")),
          lapply(source_types, function(t) shiny::tags$option(value = t))
        ))
      ),
      shiny::div(
        class = "col-md-3",
        shiny::textAreaInput(ns("doc_memo"), "Memo",
          rows  = 2,
          width = "100%")
      )
    ),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shinyjs::disabled(
        shiny::actionButton(ns("btn_import"), "Import transcript",
          class = "btn-primary")
      )
    )
  )
}
