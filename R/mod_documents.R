mod_documents_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 310,

      # ── Import from file ────────────────────────────────────────────────────
      shiny::h5("Import Document"),
      qc_help_details(
        "Import help",
        shiny::p(
          "Add each transcript, note, artifact, or pasted response as a document. ",
          "Set a source type during import so Query and Triangulation can compare ",
          "evidence across data sources."
        )
      ),
      shiny::fileInput(
        ns("file_upload"), NULL,
        accept      = c(".txt", ".docx", ".pdf", ".csv", ".md"),
        buttonLabel = "Browse…"
      ),

      shiny::hr(),
      shiny::actionButton(ns("btn_open_paste_modal"), "Paste text…",
        class = "btn-outline-secondary w-100")
    ),

    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center w-100",
          "Documents",
          shiny::div(
            class = "d-flex gap-1",
            shiny::actionButton(ns("btn_code_doc"), "Code →",
              class = "btn-sm btn-primary",
              title = "Open selected document in the Coding panel"),
            shiny::actionButton(ns("btn_edit_doc"), "Edit",
              class = "btn-sm btn-outline-secondary",
              title = "Edit selected document content"),
            shiny::actionButton(ns("btn_delete_doc"), "Delete",
              class = "btn-sm btn-outline-danger",
              title = "Delete selected document")
          )
        )
      ),
      qc_help_note(
        "Select a row, then click ",
        shiny::tags$strong("Code ->"),
        " to open it in the Coding panel. Use Edit when source text changes; ",
        "the app keeps a version history."
      ),
      DT::dataTableOutput(ns("tbl_docs"))
    )
  )
}

mod_documents_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lv <- shiny::reactiveValues(
      selected_id      = NULL,
      pending_content  = NULL,
      pending_filename = NULL
    )

    docs <- shiny::reactive({
      rv$refresh_docs
      qc_list_documents(rv$project)
    })

    output$tbl_docs <- DT::renderDataTable({
      df <- docs()
      tbl <- DT::datatable(
        dplyr::select(df, id, name, source_type, word_count, char_count,
          n_codings, n_coders, memo),
        class     = "table table-hover",
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength = 15, dom = "ftp",
          columnDefs = list(
            list(targets = 0, width = "50px"),
            list(targets = 2, width = "110px", className = "text-muted"),
            list(targets = c(3, 4, 5, 6), width = "80px",
              className = "text-center"),
            list(targets = 7, className = "dt-muted dt-truncate")
          )
        ),
        colnames = c("ID", "Name", "Type", "Words", "Chars",
          "Codings", "Coders", "Memo")
      )
      DT::formatRound(tbl, columns = c("word_count", "char_count"),
        digits = 0, mark = ",")
    })

    # Track selected row
    shiny::observeEvent(input$tbl_docs_rows_selected, {
      row <- input$tbl_docs_rows_selected
      lv$selected_id <- if (!is.null(row)) docs()$id[[row]] else NULL
    })

    # "Code →" button: set active document and switch to Coding tab
    shiny::observeEvent(input$btn_code_doc, {
      shiny::req(lv$selected_id)
      rv$active_source_id <- lv$selected_id
      shinyjs::runjs(
        "var t = document.querySelectorAll('[data-value=\"Coding\"]');
         if (t.length) t[0].click();"
      )
    })

    # ── File upload: parse then show import preview modal ─────────────────────

    shiny::observeEvent(input$file_upload, {
      f <- input$file_upload
      shiny::req(f)
      ext     <- tolower(fs::path_ext(f$name))
      content <- tryCatch(
        .read_file_content(f$datapath, ext),
        error = function(e) {
          shiny::showNotification(
            paste0("Could not read file: ", conditionMessage(e)), type = "error")
          NULL
        }
      )
      shiny::req(!is.null(content))

      lv$pending_content  <- content
      lv$pending_filename <- f$name

      n_chars  <- nchar(content)
      n_words  <- length(strsplit(trimws(content), "\\s+")[[1L]])
      preview  <- if (n_chars > 800L) paste0(substr(content, 1L, 800L), "…") else content
      auto_nm  <- fs::path_ext_remove(f$name)

      shiny::showModal(shiny::modalDialog(
        title     = paste0("Import: ", f$name),
        size      = "l",
        easyClose = FALSE,

        shiny::div(
          class = "qc-import-preview mb-3",
          shiny::div(
            class = "qc-preview-header",
            paste0(
              formatC(n_chars, format = "d", big.mark = ","), " chars  ·  ",
              formatC(n_words, format = "d", big.mark = ","), " words"
            )
          ),
          shiny::tags$pre(class = "qc-preview-text", preview)
        ),

        shiny::textInput(ns("import_modal_name"), "Display name",
          value       = auto_nm,
          placeholder = "Required"),
        shiny::textAreaInput(ns("import_modal_memo"), "Memo",
          rows  = 2,
          width = "100%"),
        shiny::div(
          shiny::tags$label("Source type", class = "form-label"),
          shiny::tags$input(
            id          = ns("import_modal_source_type"),
            class       = "form-control form-control-sm",
            type        = "text",
            list        = ns("import_modal_source_type_list"),
            placeholder = "interview, survey, …"
          ),
          shiny::tags$datalist(
            id = ns("import_modal_source_type_list"),
            shiny::tags$option(value = "interview"),
            shiny::tags$option(value = "focus_group"),
            shiny::tags$option(value = "survey"),
            shiny::tags$option(value = "observation"),
            shiny::tags$option(value = "document")
          )
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_import"), "Import",
            class = "btn-primary")
        )
      ))
    })

    # ── Confirm import from modal ──────────────────────────────────────────────

    shiny::observeEvent(input$btn_confirm_import, {
      shiny::req(lv$pending_content, input$file_upload)
      nm  <- trimws(input$import_modal_name %||% "")
      sty <- trimws(input$import_modal_source_type %||% "")
      if (nchar(nm) == 0L) nm <- fs::path_ext_remove(lv$pending_filename)
      tryCatch({
        qc_import_document(
          rv$project,
          path        = input$file_upload$datapath,
          name        = nm,
          memo        = input$import_modal_memo %||% "",
          source_type = sty
        )
        lv$pending_content  <- NULL
        lv$pending_filename <- NULL
        rv$refresh_docs <- rv$refresh_docs + 1L
        shiny::removeModal()
        shinyjs::reset("file_upload")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Paste text modal ───────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_open_paste_modal, {
      shiny::showModal(shiny::modalDialog(
        title     = "Import: Paste Text",
        size      = "l",
        easyClose = FALSE,

        shiny::textAreaInput(ns("paste_modal_content"), "Text",
          rows        = 8,
          width       = "100%",
          placeholder = "Paste document text here…"),
        shiny::textInput(ns("paste_modal_name"), "Display name",
          placeholder = "Required"),
        shiny::textAreaInput(ns("paste_modal_memo"), "Memo",
          rows  = 2,
          width = "100%"),
        shiny::div(
          shiny::tags$label("Source type", class = "form-label"),
          shiny::tags$input(
            id          = ns("paste_modal_source_type"),
            class       = "form-control form-control-sm",
            type        = "text",
            list        = ns("paste_modal_source_type_list"),
            placeholder = "interview, survey, …"
          ),
          shiny::tags$datalist(
            id = ns("paste_modal_source_type_list"),
            shiny::tags$option(value = "interview"),
            shiny::tags$option(value = "focus_group"),
            shiny::tags$option(value = "survey"),
            shiny::tags$option(value = "observation"),
            shiny::tags$option(value = "document")
          )
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_paste"), "Import",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_paste, {
      nm      <- trimws(input$paste_modal_name    %||% "")
      content <- input$paste_modal_content %||% ""
      sty     <- trimws(input$paste_modal_source_type %||% "")
      memo_v  <- input$paste_modal_memo %||% ""
      if (nchar(trimws(content)) == 0L) {
        shiny::showNotification("Text cannot be empty.", type = "warning")
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
        shiny::removeModal()
        shiny::showNotification("Document imported.", type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Edit document content ──────────────────────────────────────────────────

    shiny::observeEvent(input$btn_edit_doc, {
      shiny::req(lv$selected_id)
      doc <- tryCatch(
        qc_get_document(rv$project, lv$selected_id),
        error = function(e) NULL
      )
      shiny::req(!is.null(doc))

      shiny::showModal(shiny::modalDialog(
        title     = paste0("Edit: ", doc$name),
        size      = "l",
        easyClose = FALSE,
        shiny::p(
          class = "text-muted",
          shiny::tags$small(
            "Edit the content below, or upload a new file version. ",
            "Changes are versioned. Existing codings will be flagged for review."
          )
        ),
        shiny::textAreaInput(ns("edit_doc_content"), "Content",
          value = doc$content,
          rows  = 18,
          width = "100%"),
        shiny::div(
          class = "mt-2 p-2 border rounded bg-light",
          shiny::tags$small(
            shiny::tags$strong("Upload a new version:"),
            " replaces the content above with the parsed file text."
          ),
          shiny::fileInput(ns("edit_doc_file"), NULL,
            accept      = c(".txt", ".docx", ".pdf", ".csv", ".md"),
            buttonLabel = "Choose file…",
            placeholder = "No file chosen"
          )
        ),
        shiny::textInput(ns("edit_doc_version_memo"), "Change note (optional)",
          placeholder = "What changed and why?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_save_edit_doc"), "Save Version",
            class = "btn-primary")
        )
      ))
    })

    # When a file is chosen inside the Edit modal, load its content into the textarea
    shiny::observeEvent(input$edit_doc_file, {
      f <- input$edit_doc_file
      shiny::req(f)
      ext     <- tolower(fs::path_ext(f$name))
      content <- tryCatch(
        .read_file_content(f$datapath, ext),
        error = function(e) {
          shiny::showNotification(
            paste0("Could not read file: ", conditionMessage(e)), type = "error")
          NULL
        }
      )
      shiny::req(!is.null(content))
      shiny::updateTextAreaInput(session, "edit_doc_content", value = content)
      shiny::showNotification(
        paste0("File loaded (", formatC(nchar(content), format="d", big.mark=","),
               " chars). Review then save."),
        type = "message"
      )
    })

    shiny::observeEvent(input$btn_save_edit_doc, {
      shiny::req(lv$selected_id)
      new_content  <- input$edit_doc_content %||% ""
      version_memo <- trimws(input$edit_doc_version_memo %||% "")
      tryCatch({
        qc_update_document_content(rv$project, lv$selected_id,
          content = new_content,
          memo    = version_memo)
        rv$refresh_docs  <- rv$refresh_docs  + 1L
        rv$refresh_codes <- rv$refresh_codes + 1L
        shiny::removeModal()
        shiny::showNotification("Document updated. Codings flagged for review.",
                                type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Delete document ────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_delete_doc, {
      shiny::req(lv$selected_id)
      doc_name <- docs()$name[docs()$id == lv$selected_id]
      shiny::showModal(shiny::modalDialog(
        title     = "Delete document?",
        size      = "s",
        easyClose = TRUE,
        shiny::p(paste0(
          'Permanently remove "', doc_name, '" and all its codings? ',
          'This cannot be undone.'
        )),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_delete_doc"), "Delete",
            class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_delete_doc, {
      shiny::req(lv$selected_id)
      tryCatch({
        qc_delete_document(rv$project, lv$selected_id)
        if (identical(rv$active_source_id, lv$selected_id))
          rv$active_source_id <- NULL
        lv$selected_id   <- NULL
        rv$refresh_docs  <- rv$refresh_docs  + 1L
        rv$refresh_codes <- rv$refresh_codes + 1L
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })
  })
}
