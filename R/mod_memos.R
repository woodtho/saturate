mod_memos_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 310,

      shiny::h5("New Journal Entry"),
      qc_help_note(
        "Reflexive journals and analytical memos are central to rigorous qualitative ",
        "analysis. Write decisions, hunches, methodological notes, or reflections here."
      ),
      shiny::selectInput(ns("new_memo_type"), "Entry type",
        choices = c(
          "Analytical"     = "analytical",
          "Reflexivity"    = "reflexivity",
          "Decision"       = "decision",
          "Methodological" = "methodological",
          "Other"          = "other"
        )
      ),
      shiny::textAreaInput(ns("new_memo_content"), NULL,
        rows        = 10,
        placeholder = "Write your memo here. Markdown is supported."
      ),
      shiny::actionButton(ns("btn_add_memo"), "Save entry",
        class = "btn-primary w-100"),

      shiny::hr(),

      shiny::tags$p(class = "text-muted small mb-1 mt-0", "Export journal"),
      shiny::div(
        class = "d-flex gap-2 flex-wrap",
        shiny::downloadButton(ns("dl_memos_csv"),  "CSV",
          class = "btn-sm btn-outline-secondary"),
        shiny::downloadButton(ns("dl_memos_txt"),  "Text",
          class = "btn-sm btn-outline-secondary"),
        shiny::downloadButton(ns("dl_memos_docx"), "Word",
          class = "btn-sm btn-outline-secondary"),
        shiny::downloadButton(ns("dl_memos_html"), "HTML",
          class = "btn-sm btn-outline-secondary")
      )
    ),

    shiny::div(
      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "d-flex justify-content-between align-items-center w-100",
            shiny::span("Journal"),
            shiny::div(
              class = "d-flex gap-2 align-items-center",
              shiny::selectInput(ns("filter_type"), NULL,
                choices = c(
                  "All types"      = "",
                  "Analytical"     = "analytical",
                  "Reflexivity"    = "reflexivity",
                  "Decision"       = "decision",
                  "Methodological" = "methodological",
                  "Other"          = "other"
                ),
                width = "160px"
              ),
              shiny::textInput(ns("filter_search"), NULL,
                placeholder = "Search…",
                width       = "180px"
              )
            )
          )
        ),
        shiny::uiOutput(ns("memo_list"))
      )
    )
  )
}

mod_memos_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lv <- shiny::reactiveValues(refresh = 0L)

    memos <- shiny::reactive({
      lv$refresh
      type_filter <- input$filter_type %||% ""
      if (nchar(type_filter) == 0L)
        qc_list_project_memos(rv$project)
      else
        qc_list_project_memos(rv$project, type = type_filter)
    })

    filtered_memos <- shiny::reactive({
      df     <- memos()
      query  <- trimws(input$filter_search %||% "")
      if (nrow(df) == 0L || nchar(query) == 0L) return(df)
      hits <- grepl(query, df$content, ignore.case = TRUE) |
              grepl(query, df$created_by, ignore.case = TRUE)
      df[hits, , drop = FALSE]
    })

    output$memo_list <- shiny::renderUI({
      df <- filtered_memos()
      if (nrow(df) == 0L) {
        return(shiny::div(
          class = "p-4 text-muted text-center",
          "No journal entries yet. Write the first one in the sidebar."
        ))
      }
      entries <- lapply(seq_len(nrow(df)), function(i) {
        row   <- df[i, , drop = FALSE]
        badge <- .memo_type_badge(row$memo_type)
        ts    <- format(as.POSIXct(row$created_at), "%d %b %Y %H:%M")
        shiny::div(
          class = "qc-memo-card card mb-2",
          shiny::div(
            class = "card-header d-flex justify-content-between align-items-center py-2",
            shiny::div(
              class = "d-flex gap-2 align-items-center",
              badge,
              shiny::span(class = "text-muted small", ts),
              shiny::span(class = "text-muted small", paste0("— ", row$created_by))
            ),
            shiny::actionButton(
              ns(paste0("btn_del_memo_", row$id)),
              shiny::icon("trash"),
              class        = "btn-sm btn-outline-danger",
              `aria-label` = "Delete this memo",
              onclick      = sprintf(
                "Shiny.setInputValue('%s', %d, {priority:'event'})",
                ns("delete_memo_id"), row$id
              )
            )
          ),
          shiny::div(
            class = "card-body py-2",
            shiny::markdown(row$content)
          )
        )
      })
      shiny::tagList(entries)
    })

    shiny::observeEvent(input$btn_add_memo, {
      content <- trimws(input$new_memo_content %||% "")
      if (nchar(content) == 0L) {
        shiny::showNotification("Write something before saving.", type = "warning")
        return()
      }
      tryCatch({
        qc_add_project_memo(
          rv$project,
          content    = content,
          type       = input$new_memo_type %||% "analytical",
          created_by = rv$current_coder %||% Sys.info()[["user"]] %||% "default"
        )
        lv$refresh <- lv$refresh + 1L
        shiny::updateTextAreaInput(session, "new_memo_content", value = "")
        shiny::showNotification("Entry saved.", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$delete_memo_id, {
      mid <- as.integer(input$delete_memo_id)
      if (is.na(mid)) return()
      shiny::showModal(shiny::modalDialog(
        title     = "Delete Entry",
        easyClose = TRUE,
        shiny::p("Permanently remove this journal entry?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_delete_memo"), "Delete",
            class = "btn-danger")
        )
      ))
      lv$pending_memo_id <- mid
    })

    shiny::observeEvent(input$btn_confirm_delete_memo, {
      mid <- lv$pending_memo_id
      if (is.null(mid)) return()
      tryCatch({
        qc_delete_project_memo(rv$project, mid)
        lv$refresh         <- lv$refresh + 1L
        lv$pending_memo_id <- NULL
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    output$dl_memos_csv <- shiny::downloadHandler(
      filename = function() paste0("journal_", Sys.Date(), ".csv"),
      content  = function(file) {
        df <- tryCatch(qc_list_project_memos(rv$project),
                       error = function(e) tibble::tibble())
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_memos_txt <- shiny::downloadHandler(
      filename = function() paste0("journal_", Sys.Date(), ".txt"),
      content  = function(file) {
        df <- tryCatch(qc_list_project_memos(rv$project),
                       error = function(e) tibble::tibble())
        lines <- character(0)
        for (i in seq_len(nrow(df))) {
          row <- df[i, , drop = FALSE]
          ts  <- format(as.POSIXct(row$created_at), "%d %b %Y %H:%M")
          lines <- c(lines,
            paste0("[", toupper(row$memo_type), "] ", ts, " — ", row$created_by),
            row$content,
            ""
          )
        }
        writeLines(lines, file)
      }
    )

    output$dl_memos_docx <- shiny::downloadHandler(
      filename = function() paste0("journal_", Sys.Date(), ".docx"),
      content  = function(file) {
        tryCatch({
          file.copy(.export_memos_docx(rv$project), file)
        }, error = function(e) {
          shiny::showNotification(paste0("Export error: ", conditionMessage(e)),
            type = "error")
        })
      }
    )

    output$dl_memos_html <- shiny::downloadHandler(
      filename = function() paste0("journal_", Sys.Date(), ".html"),
      content  = function(file) {
        tryCatch({
          file.copy(.export_memos_html(rv$project), file)
        }, error = function(e) {
          shiny::showNotification(paste0("Export error: ", conditionMessage(e)),
            type = "error")
        })
      }
    )
  })
}

.memo_type_badge <- function(type) {
  colour <- switch(type %||% "other",
    analytical     = "primary",
    reflexivity    = "info",
    decision       = "warning",
    methodological = "secondary",
    "dark"
  )
  shiny::span(class = paste0("badge bg-", colour, " text-white"),
    tools::toTitleCase(type %||% "other"))
}
