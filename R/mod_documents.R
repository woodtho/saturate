mod_documents_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::h5("Import Document"),
      shiny::fileInput(
        ns("file_upload"), NULL,
        accept      = c(".txt", ".docx", ".pdf", ".csv", ".md"),
        buttonLabel = "Browse..."
      ),
      shiny::textInput(ns("doc_name"), "Display name",
                       placeholder = "Auto from filename"),
      shiny::textAreaInput(ns("doc_memo"), "Memo", rows = 2),
      shiny::div(
        shiny::tags$label("Source type", class = "form-label"),
        shiny::tags$input(
          id       = ns("doc_source_type"),
          class    = "form-control form-control-sm mb-2",
          type     = "text",
          list     = ns("source_type_suggestions"),
          placeholder = "interview, survey, …"
        ),
        shiny::tags$datalist(
          id = ns("source_type_suggestions"),
          shiny::tags$option(value = "interview"),
          shiny::tags$option(value = "focus_group"),
          shiny::tags$option(value = "survey"),
          shiny::tags$option(value = "observation"),
          shiny::tags$option(value = "document")
        )
      ),
      shiny::actionButton(ns("btn_import"), "Import",
                          class = "btn-primary w-100"),
      shiny::hr(),
      shiny::h5("Paste text"),
      shiny::textAreaInput(ns("paste_content"), NULL, rows = 5,
                           placeholder = "Paste document text here..."),
      shiny::textInput(ns("paste_name"), "Name",
                       placeholder = "Required"),
      shiny::div(
        shiny::tags$label("Source type", class = "form-label"),
        shiny::tags$input(
          id       = ns("paste_source_type"),
          class    = "form-control form-control-sm mb-2",
          type     = "text",
          list     = ns("source_type_suggestions"),
          placeholder = "interview, survey, …"
        )
      ),
      shiny::actionButton(ns("btn_paste"), "Add",
                          class = "btn-outline-primary w-100")
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
      shiny::p(shiny::tags$small(
        "Select a row, then click ",
        shiny::tags$strong("Code →"),
        " to open it in the Coding panel.",
        class = "text-muted ps-2 pt-2 mb-0"
      )),
      DT::dataTableOutput(ns("tbl_docs"))
    )
  )
}

mod_documents_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lv <- shiny::reactiveValues(selected_id = NULL)

    docs <- shiny::reactive({
      rv$refresh_docs
      qc_list_documents(rv$project)
    })

    output$tbl_docs <- DT::renderDataTable({
      df <- docs()
      DT::datatable(
        dplyr::select(df, id, name, source_type, n_codings, memo),
        class     = "table table-hover",
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength = 15, dom = "ftp",
          columnDefs = list(
            list(targets = 0, width = "50px"),
            list(targets = 2, width = "110px", className = "text-muted"),
            list(targets = 3, width = "80px", className = "text-center"),
            list(targets = 4, className = "dt-muted dt-truncate")
          )
        ),
        colnames = c("ID", "Name", "Type", "Codings", "Memo")
      )
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

    # ── Import / paste ─────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_import, {
      shiny::req(input$file_upload)
      nm  <- if (nchar(trimws(input$doc_name %||% "")) > 0) input$doc_name else NULL
      sty <- trimws(input$doc_source_type %||% "")
      tryCatch({
        qc_import_document(
          rv$project,
          path        = input$file_upload$datapath,
          name        = nm %||% input$file_upload$name,
          memo        = input$doc_memo,
          source_type = sty
        )
        rv$refresh_docs <- rv$refresh_docs + 1L
        shinyjs::reset("doc_name")
        shinyjs::reset("doc_memo")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    shiny::observeEvent(input$btn_paste, {
      shiny::req(
        nchar(trimws(input$paste_content %||% "")) > 0,
        nchar(trimws(input$paste_name    %||% "")) > 0
      )
      sty <- trimws(input$paste_source_type %||% "")
      tryCatch({
        qc_import_document(rv$project,
                           content     = input$paste_content,
                           name        = input$paste_name,
                           source_type = sty)
        rv$refresh_docs <- rv$refresh_docs + 1L
        shinyjs::reset("paste_content")
        shinyjs::reset("paste_name")
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
            "Use **bold** for emphasis. Changes are versioned and auditable. ",
            "Existing codings will be flagged for review."
          )
        ),
        shiny::textAreaInput(ns("edit_doc_content"), "Content",
          value = doc$content,
          rows  = 20,
          width = "100%"),
        shiny::textInput(ns("edit_doc_version_memo"), "Change note (optional)",
          placeholder = "What changed and why?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_save_edit_doc"), "Save Version",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$btn_save_edit_doc, {
      shiny::req(lv$selected_id)
      new_content <- input$edit_doc_content %||% ""
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
