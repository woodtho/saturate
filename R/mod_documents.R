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
      bslib::card_header("Documents"),
      DT::dataTableOutput(ns("tbl_docs"))
    )
  )
}

mod_documents_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

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

    # Row click → activate document and switch to Coding tab
    shiny::observeEvent(input$tbl_docs_rows_selected, {
      row <- input$tbl_docs_rows_selected
      shiny::req(row)
      rv$active_source_id <- docs()$id[[row]]
      shinyjs::runjs(
        "var t = document.querySelectorAll('[data-value=\"Coding\"]');
         if (t.length) t[0].click();"
      )
    })
  })
}
