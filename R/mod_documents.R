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
      shiny::actionButton(ns("btn_import"), "Import",
                          class = "btn-primary w-100"),
      shiny::hr(),
      shiny::h5("Paste text"),
      shiny::textAreaInput(ns("paste_content"), NULL, rows = 6,
                           placeholder = "Paste document text here..."),
      shiny::textInput(ns("paste_name"), "Name",
                       placeholder = "Required"),
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
      DT::datatable(
        dplyr::select(docs(), id, name, n_codings, memo),
        selection = "single",
        rownames  = FALSE,
        options   = list(pageLength = 15, dom = "ftp"),
        colnames  = c("ID", "Name", "Codings", "Memo")
      )
    })

    shiny::observeEvent(input$btn_import, {
      shiny::req(input$file_upload)
      nm <- if (nchar(trimws(input$doc_name)) > 0) input$doc_name else NULL
      tryCatch({
        qc_import_document(
          rv$project,
          path = input$file_upload$datapath,
          name = nm %||% input$file_upload$name,
          memo = input$doc_memo
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
        nchar(trimws(input$paste_content)) > 0,
        nchar(trimws(input$paste_name))    > 0
      )
      tryCatch({
        qc_import_document(rv$project,
                           content = input$paste_content,
                           name    = input$paste_name)
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
