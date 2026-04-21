mod_query_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 280,
      shiny::h5("Filters"),
      shiny::selectizeInput(ns("filter_codes"), "Codes",
                            choices = NULL, multiple = TRUE,
                            options = list(placeholder = "All codes")),
      shiny::selectizeInput(ns("filter_sources"), "Documents",
                            choices = NULL, multiple = TRUE,
                            options = list(placeholder = "All documents")),
      shiny::selectizeInput(ns("filter_cases"), "Cases",
                            choices = NULL, multiple = TRUE,
                            options = list(placeholder = "All cases")),
      shiny::selectizeInput(ns("filter_cats"), "Categories",
                            choices = NULL, multiple = TRUE,
                            options = list(placeholder = "All categories")),
      shiny::hr(),
      shiny::actionButton(ns("btn_run"), "Run Query",
                          class = "btn-primary w-100"),
      shiny::br(), shiny::br(),
      shiny::downloadButton(ns("btn_csv"), "Export CSV",
                            class = "btn-outline-secondary w-100"),
      shiny::br(), shiny::br(),
      shiny::uiOutput(ns("summary_text"))
    ),
    bslib::card(
      bslib::card_header("Coded Segments"),
      DT::dataTableOutput(ns("tbl_results"))
    )
  )
}

mod_query_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # Populate filter selectors whenever codes/docs change
    shiny::observe({
      rv$refresh_codes
      rv$refresh_docs
      codes   <- qc_list_codes(rv$project)
      docs    <- qc_list_documents(rv$project)
      cases   <- qc_list_cases(rv$project)
      cats    <- .query(rv$project$con,
        "SELECT id, name FROM code_categories WHERE status = 1 ORDER BY name")

      shiny::updateSelectizeInput(session, "filter_codes",
        choices = stats::setNames(codes$id, codes$name), server = TRUE)
      shiny::updateSelectizeInput(session, "filter_sources",
        choices = stats::setNames(docs$id, docs$name), server = TRUE)
      shiny::updateSelectizeInput(session, "filter_cases",
        choices = stats::setNames(cases$id, cases$name), server = TRUE)
      shiny::updateSelectizeInput(session, "filter_cats",
        choices = stats::setNames(cats$id, cats$name), server = TRUE)
    })

    results <- shiny::eventReactive(input$btn_run, {
      code_ids     <- if (length(input$filter_codes)   > 0) as.integer(input$filter_codes)   else NULL
      source_ids   <- if (length(input$filter_sources) > 0) as.integer(input$filter_sources) else NULL
      case_ids     <- if (length(input$filter_cases)   > 0) as.integer(input$filter_cases)   else NULL
      category_ids <- if (length(input$filter_cats)    > 0) as.integer(input$filter_cats)    else NULL
      qc_get_coded_segments(rv$project,
                            code_ids     = code_ids,
                            source_ids   = source_ids,
                            case_ids     = case_ids,
                            category_ids = category_ids)
    }, ignoreNULL = FALSE)

    output$summary_text <- shiny::renderUI({
      d <- results()
      shiny::tagList(
        shiny::strong(paste0(nrow(d), " segments")),
        shiny::br(),
        shiny::span(paste0(dplyr::n_distinct(d$source_id), " documents"),
                    style = "color:#6c757d; font-size:0.85rem;")
      )
    })

    output$tbl_results <- DT::renderDataTable({
      d <- results()
      DT::datatable(
        dplyr::select(d, source_name, code_name, category_names,
                      seltext, memo, selfirst, selast),
        rownames = FALSE,
        options  = list(pageLength = 20, dom = "ftp",
                        scrollX = TRUE),
        colnames = c("Document", "Code", "Categories",
                     "Passage", "Memo", "Start", "End")
      )
    })

    output$btn_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("qualcoder_export_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        utils::write.csv(results(), file, row.names = FALSE)
      }
    )
  })
}
