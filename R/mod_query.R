mod_query_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,

      shiny::h5("Filters"),
      shiny::selectizeInput(ns("filter_codes"), "Codes (OR — any of these)",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "All codes")),
      shiny::selectizeInput(ns("must_have"), "Must also have (AND)",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "No AND constraint")),
      shiny::selectizeInput(ns("must_not"), "Exclude codes (NOT)",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "No exclusion")),
      shiny::selectizeInput(ns("filter_sources"), "Documents",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "All documents")),
      shiny::selectizeInput(ns("filter_cases"), "Cases",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "All cases")),
      shiny::selectizeInput(ns("filter_cats"), "Categories",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "All categories")),
      shiny::selectInput(ns("filter_coder"), "Coder",
        choices = c("All" = ""), selected = ""),
      shiny::hr(),
      shiny::actionButton(ns("btn_run"), "Run Query",
                          class = "btn-primary w-100"),
      shiny::br(), shiny::br(),
      shiny::downloadButton(ns("btn_csv"), "Export CSV",
                            class = "btn-outline-secondary w-100"),
      shiny::br(), shiny::br(),
      shiny::uiOutput(ns("summary_text"))
    ),

    bslib::navset_card_underline(
      id = ns("result_tabs"),

      # ── Coded segments ───────────────────────────────────────────────────
      bslib::nav_panel("Segments",
        DT::dataTableOutput(ns("tbl_results"))
      ),

      # ── Full-text search ─────────────────────────────────────────────────
      bslib::nav_panel("Search",
        shiny::div(
          class = "p-3",
          shiny::div(
            class = "d-flex gap-2 mb-3",
            shiny::div(
              class = "flex-grow-1",
              shiny::textInput(ns("search_pattern"), "Search pattern",
                               placeholder = "Text or regex")
            ),
            shiny::checkboxInput(ns("search_regex"),
                                 "Regex", value = FALSE),
            shiny::checkboxInput(ns("search_icase"),
                                 "Ignore case", value = TRUE)
          ),
          shiny::actionButton(ns("btn_search"), "Search",
                              class = "btn-primary"),
          shiny::br(), shiny::br(),
          DT::dataTableOutput(ns("tbl_search"))
        )
      ),

      # ── Co-occurrence ────────────────────────────────────────────────────
      bslib::nav_panel("Co-occurrence",
        shiny::div(
          class = "p-3",
          shiny::div(
            class = "d-flex gap-2 align-items-end mb-3",
            shiny::selectInput(ns("cooc_unit"), "Unit",
                               choices = c("Document" = "document",
                                           "Segment (overlap)" = "segment"),
                               width = "200px"),
            shiny::actionButton(ns("btn_cooc"), "Compute",
                                class = "btn-primary")
          ),
          DT::dataTableOutput(ns("tbl_cooc"))
        )
      ),

      # ── Cross-tabulation ─────────────────────────────────────────────────
      bslib::nav_panel("Cross-tab",
        shiny::div(
          class = "p-3",
          shiny::div(
            class = "d-flex gap-2 align-items-end mb-3",
            shiny::div(
              class = "flex-grow-1",
              shiny::textInput(ns("xtab_attr"), "Case attribute variable",
                               placeholder = "e.g. industry")
            ),
            shiny::actionButton(ns("btn_xtab"), "Compute",
                                class = "btn-primary")
          ),
          shiny::p(shiny::tags$small(
            "Cross-tabulates code frequency (document count) by case attribute.",
            class = "text-muted")),
          DT::dataTableOutput(ns("tbl_xtab"))
        )
      )
    )
  )
}

mod_query_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      rv$refresh_codes
      rv$refresh_docs
      codes  <- qc_list_codes(rv$project)
      docs   <- qc_list_documents(rv$project)
      cases  <- qc_list_cases(rv$project)
      cats   <- .query(rv$project$con,
        "SELECT id, name FROM code_categories WHERE status = 1 ORDER BY name")
      coders <- qc_list_coders(rv$project)$coder

      code_choices <- stats::setNames(codes$id, codes$name)
      shiny::updateSelectizeInput(session, "filter_codes",
        choices = code_choices, server = TRUE)
      shiny::updateSelectizeInput(session, "must_have",
        choices = code_choices, server = TRUE)
      shiny::updateSelectizeInput(session, "must_not",
        choices = code_choices, server = TRUE)
      shiny::updateSelectizeInput(session, "filter_sources",
        choices = stats::setNames(docs$id,  docs$name), server = TRUE)
      shiny::updateSelectizeInput(session, "filter_cases",
        choices = stats::setNames(cases$id, cases$name), server = TRUE)
      shiny::updateSelectizeInput(session, "filter_cats",
        choices = stats::setNames(cats$id,  cats$name), server = TRUE)
      shiny::updateSelectInput(session, "filter_coder",
        choices = c("All" = "", stats::setNames(coders, coders)))
    })

    int_or_null <- function(x) if (length(x) > 0L) as.integer(x) else NULL
    str_or_null <- function(x) if (nchar(x) > 0L) x else NULL

    results <- shiny::eventReactive(input$btn_run, {
      qc_get_coded_segments(
        rv$project,
        code_ids   = int_or_null(input$filter_codes),
        must_have  = int_or_null(input$must_have),
        must_not   = int_or_null(input$must_not),
        source_ids = int_or_null(input$filter_sources),
        case_ids   = int_or_null(input$filter_cases),
        category_ids = int_or_null(input$filter_cats),
        coder      = str_or_null(input$filter_coder)
      )
    }, ignoreNULL = FALSE)

    output$summary_text <- shiny::renderUI({
      d <- results()
      shiny::tagList(
        shiny::strong(paste0(nrow(d), " segments")),
        shiny::br(),
        shiny::span(
          paste0(dplyr::n_distinct(d$source_id), " documents"),
          style = "color:#6c757d; font-size:0.85rem;"
        )
      )
    })

    output$tbl_results <- DT::renderDataTable({
      DT::datatable(
        dplyr::select(results(),
                      source_name, code_name, category_names,
                      seltext, memo, coder, coding_source,
                      coding_status, selfirst, selast),
        rownames = FALSE,
        options  = list(pageLength = 20, dom = "ftp", scrollX = TRUE),
        colnames = c("Document", "Code", "Categories", "Passage", "Memo",
                     "Coder", "Source", "Status", "Start", "End")
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

    # ── Search ─────────────────────────────────────────────────────────────

    search_results <- shiny::eventReactive(input$btn_search, {
      shiny::req(nchar(trimws(input$search_pattern)) > 0)
      qc_search_documents(
        rv$project,
        pattern    = input$search_pattern,
        regex      = isTRUE(input$search_regex),
        ignore_case = isTRUE(input$search_icase),
        source_ids = int_or_null(input$filter_sources)
      )
    })

    output$tbl_search <- DT::renderDataTable({
      DT::datatable(
        search_results(),
        rownames = FALSE,
        options  = list(pageLength = 20, dom = "ftp", scrollX = TRUE),
        colnames = c("Doc ID", "Document", "Match #",
                     "Start", "End", "Match", "Context")
      )
    })

    # ── Co-occurrence ──────────────────────────────────────────────────────

    cooc_results <- shiny::eventReactive(input$btn_cooc, {
      qc_code_cooccurrence(rv$project, unit = input$cooc_unit)
    })

    output$tbl_cooc <- DT::renderDataTable({
      DT::datatable(
        cooc_results(),
        rownames = FALSE,
        options  = list(pageLength = 25, dom = "ftp"),
        colnames = c("Code 1 ID", "Code 1", "Code 2 ID", "Code 2", "Count")
      )
    })

    # ── Cross-tabulation ───────────────────────────────────────────────────

    xtab_results <- shiny::eventReactive(input$btn_xtab, {
      shiny::req(nchar(trimws(input$xtab_attr)) > 0)
      tryCatch(
        qc_cross_tabulate(rv$project,
                          attribute = trimws(input$xtab_attr),
                          code_ids  = int_or_null(input$filter_codes)),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
    })

    output$tbl_xtab <- DT::renderDataTable({
      shiny::req(xtab_results())
      DT::datatable(
        xtab_results(),
        rownames = FALSE,
        options  = list(pageLength = 25, dom = "ftp", scrollX = TRUE)
      )
    })
  })
}
