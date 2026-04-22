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
                          class = "btn-primary w-100 mb-2"),
      shiny::downloadButton(ns("btn_csv"), "Export CSV",
                            class = "btn-outline-secondary w-100 mb-3"),
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

      # ── Saturation curve ─────────────────────────────────────────────────
      bslib::nav_panel("Saturation",
        shiny::div(
          class = "p-3",
          shiny::p(shiny::tags$small(
            "Plots cumulative distinct codes per document. ",
            "A flattening curve indicates theoretical saturation.",
            class = "text-muted")),
          shiny::div(
            class = "d-flex gap-3 align-items-end mb-3",
            shiny::div(
              shiny::tags$label("Order documents by", class = "form-label"),
              shiny::selectInput(ns("sat_order"), NULL,
                choices = c("Import date"  = "import_order",
                            "First coding" = "first_coded"),
                width = "180px")
            ),
            shiny::actionButton(ns("btn_saturation"), "Compute",
                                class = "btn-primary")
          ),
          shiny::plotOutput(ns("plt_saturation"), height = "320px"),
          shiny::br(),
          DT::dataTableOutput(ns("tbl_saturation"))
        )
      ),

      # ── Triangulation ─────────────────────────────────────────────────────
      bslib::nav_panel("Triangulation",
        shiny::div(
          class = "p-3",
          shiny::p(shiny::tags$small(
            "Compares code presence across source types ",
            "(set source type on documents in the Documents tab).",
            class = "text-muted")),
          shiny::div(
            class = "d-flex gap-3 align-items-end mb-3",
            shiny::div(
              shiny::tags$label("Count by", class = "form-label"),
              shiny::selectInput(ns("tri_metric"), NULL,
                choices = c("Segments" = "segments",
                            "Documents" = "documents"),
                width = "160px")
            ),
            shiny::actionButton(ns("btn_triangulate"), "Compute",
                                class = "btn-primary")
          ),
          shiny::uiOutput(ns("tri_summary")),
          DT::dataTableOutput(ns("tbl_triangulate"))
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
        class    = "table table-hover",
        rownames = FALSE,
        options  = list(
          pageLength = 20, dom = "ftp", scrollX = TRUE,
          columnDefs = list(
            list(targets = 3, className = "dt-truncate"),
            list(targets = 4, className = "dt-muted dt-truncate"),
            list(targets = c(8, 9), width = "60px", className = "text-center")
          )
        ),
        colnames = c("Document", "Code", "Categories", "Passage", "Memo",
                     "Coder", "Source", "Status", "Start", "End")
      )
    })

    output$btn_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("saturate_export_", format(Sys.Date(), "%Y%m%d"), ".csv")
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
        class    = "table table-hover",
        rownames = FALSE,
        options  = list(
          pageLength = 20, dom = "ftp", scrollX = TRUE,
          columnDefs = list(
            list(targets = 0, visible = FALSE),
            list(targets = 5, className = "dt-truncate"),
            list(targets = 6, className = "dt-muted")
          )
        ),
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
        class    = "table table-hover",
        rownames = FALSE,
        options  = list(
          pageLength = 25, dom = "ftp",
          columnDefs = list(
            list(targets = c(0, 2), visible = FALSE)
          )
        ),
        colnames = c("Code 1 ID", "Code 1", "Code 2 ID", "Code 2", "Count")
      )
    })

    # ── Saturation curve ───────────────────────────────────────────────────

    sat_rv <- shiny::eventReactive(input$btn_saturation, {
      tryCatch(
        qc_saturation_curve(rv$project, order_by = input$sat_order %||% "import_order",
                            code_ids = int_or_null(input$filter_codes)),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
    }, ignoreNULL = FALSE)

    output$plt_saturation <- shiny::renderPlot({
      df <- sat_rv()
      shiny::req(!is.null(df) && nrow(df) > 0)
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Install ggplot2 to see this chart",
                       cex = 1.2, col = "grey40")
        return(invisible(NULL))
      }
      qc_plot_saturation(rv$project,
                         order_by = input$sat_order %||% "import_order",
                         code_ids = int_or_null(input$filter_codes))
    })

    output$tbl_saturation <- DT::renderDataTable({
      df <- sat_rv()
      shiny::req(!is.null(df))
      if (nrow(df) == 0L) {
        return(DT::datatable(
          tibble::tibble(message = "No coded documents found."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      DT::datatable(df,
        class    = "table table-hover table-sm",
        rownames = FALSE,
        colnames = c("#", "Document", "Source type",
                     "Codings", "New codes", "Cumulative"),
        options  = list(pageLength = 25, dom = "ftp",
                        columnDefs = list(
                          list(targets = 0, width = "40px"),
                          list(targets = c(3, 4, 5), width = "90px",
                               className = "text-center")
                        ))
      )
    })

    # ── Triangulation ──────────────────────────────────────────────────────

    tri_rv <- shiny::eventReactive(input$btn_triangulate, {
      tryCatch(
        qc_triangulate(rv$project,
                       code_ids     = int_or_null(input$filter_codes),
                       category_ids = int_or_null(input$filter_cats),
                       metric       = input$tri_metric %||% "segments"),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
    }, ignoreNULL = FALSE)

    output$tri_summary <- shiny::renderUI({
      df <- tri_rv()
      if (is.null(df) || nrow(df) == 0L) return(NULL)
      type_cols <- setdiff(names(df), c("code_name", "total"))
      shiny::div(
        class = "d-flex flex-wrap gap-2 mb-3",
        lapply(type_cols, function(tc) {
          n <- sum(df[[tc]])
          shiny::tags$span(
            class = "badge bg-secondary",
            paste0(tc, ": ", n)
          )
        })
      )
    })

    output$tbl_triangulate <- DT::renderDataTable({
      df <- tri_rv()
      shiny::req(!is.null(df))
      if (nrow(df) == 0L) {
        return(DT::datatable(
          tibble::tibble(
            message = paste0(
              "No data. Set source types on documents via Documents tab, ",
              "then click Compute.")),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      DT::datatable(df,
        class    = "table table-hover",
        rownames = FALSE,
        options  = list(pageLength = 25, dom = "ftp", scrollX = TRUE)
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
        class    = "table table-hover",
        rownames = FALSE,
        options  = list(pageLength = 25, dom = "ftp", scrollX = TRUE)
      )
    })
  })
}
