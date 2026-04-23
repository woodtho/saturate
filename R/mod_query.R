mod_query_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,

      shiny::h5("Filters"),
      qc_help_details(
        "Query filter help",
        shiny::p(
          "Filters apply to the Segments table and to analysis tools that use ",
          "the current query context."
        ),
        qc_help_list(c(
          "Codes is an OR filter: passages can match any selected code.",
          "Must also have is an AND filter: passages need those additional codes.",
          "Exclude codes removes passages containing selected codes."
        ))
      ),
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
          qc_help_note(
            "Search scans document text, not only coded passages. Use Regex ",
            "for patterns such as word boundaries or alternatives."
          ),
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
          qc_help_note(
            "Co-occurrence counts codes that appear in the same document or ",
            "overlapping segment, depending on the selected unit."
          ),
          shiny::div(
            class = "d-flex gap-2 align-items-end mb-3",
            shiny::selectInput(ns("cooc_unit"), "Unit",
                               choices = c("Document" = "document",
                                           "Segment (overlap)" = "segment"),
                               width = "200px"),
            shiny::actionButton(ns("btn_cooc"), "Compute",
                                class = "btn-primary"),
            shiny::downloadButton(ns("dl_cooc"), "Download PNG",
                                  class = "btn-outline-secondary")
          ),
          shiny::plotOutput(ns("plt_cooc"), height = "380px"),
          shiny::br(),
          DT::dataTableOutput(ns("tbl_cooc"))
        )
      ),

      # ── Saturation curve ─────────────────────────────────────────────────
      bslib::nav_panel("Saturation",
        shiny::div(
          class = "p-3",
          qc_help_note(
            "Saturation plots cumulative distinct codes per document. A ",
            "flattening curve suggests fewer new concepts are appearing."
          ),
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
                                class = "btn-primary"),
            shiny::downloadButton(ns("dl_saturation"), "Download PNG",
                                  class = "btn-outline-secondary")
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
          qc_help_note(
            "Triangulation compares code presence across source types. Set ",
            "source type during document import or edit it from Documents."
          ),
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
          qc_help_note(
            "Cross-tabs compare code frequency by a case attribute. Add case ",
            "attributes before using this view."
          ),
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
          DT::dataTableOutput(ns("tbl_xtab"))
        )
      ),

      # ── Word Cloud ───────────────────────────────────────────────────────
      bslib::nav_panel("Word Cloud",
        shiny::div(
          class = "p-3",
          qc_help_note(
            "Generate a quick visual summary from code names or from words in ",
            "passages tied to one code."
          ),
          shiny::div(
            class = "d-flex gap-3 align-items-end mb-3 flex-wrap",
            shiny::div(
              shiny::tags$label("Cloud type", class = "form-label"),
              shiny::selectInput(ns("wc_type"), NULL,
                choices = c(
                  "All codes — sized by coding count" = "codes",
                  "Single code — words in excerpts"   = "excerpt_words"
                ),
                width = "280px")
            ),
            shiny::uiOutput(ns("wc_code_picker")),
            shiny::actionButton(ns("btn_wordcloud"), "Generate",
              class = "btn-primary"),
            shiny::downloadButton(ns("dl_wordcloud"), "Download PNG",
              class = "btn-outline-secondary")
          ),
          shiny::uiOutput(ns("wc_output"))
        )
      )
    )
  )
}

mod_query_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    output$plt_cooc <- shiny::renderPlot({
      shiny::req(nrow(cooc_results()) > 0)
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Install ggplot2 to see this chart",
                       cex = 1.2, col = "grey40")
        return(invisible(NULL))
      }
      qc_plot_cooccurrence(rv$project, unit = input$cooc_unit %||% "document")
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

    output$dl_cooc <- shiny::downloadHandler(
      filename = function() paste0("cooccurrence_", format(Sys.Date(), "%Y%m%d"), ".png"),
      content  = function(file) {
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
          shiny::showNotification("Install ggplot2 to export this chart.", type = "error")
          return()
        }
        p <- tryCatch(
          qc_plot_cooccurrence(rv$project, unit = input$cooc_unit %||% "document"),
          error = function(e) NULL
        )
        if (is.null(p)) {
          shiny::showNotification("No co-occurrence data to export.", type = "warning")
          return()
        }
        ggplot2::ggsave(file, plot = p, width = 8, height = 6, dpi = 150)
      }
    )

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

    # ── Word Cloud ──────────────────────────────────────────────────────────

    output$wc_code_picker <- shiny::renderUI({
      if (input$wc_type != "excerpt_words") return(NULL)
      codes <- qc_list_codes(rv$project)
      shiny::div(
        shiny::tags$label("Code", class = "form-label"),
        shiny::selectInput(ns("wc_code_id"), NULL,
          choices = stats::setNames(codes$id, codes$name),
          width   = "220px")
      )
    })

    wc_data <- shiny::eventReactive(input$btn_wordcloud, {
      type <- input$wc_type %||% "codes"
      if (type == "codes") {
        codes <- qc_list_codes(rv$project)
        codes <- codes[codes$n_codings > 0L, ]
        if (nrow(codes) == 0L) return(NULL)
        list(
          type  = "codes",
          words = tibble::tibble(
            word = codes$name,
            freq = as.integer(codes$n_codings)
          )
        )
      } else {
        shiny::req(input$wc_code_id)
        code_id  <- as.integer(input$wc_code_id)
        segments <- qc_get_coded_segments(rv$project,
                                          code_ids = code_id)
        if (nrow(segments) == 0L) return(NULL)
        all_text <- paste(segments$seltext, collapse = " ")
        words    <- unlist(strsplit(
          tolower(gsub("[^a-zA-Z' ]", " ", all_text)), "\\s+"))
        words    <- words[nchar(words) > 3L]
        stop_words <- c(
          "that", "this", "with", "have", "from", "they", "been",
          "were", "their", "would", "could", "about", "when", "what",
          "your", "which", "some", "also", "than", "then", "into",
          "more", "will", "just", "there", "like", "very", "only"
        )
        words <- words[!words %in% stop_words]
        if (length(words) == 0L) return(NULL)
        freq_tbl <- sort(table(words), decreasing = TRUE)
        list(
          type  = "excerpt_words",
          words = tibble::tibble(
            word = names(freq_tbl),
            freq = as.integer(freq_tbl)
          )
        )
      }
    }, ignoreNULL = FALSE)

    output$wc_output <- shiny::renderUI({
      d <- wc_data()
      if (is.null(d)) {
        return(shiny::p(
          class = "text-muted",
          "No data. Run a query or select a code with codings, then click Generate."
        ))
      }

      if (!requireNamespace("wordcloud2", quietly = TRUE)) {
        # Fallback: horizontal bar chart via base graphics
        return(shiny::div(
          shiny::p(shiny::tags$small(
            class = "text-muted",
            "Install the wordcloud2 package for interactive clouds. ",
            "Showing bar chart instead."
          )),
          shiny::plotOutput(ns("wc_fallback_plot"), height = "400px")
        ))
      }

      top <- head(d$words[order(-d$words$freq), ], 150L)
      wordcloud2::wordcloud2Output(ns("wc_widget"), height = "480px")
    })

    if (requireNamespace("wordcloud2", quietly = TRUE)) {
      output$wc_widget <- wordcloud2::renderWordcloud2({
        d <- wc_data()
        shiny::req(!is.null(d))
        top <- head(d$words[order(-d$words$freq), ], 150L)
        wordcloud2::wordcloud2(top, size = 0.6, color = "random-dark")
      })
    }

    output$wc_fallback_plot <- shiny::renderPlot({
      d <- wc_data()
      shiny::req(!is.null(d))
      top <- head(d$words[order(-d$words$freq), ], 30L)
      top <- top[order(top$freq), ]
      op <- graphics::par(mar = c(4, 10, 2, 1))
      graphics::barplot(top$freq,
        names.arg = top$word,
        horiz     = TRUE,
        las       = 1,
        col       = "#4E79A7",
        border    = NA,
        xlab      = "Count"
      )
      graphics::par(op)
    })
  })
}
