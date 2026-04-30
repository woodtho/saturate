# ── Export module ─────────────────────────────────────────────────────────────

mod_export_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 270,
      shiny::tags$nav(
        `aria-label` = "Export type",
        shiny::h5("Export type"),
        shiny::radioButtons(ns("export_type"), NULL,
          choices = c(
            "Analytical Report"  = "report",
            "Codebook"           = "codebook",
            "Raw Project Data"   = "table"
          ),
          selected = "report"
        ),
        qc_help_details(
          "Export help",
          shiny::p(
            "Choose the export that matches the task: narrative report for ",
            "analysis, codebook for methods appendices, or raw data for audit ",
            "and reuse outside the app."
          )
        )
      ),
      shiny::hr(),
      shiny::uiOutput(ns("sidebar_opts"))
    ),

    shiny::div(
      id    = ns("export_main"),
      role  = "main",
      `aria-live` = "polite",
      shiny::uiOutput(ns("export_panel"))
    )
  )
}

mod_export_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Sidebar options vary by export type ───────────────────────────────────

    output$sidebar_opts <- shiny::renderUI({
      switch(input$export_type,

        report = shiny::tagList(
          shiny::selectizeInput(ns("rpt_themes"), "Themes",
            choices = NULL, multiple = TRUE,
            options = list(placeholder = "All themes")),
          shiny::checkboxInput(ns("rpt_excerpts"),  "Include excerpts",  TRUE),
          shiny::checkboxInput(ns("rpt_narrative"), "Include narrative", TRUE)
        ),

        codebook = shiny::tagList(
          shiny::checkboxInput(ns("cb_definitions"), "Definitions",      TRUE),
          shiny::checkboxInput(ns("cb_criteria"),    "Criteria",         TRUE),
          shiny::checkboxInput(ns("cb_memo"),        "Memos",            FALSE),
          shiny::checkboxInput(ns("cb_examples"),    "Example excerpts", FALSE),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("cb_examples")),
            shiny::numericInput(ns("cb_n_ex"), "Max examples per code",
                                value = 2L, min = 1L, max = 10L)
          )
        ),

        table = shiny::tagList(
          shiny::selectInput(ns("tbl_name"), "Table",
            choices = c(
              "Documents"        = "documents",
              "Codes"            = "codes",
              "Codings"          = "codings",
              "Categories"       = "categories",
              "Category–Code links" = "category_links",
              "Themes"           = "themes",
              "Cases"            = "cases",
              "Case attributes"  = "case_attributes",
              "Annotations"      = "annotations",
              "Project memos"    = "memos",
              "Coding audit"     = "coding_audit",
              "Code history"     = "code_history"
            )
          ),
          shiny::uiOutput(ns("tbl_row_count"))
        )
      )
    })

    # Populate theme selectize once project is ready
    shiny::observe({
      rv$refresh_codes
      themes <- tryCatch(qc_list_themes(rv$project),
                          error = function(e) NULL)
      if (is.null(themes) || nrow(themes) == 0L) {
        shiny::updateSelectizeInput(session, "rpt_themes",
          choices = character(0), server = TRUE)
      } else {
        shiny::updateSelectizeInput(session, "rpt_themes",
          choices = stats::setNames(themes$id, themes$name),
          server  = TRUE)
      }
    })

    # Row count for raw table
    output$tbl_row_count <- shiny::renderUI({
      shiny::req(input$tbl_name)
      # Quick count — approximate using status=1 where applicable
      n <- tryCatch({
        tbl_sql <- list(
          documents     = "SELECT COUNT(*) FROM sources WHERE status=1",
          codes         = "SELECT COUNT(*) FROM codes WHERE status=1",
          codings       = "SELECT COUNT(*) FROM codings cod JOIN sources s ON s.id=cod.source_id JOIN codes c ON c.id=cod.code_id WHERE cod.status=1 AND s.status=1 AND c.status=1",
          categories    = "SELECT COUNT(*) FROM code_categories WHERE status=1",
          category_links= "SELECT COUNT(*) FROM code_category_links WHERE status=1",
          themes        = "SELECT COUNT(*) FROM themes WHERE status=1",
          cases         = "SELECT COUNT(*) FROM cases WHERE status=1",
          case_attributes="SELECT COUNT(*) FROM case_attributes WHERE status=1",
          annotations   = "SELECT COUNT(*) FROM annotations WHERE status=1",
          memos         = "SELECT COUNT(*) FROM project_memos WHERE status=1",
          coding_audit  = "SELECT COUNT(*) FROM coding_audit",
          code_history  = "SELECT COUNT(*) FROM code_history"
        )
        sql <- tbl_sql[[input$tbl_name]]
        if (!is.null(sql)) .query(rv$project$con, sql)[[1]] else NA
      }, error = function(e) NA)

      if (!is.na(n))
        shiny::tags$small(class = "text-muted",
          paste0(format(n, big.mark = ","), " rows"))
    })

    # ── Main panel ────────────────────────────────────────────────────────────

    output$export_panel <- shiny::renderUI({
      switch(input$export_type,
        report   = .export_report_panel(ns),
        codebook = .export_codebook_panel(ns),
        table    = .export_table_panel(ns)
      )
    })

    # ── Summary stats reactive ────────────────────────────────────────────────

    output$rpt_stats <- shiny::renderUI({
      themes   <- tryCatch(qc_list_themes(rv$project),  error = function(e) NULL)
      n_themes <- if (!is.null(themes)) nrow(themes) else 0L

      sel_ids <- input$rpt_themes
      if (length(sel_ids) > 0L)
        n_themes <- length(sel_ids)

      n_codes <- tryCatch(
        nrow(qc_list_codes(rv$project)), error = function(e) 0L)
      n_codings <- tryCatch(
        .query(rv$project$con,
          "SELECT COUNT(*) FROM codings WHERE status=1")[[1]],
        error = function(e) 0L)

      shiny::div(
        class = "d-flex gap-3 flex-wrap mb-3",
        .stat_pill(n_themes,  "themes"),
        .stat_pill(n_codes,   "codes"),
        .stat_pill(n_codings, "excerpts")
      )
    })

    output$cb_stats <- shiny::renderUI({
      n_codes <- tryCatch(nrow(qc_list_codes(rv$project)), error = function(e) 0L)
      n_cats  <- tryCatch(
        .query(rv$project$con,
          "SELECT COUNT(*) FROM code_categories WHERE status=1")[[1]],
        error = function(e) 0L)

      shiny::div(
        class = "d-flex gap-3 flex-wrap mb-3",
        .stat_pill(n_codes, "codes"),
        .stat_pill(n_cats,  "categories")
      )
    })

    # ── Download handlers — Analytical Report ─────────────────────────────────

    .rpt_content <- function(fmt) {
      function(file) {
        ids <- if (length(input$rpt_themes) > 0L) as.integer(input$rpt_themes) else NULL
        tryCatch({
          tmp <- qc_export_themes_report(
            rv$project,
            format           = fmt,
            theme_ids        = ids,
            include_excerpts  = isTRUE(input$rpt_excerpts),
            include_narrative = isTRUE(input$rpt_narrative)
          )
          file.copy(tmp, file)
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      }
    }

    output$dl_rpt_docx <- shiny::downloadHandler(
      filename = function() paste0("themes_report_", Sys.Date(), ".docx"),
      content  = .rpt_content("docx")
    )
    output$dl_rpt_html <- shiny::downloadHandler(
      filename = function() paste0("themes_report_", Sys.Date(), ".html"),
      content  = .rpt_content("html")
    )
    output$dl_rpt_txt <- shiny::downloadHandler(
      filename = function() paste0("themes_report_", Sys.Date(), ".txt"),
      content  = .rpt_content("txt")
    )
    output$dl_rpt_json <- shiny::downloadHandler(
      filename = function() paste0("themes_report_", Sys.Date(), ".json"),
      content  = .rpt_content("json")
    )

    # ── Download handlers — Codebook ──────────────────────────────────────────

    .cb_content <- function(fmt) {
      function(file) {
        tryCatch({
          tmp <- qc_export_codebook_full(
            rv$project,
            format              = fmt,
            include_definitions = isTRUE(input$cb_definitions),
            include_criteria    = isTRUE(input$cb_criteria),
            include_memo        = isTRUE(input$cb_memo),
            include_examples    = isTRUE(input$cb_examples),
            n_examples          = as.integer(input$cb_n_ex %||% 2L)
          )
          file.copy(tmp, file)
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      }
    }

    output$dl_cb_docx <- shiny::downloadHandler(
      filename = function() paste0("codebook_", Sys.Date(), ".docx"),
      content  = .cb_content("docx")
    )
    output$dl_cb_xlsx <- shiny::downloadHandler(
      filename = function() paste0("codebook_", Sys.Date(), ".xlsx"),
      content  = .cb_content("xlsx")
    )
    output$dl_cb_csv <- shiny::downloadHandler(
      filename = function() paste0("codebook_", Sys.Date(), ".csv"),
      content  = .cb_content("csv")
    )
    output$dl_cb_json <- shiny::downloadHandler(
      filename = function() paste0("codebook_", Sys.Date(), ".json"),
      content  = .cb_content("json")
    )
    output$dl_cb_html <- shiny::downloadHandler(
      filename = function() paste0("codebook_", Sys.Date(), ".html"),
      content  = .cb_content("html")
    )

    # ── Download handlers — Raw Table ─────────────────────────────────────────

    .tbl_content <- function(fmt) {
      function(file) {
        shiny::req(input$tbl_name)
        tryCatch({
          tmp <- qc_export_project_data(rv$project,
                                         table_name = input$tbl_name,
                                         format     = fmt)
          file.copy(tmp, file)
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      }
    }

    output$dl_tbl_csv <- shiny::downloadHandler(
      filename = function() paste0(input$tbl_name %||% "export", "_", Sys.Date(), ".csv"),
      content  = .tbl_content("csv")
    )
    output$dl_tbl_json <- shiny::downloadHandler(
      filename = function() paste0(input$tbl_name %||% "export", "_", Sys.Date(), ".json"),
      content  = .tbl_content("json")
    )
    output$dl_tbl_xlsx <- shiny::downloadHandler(
      filename = function() paste0(input$tbl_name %||% "export", "_", Sys.Date(), ".xlsx"),
      content  = .tbl_content("xlsx")
    )
  })
}

# ── Panel UI helpers ──────────────────────────────────────────────────────────

.stat_pill <- function(n, label) {
  shiny::tags$div(
    class = "text-center px-3 py-2 rounded",
    style = paste0(
      "background:var(--sat-surface-card);",
      "border:1px solid var(--sat-border);min-width:80px"
    ),
    shiny::tags$div(
      style = "font-size:1.4rem;font-weight:700;line-height:1;color:var(--sat-primary)",
      format(n, big.mark = ",")
    ),
    shiny::tags$div(
      style = "font-size:.72rem;text-transform:uppercase;letter-spacing:.06em;color:var(--sat-text-muted)",
      label
    )
  )
}

.fmt_btn <- function(dl_id, label, icon_class = NULL, btn_class = "btn-outline-secondary") {
  shiny::tags$div(
    class = "d-grid",
    shiny::downloadButton(
      dl_id,
      label    = if (is.null(icon_class)) label else
                   shiny::tagList(shiny::tags$i(class = icon_class,
                     `aria-hidden` = "true"), " ", label),
      class    = paste("btn", btn_class),
      style    = "width:100%;text-align:left"
    )
  )
}

.export_report_panel <- function(ns) {
  bslib::layout_columns(
    col_widths = c(12),
    bslib::card(
      bslib::card_header(
        shiny::tags$span(
          shiny::tags$i(class = "fa fa-file-alt", `aria-hidden` = "true"), " Analytical Report"
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("rpt_stats")),
        qc_help_note(
          "Exports each theme with its proposition, narrative, linked codes,",
          " and supporting excerpts. Choose a format below."
        ),
        shiny::tags$fieldset(
          shiny::tags$legend(
            class = "h6",
            "Download as"
          ),
          shiny::div(
            class = "d-flex flex-wrap gap-2",
            shiny::div(
              class = "d-grid",
              shiny::downloadButton(ns("dl_rpt_docx"),
                shiny::tagList(shiny::tags$i(class = "fa fa-file-word-o", `aria-hidden` = "true"),
                               " Word"),
                class = "btn btn-primary"
              )
            ),
            shiny::div(
              class = "d-grid",
              shiny::downloadButton(ns("dl_rpt_html"),
                shiny::tagList(shiny::tags$i(class = "fa fa-globe", `aria-hidden` = "true"),
                               " HTML"),
                class = "btn btn-outline-secondary"
              )
            ),
            shiny::div(
              class = "d-grid",
              shiny::downloadButton(ns("dl_rpt_txt"),
                shiny::tagList(shiny::tags$i(class = "fa fa-file-text-o", `aria-hidden` = "true"),
                               " Text"),
                class = "btn btn-outline-secondary"
              )
            ),
            shiny::div(
              class = "d-grid",
              shiny::downloadButton(ns("dl_rpt_json"),
                shiny::tagList(shiny::tags$i(class = "fa fa-code", `aria-hidden` = "true"),
                               " JSON"),
                class = "btn btn-outline-secondary"
              )
            )
          )
        )
      )
    )
  )
}

.export_codebook_panel <- function(ns) {
  bslib::card(
    bslib::card_header(
      shiny::tags$span(
        shiny::tags$i(class = "fa fa-book", `aria-hidden` = "true"), " Codebook"
      )
    ),
    bslib::card_body(
      shiny::uiOutput(ns("cb_stats")),
      qc_help_note(
        "Exports all codes with definitions, criteria, and optional examples.",
        " Excel export includes a separate categories sheet."
      ),
      shiny::tags$fieldset(
        shiny::tags$legend(class = "h6", "Download as"),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          shiny::div(class = "d-grid",
            shiny::downloadButton(ns("dl_cb_docx"),
              shiny::tagList(shiny::tags$i(class = "fa fa-file-word-o",
                `aria-hidden` = "true"), " Word"),
              class = "btn btn-primary")),
          shiny::div(class = "d-grid",
            shiny::downloadButton(ns("dl_cb_xlsx"),
              shiny::tagList(shiny::tags$i(class = "fa fa-file-excel-o",
                `aria-hidden` = "true"), " Excel"),
              class = "btn btn-outline-secondary")),
          shiny::div(class = "d-grid",
            shiny::downloadButton(ns("dl_cb_csv"),
              shiny::tagList(shiny::tags$i(class = "fa fa-table",
                `aria-hidden` = "true"), " CSV"),
              class = "btn btn-outline-secondary")),
          shiny::div(class = "d-grid",
            shiny::downloadButton(ns("dl_cb_json"),
              shiny::tagList(shiny::tags$i(class = "fa fa-code",
                `aria-hidden` = "true"), " JSON"),
              class = "btn btn-outline-secondary")),
          shiny::div(class = "d-grid",
            shiny::downloadButton(ns("dl_cb_html"),
              shiny::tagList(shiny::tags$i(class = "fa fa-globe",
                `aria-hidden` = "true"), " HTML"),
              class = "btn btn-outline-secondary"))
        )
      )
    )
  )
}

.export_table_panel <- function(ns) {
  bslib::card(
    bslib::card_header(
      shiny::tags$span(
        shiny::tags$i(class = "fa fa-database", `aria-hidden` = "true"),
        " Raw Project Data"
      )
    ),
    bslib::card_body(
      qc_help_note(
        "Export the selected database table as a flat file.",
        " Foreign keys are resolved to human-readable names where possible."
      ),
      shiny::tags$fieldset(
        shiny::tags$legend(class = "h6", "Download as"),
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          shiny::div(class = "d-grid",
            shiny::downloadButton(ns("dl_tbl_csv"),
              shiny::tagList(shiny::tags$i(class = "fa fa-table",
                `aria-hidden` = "true"), " CSV"),
              class = "btn btn-primary")),
          shiny::div(class = "d-grid",
            shiny::downloadButton(ns("dl_tbl_xlsx"),
              shiny::tagList(shiny::tags$i(class = "fa fa-file-excel-o",
                `aria-hidden` = "true"), " Excel"),
              class = "btn btn-outline-secondary")),
          shiny::div(class = "d-grid",
            shiny::downloadButton(ns("dl_tbl_json"),
              shiny::tagList(shiny::tags$i(class = "fa fa-code",
                `aria-hidden` = "true"), " JSON"),
              class = "btn btn-outline-secondary"))
        )
      )
    )
  )
}
