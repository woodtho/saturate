# ── Top-level UI ──────────────────────────────────────────────────────────────

saturate_ui <- function() {
  default_coder <- Sys.info()[["user"]] %||% "default"
  css_file      <- system.file("app", "styles.css", package = "saturate")

  shiny::tagList(
    shiny::tags$head(shiny::includeCSS(css_file)),
    bslib::page_navbar(
      title  = shiny::span(
        shiny::tags$img(src = NULL, height = 0),
        "saturate"
      ),
      theme  = bslib::bs_theme(version = 5, bootswatch = "flatly"),

      bslib::nav_panel("Documents",
        shiny::tagList(shinyjs::useShinyjs(), mod_documents_ui("docs"))
      ),
      bslib::nav_panel("Coding",    mod_coding_ui("coding")),
      bslib::nav_panel("Compare",   mod_compare_ui("compare")),
      bslib::nav_panel("Codebook",  mod_codebook_ui("codebook")),
      bslib::nav_panel("Query",     mod_query_ui("query")),
      bslib::nav_panel("Graph",     mod_graph_ui("graph")),
      bslib::nav_panel("Members",   mod_member_check_ui("members")),
      bslib::nav_panel("Audit",     mod_audit_ui("audit")),

      bslib::nav_spacer(),
      bslib::nav_item(
        shiny::div(
          class = "qc-navbar-session d-flex align-items-center gap-2",

          # ── Active coder ─────────────────────────────────────────────────
          shiny::div(
            class = "qc-coder-block d-flex flex-column",
            style = "line-height:1;",
            shiny::tags$span(
              "CODING AS",
              style = paste0("font-size:0.62rem;font-weight:600;",
                             "letter-spacing:.07em;color:rgba(255,255,255,.5);",
                             "margin-bottom:2px;")
            ),
            shiny::div(
              class = "d-flex align-items-center gap-1",
              shiny::selectizeInput(
                "current_coder",
                label    = NULL,
                choices  = stats::setNames(default_coder, default_coder),
                selected = default_coder,
                width    = "155px",
                options  = list(
                  create           = TRUE,
                  persist          = TRUE,
                  maxItems         = 1,
                  closeAfterSelect = TRUE,
                  placeholder      = "Type or pick…"
                )
              )
            )
          ),

          # ── Blind mode toggle ─────────────────────────────────────────────
          shiny::uiOutput("ui_blind_btn", inline = TRUE),

          shiny::actionButton("btn_project_info", "Project",
                              class = "btn-sm btn-outline-light ms-1 qc-navbar-project")
        )
      )
    )
  )
}

# ── Top-level server ──────────────────────────────────────────────────────────

saturate_server <- function(input, output, session, project) {
  default_coder <- Sys.info()[["user"]] %||% "default"

  rv <- shiny::reactiveValues(
    project          = project,
    refresh_docs     = 0L,
    refresh_codes    = 0L,
    active_source_id = NULL,
    current_coder    = default_coder,
    blind_mode       = FALSE
  )

  shiny::observeEvent(input$current_coder, {
    coder <- trimws(as.character(input$current_coder %||% ""))
    if (nchar(coder) == 0L) return()
    rv$current_coder <- coder
  }, ignoreInit = TRUE)

  shiny::observe({
    rv$refresh_codes
    coders <- tryCatch(qc_list_coders(rv$project)$coder,
                       error = function(e) character())
    choices <- unique(c(rv$current_coder %||% default_coder, coders))
    choices <- choices[!is.na(choices) & nzchar(choices)]
    selected <- rv$current_coder %||% default_coder

    if (!selected %in% choices) choices <- c(selected, choices)

    shiny::updateSelectizeInput(
      session, "current_coder",
      choices  = stats::setNames(choices, choices),
      selected = selected,
      server   = FALSE
    )
  })

  # ── Blind mode toggle ──────────────────────────────────────────────────────

  output$ui_blind_btn <- shiny::renderUI({
    is_blind <- isTRUE(rv$blind_mode)
    shiny::actionButton(
      "btn_blind_mode",
      if (is_blind)
        shiny::tagList(shiny::icon("lock"),     " Blind ON")
      else
        shiny::tagList(shiny::icon("lock-open"), " Blind"),
      class = if (is_blind)
        "btn-sm btn-warning"
      else
        "btn-sm btn-outline-light",
      title = paste0(
        "Blind coding mode: hide other coders' work while coding. ",
        if (is_blind) "Currently ON — click to disable."
        else "Currently OFF — click to enable."
      )
    )
  })

  shiny::observeEvent(input$btn_blind_mode, {
    rv$blind_mode <- !isTRUE(rv$blind_mode)
  })

  mod_documents_server("docs",     rv)
  mod_codebook_server("codebook",  rv)
  mod_coding_server("coding",      rv, session)
  mod_compare_server("compare",    rv)
  mod_query_server("query",        rv)
  mod_graph_server("graph",        rv)
  mod_member_check_server("members", rv)
  mod_audit_server("audit",         rv)

  shiny::observeEvent(input$btn_project_info, {
    info <- qc_project_info(rv$project)
    shiny::showModal(shiny::modalDialog(
      title = "Project Info",
      shiny::tags$dl(
        shiny::tags$dt("Name"),  shiny::tags$dd(info$name),
        shiny::tags$dt("Owner"), shiny::tags$dd(info$owner),
        shiny::tags$dt("Path"),  shiny::tags$dd(rv$project$path),
        shiny::tags$dt("Memo"),  shiny::tags$dd(info$memo)
      ),
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ))
  })
}

#' Launch the saturate Shiny GUI
#'
#' Opens an interactive coding interface. Can be launched against an existing
#' project or will show an error if `project` is `NULL`.
#'
#' @param project A `qc_project` object created by [qc_new()] or [qc_open()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Called for its side effect; does not return normally.
#' @export
shiny_saturate <- function(project, ...) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  .env          <- new.env(parent = emptyenv())
  .env$project  <- project

  app <- shiny::shinyApp(
    ui     = saturate_ui(),
    server = function(input, output, session) {
      saturate_server(input, output, session, .env$project)
    }
  )
  shiny::runApp(app, ...)
}
