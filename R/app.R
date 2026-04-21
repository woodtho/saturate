# ── Top-level UI ──────────────────────────────────────────────────────────────

saturate_ui <- function() {
  bslib::page_navbar(
    title = shiny::span(
      shiny::tags$img(src = NULL, height = 0),
      "saturate"
    ),
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    shinyjs::useShinyjs(),

    bslib::nav_panel("Documents", mod_documents_ui("docs")),
    bslib::nav_panel("Coding",    mod_coding_ui("coding")),
    bslib::nav_panel("Codebook",  mod_codebook_ui("codebook")),
    bslib::nav_panel("Query",     mod_query_ui("query")),
    bslib::nav_panel("Graph",     mod_graph_ui("graph")),

    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::actionButton("btn_project_info", "Project",
                          class = "btn-sm btn-outline-light")
    )
  )
}

# ── Top-level server ──────────────────────────────────────────────────────────

saturate_server <- function(input, output, session, project) {
  rv <- shiny::reactiveValues(
    project          = project,
    refresh_docs     = 0L,
    refresh_codes    = 0L,
    active_source_id = NULL
  )

  mod_documents_server("docs",     rv)
  mod_codebook_server("codebook",  rv)
  mod_coding_server("coding",      rv, session)
  mod_query_server("query",        rv)
  mod_graph_server("graph",        rv)

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
