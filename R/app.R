# ── Top-level UI ──────────────────────────────────────────────────────────────

saturate_ui <- function(app_name = "saturate", brand_css = "") {
  default_coder <- Sys.info()[["user"]] %||% "default"
  css_file      <- system.file("app", "styles.css", package = "saturate")

  shiny::tagList(

    # ── Skip-to-main-content (keyboard / screen-reader accessibility) ──────
    shiny::tags$a(
      href  = "#sat-main-content",
      class = "skip-link",
      "Skip to main content"
    ),

    shiny::tags$head(
      shiny::includeCSS(css_file),
      # Brand CSS variable overrides — injected when brand != NULL
      if (nchar(brand_css) > 0L)
        shiny::tags$style(shiny::HTML(brand_css))
    ),

    bslib::page_navbar(
      title  = shiny::span(
        shiny::tags$img(src = NULL, height = 0, alt = ""),
        app_name
      ),
      theme  = bslib::bs_theme(version = 5, bootswatch = "flatly"),
      lang   = "en",

      bslib::nav_panel("Documents",
        shiny::div(
          id = "sat-main-content", tabindex = "-1",
          shiny::tagList(shinyjs::useShinyjs(), mod_documents_ui("docs"))
        )
      ),
      bslib::nav_panel("Coding",    mod_coding_ui("coding")),
      bslib::nav_panel("Compare",   mod_compare_ui("compare")),
      bslib::nav_panel("Codebook",  mod_codebook_ui("codebook")),
      bslib::nav_panel("Themes",    mod_themes_ui("themes")),
      bslib::nav_panel("Query",     mod_query_ui("query")),
      bslib::nav_panel("Graph",     mod_graph_ui("graph")),
      bslib::nav_panel("Members",   mod_member_check_ui("members")),
      bslib::nav_panel("Export",    mod_export_ui("export")),
      bslib::nav_panel("Audit",     mod_audit_ui("audit")),

      bslib::nav_spacer(),
      bslib::nav_item(
        shiny::div(
          class = "qc-navbar-session d-flex align-items-center gap-2",

          # ── Active coder ─────────────────────────────────────────────────
          shiny::div(
            class = "qc-coder-block",
            shiny::tags$label(
              "CODING AS",
              `for`  = "current_coder",
              class  = "qc-coder-label",
              id     = "lbl-current-coder"
            ),
            shiny::textInput(
              "current_coder",
              label       = NULL,
              value       = default_coder,
              placeholder = "Coder name…",
              width       = "160px"
            ),
            shiny::uiOutput("coder_suggestions_ui", inline = TRUE),
            shiny::tags$script(
              'setTimeout(function(){
                 var e = document.getElementById("current_coder");
                 if (e) {
                   e.setAttribute("list", "coder_sugg");
                   e.setAttribute("aria-label", "Current coder name");
                   e.setAttribute("autocomplete", "off");
                 }
               }, 250);'
            )
          ),

          # ── Blind mode toggle ─────────────────────────────────────────────
          shiny::uiOutput("ui_blind_btn", inline = TRUE),

          shiny::actionButton("btn_project_info", "Project",
            class = "btn-sm btn-outline-light ms-1 qc-navbar-project",
            `aria-label` = "View project information"
          )
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

  output$coder_suggestions_ui <- shiny::renderUI({
    rv$refresh_codes
    coders     <- tryCatch(qc_list_coders(rv$project)$coder,
                           error = function(e) character())
    all_coders <- unique(c(rv$current_coder %||% default_coder, coders))
    all_coders <- all_coders[!is.na(all_coders) & nzchar(all_coders)]
    shiny::tags$datalist(
      id = "coder_sugg",
      lapply(all_coders, function(x) shiny::tags$option(value = x))
    )
  })

  # ── Blind mode toggle ──────────────────────────────────────────────────────

  output$ui_blind_btn <- shiny::renderUI({
    is_blind <- isTRUE(rv$blind_mode)
    shiny::actionButton(
      "btn_blind_mode",
      if (is_blind)
        shiny::tagList(shiny::icon("lock"),      " Blind ON")
      else
        shiny::tagList(shiny::icon("lock-open"), " Blind"),
      class      = if (is_blind) "btn-sm btn-warning" else "btn-sm btn-outline-light",
      `aria-pressed` = if (is_blind) "true" else "false",
      title = paste0(
        "Blind coding mode: hide other coders’ work while coding. ",
        if (is_blind) "Currently ON — click to disable."
        else "Currently OFF — click to enable."
      )
    )
  })

  shiny::observeEvent(input$btn_blind_mode, {
    rv$blind_mode <- !isTRUE(rv$blind_mode)
  })

  mod_documents_server("docs",       rv)
  mod_codebook_server("codebook",    rv)
  mod_themes_server("themes",        rv)
  mod_coding_server("coding",        rv, session)
  mod_compare_server("compare",      rv)
  mod_query_server("query",          rv)
  mod_graph_server("graph",          rv)
  mod_member_check_server("members", rv)
  mod_export_server("export",        rv)
  mod_audit_server("audit",          rv)

  shiny::observeEvent(input$btn_project_info, {
    info <- qc_project_info(rv$project)
    shiny::showModal(shiny::modalDialog(
      title      = "Project Info",
      `aria-modal` = "true",
      shiny::tags$dl(
        shiny::tags$dt("Name"),  shiny::tags$dd(info$name),
        shiny::tags$dt("Owner"), shiny::tags$dd(info$owner),
        shiny::tags$dt("Path"),  shiny::tags$dd(rv$project$path),
        shiny::tags$dt("Memo"),  shiny::tags$dd(info$memo)
      ),
      easyClose = TRUE,
      footer    = shiny::modalButton("Close")
    ))
  })
}

#' Launch the saturate Shiny GUI
#'
#' Opens an interactive coding interface. Pass a `brand` list to apply
#' organisation-specific colours and a custom app name — useful for
#' institutions that want to present the tool under their own branding.
#'
#' @param project A `qc_project` object created by [qc_new()] or [qc_open()].
#' @param brand Optional named list for visual branding. Supported keys:
#'   \describe{
#'     \item{`name`}{App title shown in the navbar (default: `"saturate"`).}
#'     \item{`primary`}{CSS hex colour for the navbar, primary buttons, and
#'       pagination (e.g. `"#003366"`). Ensure ≥ 4.5:1 contrast with white.}
#'     \item{`primary_hover`}{Slightly darker version for hover states.}
#'     \item{`primary_fg`}{Foreground (text) colour on `primary` backgrounds
#'       (default: `"#ffffff"`).}
#'     \item{`accent`}{Accent colour used in charts and sparklines.}
#'     \item{`custom_css`}{A raw CSS string appended last — override anything.}
#'   }
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Called for its side effect; does not return normally.
#' @export
shiny_saturate <- function(project, brand = NULL, ...) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  app_name  <- (brand$name %||% "saturate")
  brand_css <- .build_brand_css(brand)

  .env          <- new.env(parent = emptyenv())
  .env$project  <- project

  app <- shiny::shinyApp(
    ui     = saturate_ui(app_name, brand_css),
    server = function(input, output, session) {
      saturate_server(input, output, session, .env$project)
    }
  )
  shiny::runApp(app, ...)
}

# ── Brand CSS builder ─────────────────────────────────────────────────────────

.build_brand_css <- function(brand) {
  if (is.null(brand)) return("")

  vars <- character(0)
  .v <- function(name, key) {
    val <- brand[[key]]
    if (!is.null(val) && nchar(trimws(val)) > 0L)
      vars <<- c(vars, paste0(name, ":", trimws(val)))
  }

  .v("--sat-primary",       "primary")
  .v("--sat-primary-hover", "primary_hover")
  .v("--sat-primary-fg",    "primary_fg")
  .v("--sat-accent",        "accent")
  .v("--sat-accent-fg",     "accent_fg")

  root_block <- if (length(vars) > 0L)
    paste0(":root{", paste(vars, collapse = ";"), "}")
  else
    ""

  # Navbar background mirrors --sat-primary through CSS variable already;
  # add explicit overrides so bslib-compiled BS classes also respond.
  nav_block <- if (!is.null(brand$primary)) {
    paste0(
      ".navbar{background-color:", brand$primary, "!important}",
      ".btn-primary{background-color:", brand$primary,
        ";border-color:", brand$primary, "}",
      ".btn-primary:hover,.btn-primary:active{",
        "background-color:", brand$primary_hover %||% brand$primary,
        ";border-color:", brand$primary_hover %||% brand$primary, "}",
      ".dataTables_wrapper .dataTables_paginate .paginate_button.current,",
      ".dataTables_wrapper .dataTables_paginate .paginate_button.current:hover{",
        "background:", brand$primary, "!important;",
        "color:", brand$primary_fg %||% "#fff", "!important}"
    )
  } else ""

  custom <- brand$custom_css %||% ""

  paste0(root_block, nav_block, custom)
}
