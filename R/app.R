# ── Top-level UI ──────────────────────────────────────────────────────────────

saturate_ui <- function(app_name = "saturate", brand_css = "") {
  default_coder <- Sys.info()[["user"]] %||% "default"
  css_file      <- system.file("app", "styles.css", package = "saturate")
  profile_js    <- system.file("app", "profile.js", package = "saturate")

  shiny::tagList(

    # ── Skip-to-main-content (keyboard / screen-reader accessibility) ──────
    shiny::tags$a(
      href  = "#sat-main-content",
      class = "skip-link",
      "Skip to main content"
    ),

    shiny::tags$head(
      shiny::includeCSS(css_file),
      shiny::includeScript(profile_js),
      # Brand CSS variable overrides — injected when brand != NULL
      if (nchar(brand_css) > 0L)
        shiny::tags$style(shiny::HTML(brand_css))
    ),

    .profile_gate_ui(default_coder),

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
      bslib::nav_panel("Cases",     mod_cases_ui("cases")),
      bslib::nav_panel("Journal",   mod_memos_ui("memos")),
      bslib::nav_panel("Members",   mod_member_check_ui("members")),
      bslib::nav_panel("Export",    mod_export_ui("export")),
      bslib::nav_panel("Audit",     mod_audit_ui("audit")),
      bslib::nav_panel("Help",      mod_help_ui("help")),

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

          shiny::actionButton("btn_settings",
            shiny::tagList(shiny::icon("gear"), "Settings"),
            class = "btn-sm btn-outline-light ms-1 qc-navbar-settings",
            `aria-label` = "Open profile and display settings"
          ),

          shiny::actionButton("btn_project_info", "Project",
            class = "btn-sm btn-outline-light ms-1 qc-navbar-project",
            `aria-label` = "View project information"
          )
        )
      )
    )
  )
}

.profile_gate_ui <- function(default_coder = "default") {
  shiny::div(
    id = "qc-profile-gate",
    class = "qc-profile-gate",
    role = "dialog",
    `aria-modal` = "true",
    `aria-labelledby` = "qc-profile-title",
    shiny::div(
      class = "qc-profile-panel",
      shiny::div(
        class = "qc-profile-brand",
        shiny::span(class = "qc-profile-mark", "s"),
        shiny::span("saturate")
      ),
      shiny::h1(id = "qc-profile-title", "Choose a profile"),
      shiny::p(
        class = "qc-profile-copy",
        "Profiles set the coder name used when saving codings, memos, ",
        "member checks, and theme edits. They are remembered in this browser."
      ),
      shiny::div(id = "qc-profile-list", class = "qc-profile-list"),
      shiny::div(
        class = "qc-profile-create",
        shiny::tags$label(`for` = "qc-profile-new-name", "New profile"),
        shiny::div(
          class = "qc-profile-create-row",
          shiny::tags$input(
            id = "qc-profile-new-name",
            type = "text",
            class = "form-control",
            placeholder = paste0("e.g. ", default_coder)
          ),
          shiny::tags$button(
            id = "qc-profile-create",
            type = "button",
            class = "btn btn-primary",
            "Create"
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
    profile_state    = NULL,
    blind_mode       = FALSE
  )

  shiny::observeEvent(input$profile_state, {
    rv$profile_state <- input$profile_state
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$profile_selected, {
    coder <- .profile_payload_name(input$profile_selected)
    if (nchar(coder) == 0L) return()
    rv$current_coder <- coder
    shiny::updateTextInput(session, "current_coder", value = coder)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$current_coder, {
    coder <- trimws(as.character(input$current_coder %||% ""))
    if (nchar(coder) == 0L) return()
    rv$current_coder <- coder
  }, ignoreInit = TRUE)

  output$coder_suggestions_ui <- shiny::renderUI({
    rv$refresh_codes
    coders     <- tryCatch(qc_list_coders(rv$project)$coder,
                           error = function(e) character())
    profiles   <- .profile_state_names(rv$profile_state, character())
    all_coders <- unique(c(rv$current_coder %||% default_coder, profiles, coders))
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
      class      = if (is_blind)
        "btn-sm btn-warning qc-blind-toggle"
      else
        "btn-sm btn-outline-light qc-blind-toggle",
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

  # ── Profile and display settings ───────────────────────────────────────────

  shiny::observeEvent(input$btn_settings, {
    shiny::showModal(.profile_settings_modal(rv$profile_state, rv$current_coder))
  })

  shiny::observeEvent(input$btn_settings_switch, {
    profile <- trimws(input$settings_profile %||% "")
    if (nchar(profile) == 0L) {
      shiny::showNotification("Choose a profile to switch to.", type = "warning")
      return()
    }
    session$sendCustomMessage("qc_profile_action",
      list(action = "switch", name = profile))
    shiny::removeModal()
  })

  shiny::observeEvent(input$btn_settings_create, {
    profile <- trimws(input$settings_new_profile %||% "")
    if (nchar(profile) == 0L) {
      shiny::showNotification("Enter a profile name.", type = "warning")
      return()
    }
    session$sendCustomMessage("qc_profile_action",
      list(action = "create", name = profile))
    shiny::removeModal()
  })

  shiny::observeEvent(input$btn_settings_delete, {
    profile <- trimws(input$settings_profile %||% "")
    if (nchar(profile) == 0L) {
      shiny::showNotification("Choose a profile to delete.", type = "warning")
      return()
    }
    session$sendCustomMessage("qc_profile_action",
      list(action = "delete", name = profile))
    shiny::removeModal()
  })

  shiny::observeEvent(input$btn_settings_save, {
    settings <- list(
      colorTheme        = input$settings_color_theme %||% "light",
      uiFont             = input$settings_ui_font %||% "system",
      uiScale            = as.integer(input$settings_ui_scale %||% 100L),
      documentFont       = input$settings_doc_font %||% "serif",
      documentScale      = as.integer(input$settings_doc_scale %||% 100L),
      documentLineHeight = as.numeric(input$settings_doc_line_height %||% 1.9),
      documentHeight     = as.integer(input$settings_doc_height %||% 68L),
      tableDensity       = input$settings_table_density %||% "comfortable",
      reduceMotion       = isTRUE(input$settings_reduce_motion),
      showLineNumbers    = isTRUE(input$settings_show_line_numbers),
      highlightOpacity   = as.numeric(input$settings_highlight_opacity %||% 0.33)
    )
    session$sendCustomMessage("qc_profile_action",
      list(action = "settings", settings = settings))
    shiny::showNotification("Settings saved.", type = "message")
  })

  shiny::observeEvent(input$btn_settings_restore, {
    session$sendCustomMessage("qc_profile_action",
      list(action = "reset_settings"))
    shiny::removeModal()
    shiny::showNotification("Settings restored to defaults.", type = "message")
  })

  shiny::observeEvent(input$btn_profile_signout, {
    session$sendCustomMessage("qc_profile_action", list(action = "logout"))
    shiny::removeModal()
  })

  mod_documents_server("docs",       rv)
  mod_codebook_server("codebook",    rv)
  mod_themes_server("themes",        rv)
  mod_coding_server("coding",        rv, session)
  mod_compare_server("compare",      rv)
  mod_query_server("query",          rv)
  mod_cases_server("cases",          rv)
  mod_memos_server("memos",          rv)
  mod_member_check_server("members", rv)
  mod_export_server("export",        rv)
  mod_audit_server("audit",          rv)
  mod_help_server("help",            rv)

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
#'     \item{`primary`}{CSS hex colour for the navbar and primary buttons
#'       (e.g. `"#003366"`). Ensure ≥ 4.5:1 contrast with white.}
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

# ── Profile/settings helpers ─────────────────────────────────────────────────

.profile_payload_name <- function(payload) {
  if (is.null(payload)) return("")
  if (is.list(payload)) payload <- payload$name %||% ""
  trimws(as.character(payload %||% ""))
}

.profile_state_names <- function(state, fallback = character()) {
  if (is.null(state) || is.null(state$profiles)) return(.clean_profile_names(fallback))

  profiles <- state$profiles
  if (is.data.frame(profiles) && "name" %in% names(profiles)) {
    names <- profiles$name
  } else if (is.list(profiles)) {
    names <- vapply(profiles, function(profile) {
      if (is.list(profile)) as.character(profile$name %||% "")
      else as.character(profile %||% "")
    }, character(1L))
  } else {
    names <- as.character(profiles)
  }

  .clean_profile_names(c(names, fallback))
}

.clean_profile_names <- function(x) {
  x <- trimws(as.character(x %||% character()))
  unique(x[!is.na(x) & nzchar(x)])
}

.profile_state_active <- function(state, fallback = "default") {
  active <- if (!is.null(state)) state$activeProfile %||% "" else ""
  active <- trimws(as.character(active))
  if (nchar(active) > 0L) active else trimws(as.character(fallback %||% "default"))
}

.profile_state_settings <- function(state) {
  settings <- if (!is.null(state)) state$settings %||% list() else list()

  list(
    colorTheme = .profile_setting_chr(settings, "colorTheme", "light"),
    uiFont = .profile_setting_chr(settings, "uiFont", "system"),
    uiScale = .profile_setting_num(settings, "uiScale", 100, 90, 125),
    documentFont = .profile_setting_chr(settings, "documentFont", "serif"),
    documentScale = .profile_setting_num(settings, "documentScale", 100, 85, 150),
    documentLineHeight = .profile_setting_num(settings, "documentLineHeight", 1.9, 1.4, 2.4),
    documentHeight = .profile_setting_num(settings, "documentHeight", 68, 48, 86),
    tableDensity = .profile_setting_chr(settings, "tableDensity", "comfortable"),
    reduceMotion = .profile_setting_bool(settings, "reduceMotion", FALSE),
    showLineNumbers = .profile_setting_bool(settings, "showLineNumbers", FALSE),
    highlightOpacity = .profile_setting_num(settings, "highlightOpacity", 0.33, 0.15, 0.85)
  )
}

.profile_setting_chr <- function(settings, key, default) {
  val <- settings[[key]] %||% default
  trimws(as.character(val %||% default))
}

.profile_setting_num <- function(settings, key, default, min_val, max_val) {
  val <- suppressWarnings(as.numeric(settings[[key]] %||% default))
  if (length(val) == 0L || is.na(val)) val <- default
  max(min_val, min(max_val, val))
}

.profile_setting_bool <- function(settings, key, default) {
  val <- settings[[key]]
  if (is.null(val)) return(isTRUE(default))
  if (is.logical(val)) return(isTRUE(val))
  tolower(trimws(as.character(val))) %in% c("true", "1", "yes", "y", "on")
}

.qc_app_color_theme <- function(rv) {
  tryCatch(
    .profile_state_settings(rv$profile_state)$colorTheme,
    error = function(e) "light"
  )
}

.qc_app_dark_mode <- function(rv) {
  identical(.qc_app_color_theme(rv), "dark")
}

.profile_settings_modal <- function(state, current_coder) {
  active   <- .profile_state_active(state, current_coder)
  profiles <- .profile_state_names(state, active)
  settings <- .profile_state_settings(state)

  font_choices <- c(
    "System UI" = "system",
    "Serif" = "serif",
    "Readable sans" = "sans",
    "Monospace" = "mono"
  )
  theme_choices <- c(
    "Light" = "light",
    "Dark" = "dark",
    "High contrast" = "contrast"
  )
  density_choices <- c(
    "Compact" = "compact",
    "Comfortable" = "comfortable",
    "Roomy" = "roomy"
  )
  if (!settings$tableDensity %in% density_choices)
    settings$tableDensity <- "comfortable"

  shiny::modalDialog(
    title = "Settings",
    size = "l",
    easyClose = TRUE,

    shiny::div(
      class = "qc-settings-section",
      shiny::h6("Profile"),
      qc_help_note(
        "The active profile becomes the coder name saved with new codings, ",
        "memos, member checks, and theme edits."
      ),
      shiny::div(
        class = "qc-settings-profile-actions",
        shiny::div(
          class = "qc-settings-profile-select",
          shiny::selectInput("settings_profile", "Active profile",
            choices = stats::setNames(profiles, profiles),
            selected = active)
        ),
        shiny::actionButton("btn_settings_switch", "Switch",
          class = "btn-primary"),
        shiny::actionButton("btn_settings_delete", "Delete",
          class = "btn-outline-danger")
      ),
      shiny::div(
        class = "qc-settings-profile-actions",
        shiny::div(
          class = "qc-settings-profile-select",
          shiny::textInput("settings_new_profile", "Create profile",
            placeholder = "Coder name")
        ),
        shiny::actionButton("btn_settings_create", "Create and switch",
          class = "btn-outline-primary")
      )
    ),

    shiny::hr(),

    shiny::div(
      class = "qc-settings-section",
      shiny::h6("Display"),
      shiny::selectInput("settings_color_theme", "Colour profile",
        choices = theme_choices,
        selected = settings$colorTheme),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::selectInput("settings_ui_font", "Interface font",
          choices = font_choices,
          selected = settings$uiFont),
        shiny::sliderInput("settings_ui_scale", "Interface size",
          min = 90, max = 125, value = settings$uiScale, step = 5,
          ticks = FALSE, post = "%"),
        shiny::selectInput("settings_doc_font", "Document font",
          choices = font_choices,
          selected = settings$documentFont),
        shiny::sliderInput("settings_doc_scale", "Document text size",
          min = 85, max = 150, value = settings$documentScale, step = 5,
          ticks = FALSE, post = "%")
      ),
      shiny::sliderInput("settings_doc_line_height", "Document line spacing",
        min = 1.4, max = 2.4, value = settings$documentLineHeight,
        step = 0.05, ticks = FALSE),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::sliderInput("settings_doc_height", "Document pane height",
          min = 48, max = 86, value = settings$documentHeight, step = 2,
          ticks = FALSE, post = "vh"),
        shiny::sliderInput("settings_highlight_opacity", "Default highlight strength",
          min = 0.15, max = 0.85, value = settings$highlightOpacity,
          step = 0.05, ticks = FALSE)
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        shiny::selectInput("settings_table_density", "Table density",
          choices = density_choices,
          selected = settings$tableDensity),
        shiny::checkboxInput("settings_show_line_numbers",
          "Show line numbers by default",
          value = isTRUE(settings$showLineNumbers)),
        shiny::checkboxInput("settings_reduce_motion",
          "Reduce animation",
          value = isTRUE(settings$reduceMotion))
      )
    ),

    footer = shiny::tagList(
      shiny::actionButton("btn_profile_signout", "Sign out",
        class = "btn-outline-secondary me-auto"),
      shiny::actionButton("btn_settings_restore", "Restore defaults",
        class = "btn-outline-secondary"),
      shiny::modalButton("Close"),
      shiny::actionButton("btn_settings_save", "Save settings",
        class = "btn-primary")
    )
  )
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
        ";border-color:", brand$primary_hover %||% brand$primary, "}"
    )
  } else ""

  custom <- brand$custom_css %||% ""

  paste0(root_block, nav_block, custom)
}
