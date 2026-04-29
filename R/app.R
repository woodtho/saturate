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
      title  = shiny::tags$img(
        src    = "saturate-assets/logo.png",
        height = "38",
        alt    = "saturate — for qualitative coding",
        style  = "display:block;"
      ),
      window_title = "saturate",
      theme    = bslib::bs_theme(version = 5, bootswatch = "flatly"),
      lang     = "en",
      selected = "Coding",

      # ── Prep workspace ──────────────────────────────────────────────────
      bslib::nav_menu(
        title = shiny::tagList(shiny::icon("folder-open"), " Prep"),
        bslib::nav_panel("Documents",
          shiny::div(
            id = "sat-main-content", tabindex = "-1",
            shiny::tagList(shinyjs::useShinyjs(), mod_documents_ui("docs"))
          )
        ),
        bslib::nav_panel("Codebook", mod_codebook_ui("codebook")),
        bslib::nav_panel("Cases",    mod_cases_ui("cases")),
        bslib::nav_panel("Journal",  mod_memos_ui("memos"))
      ),

      # ── Coding workspace ─────────────────────────────────────────────────
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("tag"), " Coding"),
        value = "Coding",
        mod_coding_ui("coding")
      ),

      # ── Analysis workspace ───────────────────────────────────────────────
      bslib::nav_menu(
        title = shiny::tagList(shiny::icon("chart-bar"), " Analysis"),
        bslib::nav_panel("Compare", mod_compare_ui("compare")),
        bslib::nav_panel("Themes",  mod_themes_ui("themes")),
        bslib::nav_panel("Query",   mod_query_ui("query"))
      ),

      # ── Review workspace ─────────────────────────────────────────────────
      bslib::nav_menu(
        title = shiny::tagList(shiny::icon("clipboard-check"), " Review"),
        bslib::nav_panel("Member Checks", mod_member_check_ui("members")),
        bslib::nav_panel("Audit",         mod_audit_ui("audit")),
        bslib::nav_panel("Export",        mod_export_ui("export"))
      ),

      bslib::nav_panel("Help", mod_help_ui("help")),

      bslib::nav_spacer(),
      bslib::nav_item(
        shiny::div(
          class = "qc-navbar-session d-flex align-items-center gap-2",

          # ── Active coder ─────────────────────────────────────────────────
          shiny::div(
            class = "qc-coder-block",
            shiny::tags$label(
              "CODING AS",
              `for`  = "current_coder-selectized",
              class  = "qc-coder-label",
              id     = "lbl-current-coder"
            ),
            shiny::selectizeInput(
              "current_coder",
              label   = NULL,
              choices = default_coder,
              selected = default_coder,
              width   = "160px",
              options = list(
                create         = TRUE,
                createOnBlur   = TRUE,
                persist        = FALSE,
                placeholder    = "Coder name…",
                selectOnTab    = TRUE,
                openOnFocus    = TRUE
              )
            )
          ),

          # ── Blind mode toggle ─────────────────────────────────────────────
          shiny::uiOutput("ui_blind_btn", inline = TRUE),

          shiny::actionButton("btn_help_modal",
            shiny::icon("circle-question"),
            class = "btn-sm btn-outline-light qc-navbar-help",
            title = "Help",
            `aria-label` = "Open help reference"
          ),

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
        shiny::tags$img(
          src   = "saturate-assets/logo.png",
          height = "200",
          alt   = "saturate",
          style = "display:block;margin:0 auto 1rem;"
        )
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
    blind_mode       = FALSE,
    colorblind_mode  = FALSE
  )

  shiny::observeEvent(input$profile_state, {
    rv$profile_state <- input$profile_state
  }, ignoreInit = FALSE)

  # ── Push DB profiles to JS on first flush ─────────────────────────────────
  session$onFlushed(function() {
    profiles_df <- tryCatch(.db_list_profiles(rv$project), error = function(e) NULL)
    if (is.null(profiles_df) || nrow(profiles_df) == 0L) return()
    has_json <- requireNamespace("jsonlite", quietly = TRUE)
    profiles_list <- lapply(seq_len(nrow(profiles_df)), function(i) {
      row       <- profiles_df[i, , drop = FALSE]
      settings  <- if (has_json)
        tryCatch(jsonlite::fromJSON(row$settings_json %||% "{}", simplifyVector = FALSE),
                 error = function(e) list())
      else list()
      list(
        name       = row$name,
        createdAt  = format(row$created_at,  "%Y-%m-%dT%H:%M:%SZ"),
        lastUsedAt = if (!is.na(row$last_used_at))
          format(row$last_used_at, "%Y-%m-%dT%H:%M:%SZ") else NULL,
        settings   = settings
      )
    })
    session$sendCustomMessage("qc_load_profiles", list(profiles = profiles_list))
  }, once = TRUE)

  shiny::observeEvent(input$profile_selected, {
    coder <- .profile_payload_name(input$profile_selected)
    if (nchar(coder) == 0L) return()
    rv$current_coder <- coder
    shiny::updateSelectizeInput(session, "current_coder", selected = coder)
    tryCatch({
      .db_upsert_profile(rv$project, coder)
      .db_touch_profile(rv$project, coder)
    }, error = function(e) NULL)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$current_coder, {
    coder <- trimws(as.character(input$current_coder %||% ""))
    if (nchar(coder) == 0L) return()
    rv$current_coder <- coder
  }, ignoreInit = TRUE)

  shiny::observe({
    rv$refresh_codes
    coders     <- tryCatch(qc_list_coders(rv$project)$coder,
                           error = function(e) character())
    profiles   <- .profile_state_names(rv$profile_state, character())
    all_coders <- unique(c(rv$current_coder %||% default_coder, profiles, coders))
    all_coders <- all_coders[!is.na(all_coders) & nzchar(all_coders)]
    shiny::updateSelectizeInput(session, "current_coder",
      choices  = all_coders,
      selected = rv$current_coder %||% default_coder
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
    tryCatch({
      .db_upsert_profile(rv$project, profile)
      .db_touch_profile(rv$project, profile)
    }, error = function(e) NULL)
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
    tryCatch(.db_upsert_profile(rv$project, profile), error = function(e) NULL)
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
    tryCatch(.db_delete_profile(rv$project, profile), error = function(e) NULL)
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
    tryCatch(
      .db_save_profile_settings(rv$project, rv$current_coder %||% "default", settings),
      error = function(e) NULL
    )
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

  shiny::observeEvent(input$btn_help_modal, {
    shinyjs::runjs(
      "$('.navbar-nav .nav-link').filter(function(){ return $(this).text().trim()==='Help'; }).click();"
    )
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

  # ── Split download (static registration — button lives in renderUI) ──────────
  output$dl_split <- shiny::downloadHandler(
    filename = function() {
      info <- tryCatch(qc_project_info(rv$project),
                       error = function(e) list(name = "project"))
      base <- gsub("[^A-Za-z0-9_-]", "_", info$name %||% "project")
      paste0(base, "_split_", Sys.Date(), ".duckdb")
    },
    content = function(file) {
      ids <- if (!isTRUE(input$split_scope == "all") &&
                 length(input$split_source_ids) > 0L)
        as.integer(input$split_source_ids)
      else NULL
      dest <- qc_split_project(
        rv$project, file,
        source_ids      = ids,
        include_codings = isTRUE(input$split_include_codings),
        overwrite       = TRUE
      )
      qc_close(dest)
    }
  )

  output$split_docs_ui <- shiny::renderUI({
    shiny::req(rv$project)
    docs <- tryCatch(
      .query(rv$project$con,
             "SELECT id, name FROM sources WHERE status = 1 ORDER BY name"),
      error = function(e) tibble::tibble(id = integer(), name = character())
    )
    shiny::tagList(
      shiny::radioButtons("split_scope", "Documents to include",
        choices  = c("All documents" = "all", "Select documents" = "select"),
        selected = "all"),
      shiny::conditionalPanel(
        "input.split_scope == 'select'",
        shiny::checkboxGroupInput("split_source_ids", NULL,
          choices = if (nrow(docs) > 0L)
            setNames(as.character(docs$id), docs$name)
          else character())
      ),
      shiny::checkboxInput("split_include_codings",
        "Include existing codings", value = FALSE),
      shiny::div(
        class = "mt-3",
        shiny::downloadButton("dl_split", "Download split project (.duckdb)",
          class = "btn-primary btn-sm")
      )
    )
  })

  # ── Merge state + observers ───────────────────────────────────────────────────
  merge_preview_rv <- shiny::reactiveVal(NULL)

  output$merge_preview_ui <- shiny::renderUI({
    preview <- merge_preview_rv()
    if (is.null(preview)) return(NULL)
    if (!is.null(preview$error)) {
      return(shiny::div(class = "alert alert-danger mt-2",
        shiny::icon("circle-exclamation"), " ", preview$error))
    }
    shiny::tagList(
      shiny::hr(),
      shiny::tags$p(shiny::strong("Contributor file contains:")),
      shiny::tags$ul(
        shiny::tags$li(preview$n_codes,  " codes"),
        shiny::tags$li(preview$n_srcs,   " documents"),
        shiny::tags$li(preview$n_cods,   " codings"),
        shiny::tags$li(preview$n_themes, " themes"),
        shiny::tags$li(preview$n_memos,  " memos")
      ),
      shiny::radioButtons("merge_on_conflict", "On duplicate codings",
        choices  = c("Skip existing" = "skip", "Replace existing" = "replace"),
        selected = "skip", inline = TRUE),
      shiny::actionButton("btn_merge_confirm", "Merge into project",
        class = "btn-primary btn-sm mt-1")
    )
  })

  shiny::observeEvent(input$btn_merge_preview, {
    shiny::req(input$merge_file)
    path <- input$merge_file$datapath
    tryCatch({
      con_b <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
      on.exit(try(DBI::dbDisconnect(con_b, shutdown = TRUE), silent = TRUE))
      merge_preview_rv(list(
        path     = path,
        n_codes  = .query(con_b, "SELECT COUNT(*) AS n FROM codes  WHERE status = 1")$n,
        n_srcs   = .query(con_b, "SELECT COUNT(*) AS n FROM sources WHERE status = 1")$n,
        n_cods   = .query(con_b, "SELECT COUNT(*) AS n FROM codings WHERE status = 1")$n,
        n_themes = .query(con_b, "SELECT COUNT(*) AS n FROM themes  WHERE status = 1")$n,
        n_memos  = .query(con_b,
          "SELECT COUNT(*) AS n FROM project_memos WHERE status = 1")$n
      ))
    }, error = function(e) {
      merge_preview_rv(list(error = conditionMessage(e)))
    })
  })

  shiny::observeEvent(input$btn_merge_confirm, {
    preview <- merge_preview_rv()
    shiny::req(preview, is.null(preview$error))
    tryCatch({
      result <- qc_merge_project(
        rv$project, preview$path,
        on_conflict = input$merge_on_conflict %||% "skip"
      )
      shiny::removeModal()
      merge_preview_rv(NULL)
      rv$refresh_codes <- rv$refresh_codes + 1L
      rv$refresh_docs  <- rv$refresh_docs  + 1L
      shiny::showNotification(paste0(
        "Merge complete — ",
        result$codings_added, " coding(s), ",
        result$codes_added,   " code(s), ",
        result$sources_added, " document(s) added; ",
        result$codings_skip,  " duplicate(s) skipped."
      ), type = "message", duration = 8)
    }, error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })

  shiny::observeEvent(input$btn_project_info, {
    merge_preview_rv(NULL)
    info <- qc_project_info(rv$project)
    shiny::showModal(shiny::modalDialog(
      title      = "Project",
      size       = "l",
      `aria-modal` = "true",
      easyClose  = TRUE,
      footer     = shiny::modalButton("Close"),
      shiny::tabsetPanel(
        shiny::tabPanel("Info",
          shiny::tags$dl(
            class = "mt-3",
            shiny::tags$dt("Name"),  shiny::tags$dd(info$name),
            shiny::tags$dt("Owner"), shiny::tags$dd(info$owner),
            shiny::tags$dt("Path"),  shiny::tags$dd(rv$project$path),
            shiny::tags$dt("Memo"),  shiny::tags$dd(info$memo)
          )
        ),
        shiny::tabPanel("Split",
          shiny::div(class = "mt-3", shiny::uiOutput("split_docs_ui"))
        ),
        shiny::tabPanel("Merge",
          shiny::div(
            class = "mt-3",
            shiny::fileInput("merge_file",
              "Contributor project file (.duckdb)",
              accept = ".duckdb", width = "100%"),
            shiny::actionButton("btn_merge_preview", "Preview",
              class = "btn-secondary btn-sm"),
            shiny::uiOutput("merge_preview_ui")
          )
        )
      )
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
#' @param max_upload_mb Integer. Maximum file size (MB) accepted by the Merge
#'   file-upload input (default: 500 MB).
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Called for its side effect; does not return normally.
#' @export
shiny_saturate <- function(project, brand = NULL, max_upload_mb = 500L, ...) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  shiny::addResourcePath("saturate-assets",
    system.file("app", package = "saturate"))

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
