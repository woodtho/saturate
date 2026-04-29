mod_coding_ui <- function(id) {
  ns       <- shiny::NS(id)
  js_file  <- system.file("app", "coding.js",  package = "saturate")
  shiny::tagList(
    shiny::tags$head(
      shiny::includeScript(js_file)
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),

      # ── Left: document display ─────────────────────────────────────────────
      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "qc-doc-toolbar d-flex justify-content-between align-items-center w-100",
            shiny::div(
              class = "flex-grow-1 me-2",
              style = "min-width:0;",
              shiny::selectizeInput(
                ns("open_doc"),
                label   = NULL,
                choices = character(0),
                width   = "100%",
                options = list(placeholder = "Open document…")
              )
            ),
            shiny::div(
              class = "qc-doc-actions d-flex gap-1 flex-shrink-0 align-items-center flex-wrap justify-content-end",
              shiny::div(
                class = "qc-doc-lines-toggle form-check form-switch d-flex align-items-center gap-1 me-1",
                style = "margin-bottom:0;",
                shiny::tags$input(
                  id    = ns("show_line_numbers"),
                  class = "form-check-input",
                  type  = "checkbox",
                  role  = "switch"
                ),
                shiny::tags$label(
                  `for` = ns("show_line_numbers"),
                  class = "form-check-label text-muted",
                  style = "font-size:0.75rem;white-space:nowrap;",
                  "Lines"
                )
              ),
              shiny::actionButton(ns("btn_nav_prev"), "← Prev",
                class = "btn-sm btn-outline-secondary",
                title = "Previous uncoded segment  (p)"),
              shiny::actionButton(ns("btn_nav_next"), "Next →",
                class = "btn-sm btn-outline-secondary",
                title = "Next uncoded segment  (n)"),
              shiny::actionButton(ns("btn_nav_disputed"), "Disputed",
                class = "btn-sm btn-outline-warning",
                title = "Next disputed / draft segment  (d)"),
              shiny::actionButton(ns("btn_shortcuts"), "?",
                class = "btn-sm btn-outline-light",
                title = "Keyboard shortcuts  (?)")
            )
          )
        ),
        shiny::div(
          class = "qc-doc-search-strip px-3 pt-2 pb-1 border-bottom d-flex gap-2 align-items-center",
          shiny::div(
            class = "qc-doc-search-field flex-grow-1",
            shiny::textInput(ns("doc_search"), label = NULL,
              placeholder = "Search document…",
              width = "100%")
          ),
          shiny::div(
            class = "form-check d-flex align-items-center gap-1 flex-shrink-0",
            style = "margin-bottom:0;",
            shiny::tags$input(
              id    = ns("search_regex"),
              class = "form-check-input",
              type  = "checkbox"
            ),
            shiny::tags$label(
              `for` = ns("search_regex"),
              class = "form-check-label text-muted",
              style = "font-size:0.75rem;white-space:nowrap;",
              "Regex"
            )
          ),
          shiny::uiOutput(ns("search_match_count"), inline = TRUE)
        ),
        shiny::div(
          class = "px-3",
          qc_help_details(
            "Document pane help",
            shiny::p(
              "Select a passage in the document pane, then use the Code panel ",
              "to assign a code. Highlighted passages show existing codings."
            ),
            qc_help_list(c(
              "Next and Prev jump between uncoded segments.",
              "Disputed jumps to draft or contested coding work.",
              "Search highlights matching text without changing the saved project."
            ))
          )
        ),
        shiny::uiOutput(ns("blind_banner")),
        shiny::uiOutput(ns("text_display")),
        bslib::card_footer(
          shiny::div(
            class = "qc-tts-controls d-flex gap-2 align-items-center justify-content-center",
            shiny::tags$button(
              id = ns("tts_playpause"),
              type = "button",
              class = "btn btn-sm btn-outline-secondary",
              `aria-label` = "Read aloud",
              title = "Read aloud (or pause/resume)",
              `data-qc-tts-action` = "playpause",
              "⏯"
            ),
            shiny::tags$button(
              id = ns("tts_stop"),
              type = "button",
              class = "btn btn-sm btn-outline-secondary",
              `aria-label` = "Stop narration",
              title = "Stop the current narration",
              `data-qc-tts-action` = "stop",
              disabled = "disabled",
              "⏹"
            )
          )
        )
      ),

      # ── Right: apply-code panel ────────────────────────────────────────────
      bslib::card(
        bslib::card_header("Code"),
        shiny::div(
          class = "p-2",
          qc_help_details(
            "Code panel help",
            shiny::p(
              "Use the selected text preview to confirm the passage before saving. ",
              "Choose a code, add confidence or a memo if useful, then apply it."
            ),
            qc_help_list(c(
              "Create Excerpt stores a notable passage without changing its code.",
              "Display filters change what you see in the document; they do not delete codings.",
              "Click rows in the coding or excerpt tables to review saved work."
            ))
          ),

          # ── Display filters (collapsible) ──────────────────────────────────
          shiny::tags$details(
            class = "qc-panel-details",
            shiny::tags$summary(
              "Display filters"
            ),
            shiny::div(
              class = "qc-panel-details-body",
              shiny::selectizeInput(ns("filter_display_cats"),
                label   = "Show categories",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "All categories")),
              shiny::selectInput(ns("filter_display_coder"),
                label   = "Filter by coder",
                choices = c("All coders" = ""),
                selected = ""),
              shiny::sliderInput(ns("highlight_opacity"),
                label = "Highlight opacity",
                min = 0.1, max = 1.0, value = 0.33, step = 0.05,
                ticks = FALSE),
              shiny::checkboxInput(ns("cb_mode"),
                label = "Colorblind-safe highlights (borders)",
                value = FALSE)
            )
          ),

          # ── Code application ───────────────────────────────────────────────
          shiny::h6("Selected text"),
          shiny::uiOutput(ns("sel_preview")),
          shiny::div(
            class = "mb-2",
            shiny::tags$label("Code", class = "form-label",
                              `for` = ns("sel_code")),
            shiny::div(
              class = "d-flex gap-1",
              shiny::div(
                class = "flex-grow-1",
                shiny::selectInput(ns("sel_code"), label = NULL,
                                   choices = character(0), width = "100%")
              ),
              shiny::actionButton(ns("btn_new_code"), "+",
                class = "btn-outline-secondary",
                title = "Create a new code",
                style = paste0("height:38px;width:38px;padding:0;",
                               "font-size:1.15rem;line-height:1;flex-shrink:0;"))
            )
          ),
          shiny::uiOutput(ns("code_info")),
          shiny::selectInput(
            ns("confidence"), "Confidence",
            choices = c(
              "Unrated"       = "",
              "Low (25)"      = "25",
              "Medium (50)"   = "50",
              "High (75)"     = "75",
              "Certain (100)" = "100"
            ),
            selected = ""
          ),
          shiny::textAreaInput(ns("seg_memo"), "Segment memo", rows = 2),
          shiny::actionButton(ns("btn_apply"), "Apply Code",
            class = "btn-success w-100"),
          shiny::actionButton(ns("btn_create_excerpt"), "Create Excerpt",
            class = "btn-outline-secondary w-100 mt-1",
            title = "Save selected text as a named excerpt with optional memo"),
          shiny::hr(),
          shiny::h6("Codings in this document"),
          DT::dataTableOutput(ns("tbl_codings")),
          shiny::hr(),
          shiny::h6("Excerpts in this document"),
          DT::dataTableOutput(ns("tbl_excerpts"))
        )
      )
    )
  )
}

mod_coding_server <- function(id, rv, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    session$sendCustomMessage("qc_set_ns", list(ns_prefix = ns("")))

    lv <- shiny::reactiveValues(
      nav_targets        = NULL,  # tibble(start, end, text) of uncoded segments
      nav_cursor         = 0L,    # current position in nav_targets
      highlight_op       = 0.33,
      cb_mode            = FALSE,
      pending_sel        = NULL,  # code id to pre-select after next refresh
      editing_excerpt_id = NULL,
      last_coding_id     = NULL
    )

    shiny::observeEvent(rv$profile_state, {
      settings <- .profile_state_settings(rv$profile_state)
      lv$highlight_op <- settings$highlightOpacity
      shiny::updateSliderInput(session, "highlight_opacity",
        value = settings$highlightOpacity)
      shiny::updateCheckboxInput(session, "show_line_numbers",
        value = isTRUE(settings$showLineNumbers))
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    # ── Core reactives ─────────────────────────────────────────────────────────

    docs_rv <- shiny::reactive({
      rv$refresh_docs
      qc_list_documents(rv$project)
    })

    shiny::observe({
      docs <- docs_rv()
      choices <- stats::setNames(as.character(docs$id), docs$name)
      shiny::updateSelectizeInput(session, "open_doc", choices = choices,
        selected = if (!is.null(rv$active_source_id)) as.character(rv$active_source_id) else character(0))
    })

    shiny::observeEvent(rv$active_source_id, {
      shiny::updateSelectizeInput(session, "open_doc",
        selected = if (!is.null(rv$active_source_id)) as.character(rv$active_source_id) else character(0))
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$open_doc, {
      id <- suppressWarnings(as.integer(input$open_doc))
      if (!is.na(id) && !identical(rv$active_source_id, id))
        rv$active_source_id <- id
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    codes_rv <- shiny::reactive({
      rv$refresh_codes
      qc_list_codes(rv$project)
    })

    codings_rv <- shiny::reactive({
      shiny::req(rv$active_source_id)
      rv$refresh_codes
      coder_filter <- if (isTRUE(rv$blind_mode)) rv$current_coder else NULL
      qc_list_codings(rv$project, rv$active_source_id, coder = coder_filter)
    })

    # Codings after applying display filters
    filtered_codings_rv <- shiny::reactive({
      codings <- codings_rv()
      # Category filter
      dc <- input$filter_display_cats
      if (!is.null(dc) && length(dc) > 0L) {
        cat_links <- .query(rv$project$con, paste0(
          "SELECT code_id FROM code_category_links WHERE category_id IN (",
          paste(as.integer(dc), collapse = ","), ") AND status = 1"
        ))
        codings <- codings[codings$code_id %in% cat_links$code_id, ]
      }
      # Coder filter
      cdr <- input$filter_display_coder %||% ""
      if (nchar(cdr) > 0L)
        codings <- codings[codings$coder == cdr, ]
      codings
    })

    doc_rv <- shiny::reactive({
      shiny::req(rv$active_source_id)
      qc_get_document(rv$project, rv$active_source_id)
    })

    excerpts_rv <- shiny::reactive({
      shiny::req(rv$active_source_id)
      rv$refresh_codes
      tryCatch(
        qc_list_excerpts(rv$project, rv$active_source_id),
        error = function(e) tibble::tibble()
      )
    })

    # ── Output rendering ───────────────────────────────────────────────────────



    search_rv <- shiny::reactive({
      pattern <- trimws(input$doc_search %||% "")
      if (nchar(pattern) == 0L) return(NULL)
      doc <- doc_rv()
      shiny::req(doc)
      content   <- doc$content
      use_regex <- isTRUE(input$search_regex)
      escape_regex <- function(x) {
        gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
      }
      search_pattern <- if (use_regex) pattern else escape_regex(pattern)
      tryCatch({
        m <- gregexpr(search_pattern, content,
                      perl  = TRUE,
                      ignore.case = TRUE)[[1L]]
        if (m[[1L]] == -1L)
          return(tibble::tibble(selfirst = integer(0), selast = integer(0)))
        starts  <- as.integer(m)
        lengths <- attr(m, "match.length")
        tibble::tibble(selfirst = starts, selast = starts + lengths - 1L)
      }, error = function(e) NULL)
    })

    output$search_match_count <- shiny::renderUI({
      pattern <- trimws(input$doc_search %||% "")
      if (nchar(pattern) == 0L) return(NULL)
      sr <- search_rv()
      if (is.null(sr)) {
        return(shiny::span("Invalid pattern",
          class = "text-danger", style = "font-size:0.75rem;"))
      }
      n <- nrow(sr)
      shiny::span(
        if (n == 0L) "No matches"
        else paste0(n, " match", if (n != 1L) "es" else ""),
        class = if (n == 0L) "text-muted" else "text-success fw-semibold",
        style = "font-size:0.75rem;white-space:nowrap;"
      )
    })

    output$text_display <- shiny::renderUI({
      shiny::req(doc_rv())
      build_highlighted_html(
        doc_rv()$content,
        filtered_codings_rv(),
        opacity           = lv$highlight_op,
        cb_mode           = lv$cb_mode,
        excerpts          = excerpts_rv(),
        show_line_numbers = isTRUE(input$show_line_numbers),
        search_ranges     = search_rv()
      )
    })

    output$sel_preview <- shiny::renderUI({
      txt <- input$selection$text %||% ""
      shiny::div(
        class = "qc-selection-preview",
        if (nchar(txt) > 0L) txt
        else shiny::span("No text selected", style = "color:var(--sat-text-muted);font-style:normal;")
      )
    })

    output$blind_banner <- shiny::renderUI({
      if (!isTRUE(rv$blind_mode)) return(NULL)
      shiny::div(
        class = "alert alert-warning mb-0 py-2 px-3 rounded-0 border-0 border-bottom",
        style = "font-size:0.82rem;",
        shiny::icon("lock"), " ",
        shiny::tags$strong("Blind mode active — "),
        "showing only ",
        shiny::tags$strong(rv$current_coder %||% "default"),
        "'s codings. Other coders' work is hidden."
      )
    })

    # ── Code selector ──────────────────────────────────────────────────────────

    shiny::observe({
      codes   <- codes_rv()
      active  <- codes[codes$deprecated == 0L, ]
      pending <- shiny::isolate(lv$pending_sel)
      if (!is.null(pending)) lv$pending_sel <- NULL
      shiny::updateSelectInput(session, "sel_code",
        choices  = stats::setNames(active$id, active$name),
        selected = pending %||% character(0))
    })

    # ── New-code quick-add ─────────────────────────────────────────────────────

    .random_code_color <- function() {
      h <- stats::runif(1L, 0, 360)
      s <- stats::runif(1L, 0.55, 0.80)
      l <- stats::runif(1L, 0.42, 0.62)
      c_val <- (1 - abs(2 * l - 1)) * s
      x     <- c_val * (1 - abs((h / 60) %% 2 - 1))
      m     <- l - c_val / 2
      if      (h < 60)  { r <- c_val; g <- x;     b <- 0     }
      else if (h < 120) { r <- x;     g <- c_val; b <- 0     }
      else if (h < 180) { r <- 0;     g <- c_val; b <- x     }
      else if (h < 240) { r <- 0;     g <- x;     b <- c_val }
      else if (h < 300) { r <- x;     g <- 0;     b <- c_val }
      else              { r <- c_val; g <- 0;     b <- x     }
      sprintf("#%02X%02X%02X",
        as.integer((r + m) * 255),
        as.integer((g + m) * 255),
        as.integer((b + m) * 255))
    }

    .code_name_examples <- c(
      "Empathy", "Resilience", "Trust", "Identity", "Agency",
      "Power dynamics", "Coping strategy", "Social support", "Belonging",
      "Ambiguity", "Resistance", "Vulnerability", "Collaboration",
      "Uncertainty", "Boundary-setting", "Sense of loss", "Legitimacy"
    )

    shiny::observeEvent(input$btn_new_code, {
      default_color <- .random_code_color()
      example_name  <- sample(.code_name_examples, 1L)
      shiny::showModal(shiny::modalDialog(
        title     = "New Code",
        size      = "s",
        easyClose = TRUE,
        shiny::textInput(ns("new_code_name"), "Name",
                         placeholder = paste0("e.g. ", example_name)),
        colourpicker::colourInput(ns("new_code_color"), "Colour",
                                  value = default_color, showColour = "both"),
        shiny::textAreaInput(ns("new_code_definition"), "Definition (optional)",
                             rows = 2, placeholder = "What this code means"),
        shiny::textAreaInput(ns("new_code_criteria"), "Criteria (optional)",
                             rows = 2, placeholder = "Inclusion / exclusion rules"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_create_code"), "Create",
                              class = "btn-success")
        )
      ))
    })

    shiny::observeEvent(input$btn_create_code, {
      nm <- trimws(input$new_code_name %||% "")
      shiny::req(nchar(nm) > 0)
      tryCatch({
        new_row <- qc_add_code(
          rv$project,
          name       = nm,
          color      = input$new_code_color %||% "#4E79A7",
          definition = input$new_code_definition %||% "",
          criteria   = input$new_code_criteria   %||% ""
        )
        lv$pending_sel   <- as.character(new_row$id)
        rv$refresh_codes <- rv$refresh_codes + 1L
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    shiny::observe({
      rv$refresh_codes
      cats   <- .query(rv$project$con,
        "SELECT id, name FROM code_categories WHERE status = 1 ORDER BY name")
      active_coder <- rv$current_coder %||% "default"
      coders <- unique(c(active_coder, qc_list_coders(rv$project)$coder))
      coders <- coders[!is.na(coders) & nzchar(coders)]
      selected_coder <- input$filter_display_coder %||% ""

      # isolate blind_mode so choices/selection changes from blind toggles are
      # handled exclusively by the observeEvent below — avoids feedback loops
      if (isTRUE(shiny::isolate(rv$blind_mode))) {
        coder_choices  <- stats::setNames(active_coder, active_coder)
        selected_coder <- active_coder
      } else {
        if (!selected_coder %in% c("", coders)) selected_coder <- ""
        coder_choices <- c("All coders" = "",
                           stats::setNames(coders, coders))
      }

      shiny::updateSelectizeInput(session, "filter_display_cats",
        choices = stats::setNames(cats$id, cats$name), server = TRUE)
      shiny::updateSelectInput(session, "filter_display_coder",
        choices = coder_choices,
        selected = selected_coder)
    })

    # Fires exactly once per blind mode toggle; resets the coder display filter
    # without the feedback loop that a general observe would create.
    shiny::observeEvent(rv$blind_mode, {
      active_coder <- shiny::isolate(rv$current_coder) %||% "default"
      coders <- tryCatch({
        x <- qc_list_coders(rv$project)$coder
        unique(c(active_coder, x[!is.na(x) & nzchar(x)]))
      }, error = function(e) active_coder)

      if (isTRUE(rv$blind_mode)) {
        shiny::updateSelectInput(session, "filter_display_coder",
          choices  = stats::setNames(active_coder, active_coder),
          selected = active_coder)
      } else {
        shiny::updateSelectInput(session, "filter_display_coder",
          choices  = c("All coders" = "", stats::setNames(coders, coders)),
          selected = "")
      }
    }, ignoreInit = TRUE)

    # ── Code info tooltip ──────────────────────────────────────────────────────

    output$code_info <- shiny::renderUI({
      shiny::req(input$sel_code)
      codes <- codes_rv()
      row   <- codes[codes$id == as.integer(input$sel_code), ]
      if (nrow(row) == 0L) return(NULL)
      def  <- row$definition[[1L]] %||% ""
      crit <- row$criteria[[1L]]   %||% ""
      if (nchar(def) == 0L && nchar(crit) == 0L) return(NULL)
      shiny::tags$details(
        class = "qc-panel-details",
        shiny::tags$summary(
          "Code reference"
        ),
        shiny::div(
          class = "qc-panel-details-body",
          shiny::div(
            class = "qc-code-info",
            if (nchar(def) > 0L) shiny::div(
              class = if (nchar(crit) > 0L) "mb-1" else NULL,
              shiny::tags$strong("Definition: "), def
            ),
            if (nchar(crit) > 0L) shiny::div(
              shiny::tags$strong("Criteria: "), crit
            )
          )
        )
      )
    })

    # ── Apply code (button + Enter hotkey) ─────────────────────────────────────

    .do_apply <- function() {
      shiny::req(input$selection, input$sel_code, rv$active_source_id)
      sel <- input$selection
      tryCatch({
        conf_raw <- input$confidence
        conf     <- if (!is.null(conf_raw) && nchar(conf_raw) > 0L)
                      as.integer(conf_raw) else NULL
        new_coding <- qc_add_coding(
          project    = rv$project,
          source_id  = rv$active_source_id,
          code_id    = as.integer(input$sel_code),
          selfirst   = as.integer(sel$start),
          selast     = as.integer(sel$end),
          memo       = input$seg_memo,
          coder      = rv$current_coder %||% "default",
          confidence = conf
        )
        lv$last_coding_id <- new_coding$id
        rv$refresh_codes  <- rv$refresh_codes + 1L
        shinyjs::reset("seg_memo")
        codes_now <- shiny::isolate(codes_rv())
        code_nm <- codes_now$name[codes_now$id == as.integer(input$sel_code)]
        code_nm <- if (length(code_nm) > 0L) code_nm[[1L]] else "code"
        shiny::showNotification(
          shiny::tagList(
            shiny::span(paste0('"', code_nm, '" applied')),
            shiny::actionLink(ns("undo_last_apply"), " Undo",
              style = "color:inherit;text-decoration:underline;cursor:pointer;margin-left:8px;")
          ),
          duration = 4,
          type     = "message",
          id       = ns("apply_toast")
        )
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    }

    shiny::observeEvent(input$btn_apply,    .do_apply())
    shiny::observeEvent(input$hotkey_apply, .do_apply())

    shiny::observeEvent(input$undo_last_apply, {
      shiny::req(!is.null(lv$last_coding_id))
      tryCatch({
        qc_delete_coding(rv$project, lv$last_coding_id)
        rv$refresh_codes  <- rv$refresh_codes + 1L
        lv$last_coding_id <- NULL
        shiny::removeNotification(ns("apply_toast"))
        shiny::showNotification("Coding removed.", type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Digit hotkey: select Nth code and apply immediately ────────────────────

    shiny::observeEvent(input$hotkey_digit, {
      shiny::req(input$selection, rv$active_source_id)
      codes  <- codes_rv()
      active <- codes[codes$deprecated == 0L, ]
      idx    <- input$hotkey_digit$digit
      if (idx < 1L || idx > nrow(active)) return()
      shiny::updateSelectInput(session, "sel_code",
        selected = as.character(active$id[[idx]]))
      # Apply without waiting for the selectInput to re-render
      tryCatch({
        sel <- input$selection
        qc_add_coding(
          project   = rv$project,
          source_id = rv$active_source_id,
          code_id   = active$id[[idx]],
          selfirst  = as.integer(sel$start),
          selast    = as.integer(sel$end),
          coder     = rv$current_coder %||% "default"
        )
        rv$refresh_codes <- rv$refresh_codes + 1L
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Escape: clear selection display ────────────────────────────────────────

    shiny::observeEvent(input$hotkey_escape, {
      output$sel_preview <- shiny::renderUI({
        shiny::div(
          class = "qc-selection-preview",
          shiny::span("No text selected", style = "color:var(--sat-text-muted);font-style:normal;")
        )
      })
    })

    # ── Navigation: build uncoded targets when document / codings change ───────

    shiny::observe({
      shiny::req(rv$active_source_id)
      rv$refresh_codes
      tryCatch({
        lv$nav_targets <- qc_uncoded_segments(rv$project, rv$active_source_id)
        lv$nav_cursor  <- 0L
      }, error = function(e) NULL)
    })

    .nav_to <- function(pos) {
      session$sendCustomMessage("qc_scroll_to", list(pos = as.integer(pos)))
    }

    .nav_next <- function() {
      targets <- lv$nav_targets
      if (is.null(targets) || nrow(targets) == 0L) {
        shiny::showNotification("No uncoded segments found.", type = "message")
        return()
      }
      cursor         <- (lv$nav_cursor %% nrow(targets)) + 1L
      lv$nav_cursor  <- cursor
      .nav_to(targets$start[[cursor]])
    }

    .nav_prev <- function() {
      targets <- lv$nav_targets
      if (is.null(targets) || nrow(targets) == 0L) return()
      n_tgts        <- nrow(targets)
      cursor        <- if (lv$nav_cursor <= 1L) n_tgts else lv$nav_cursor - 1L
      lv$nav_cursor <- cursor
      .nav_to(targets$start[[cursor]])
    }

    .nav_disputed <- function() {
      shiny::req(rv$active_source_id)
      tryCatch({
        disputed <- qc_disputed_segments(rv$project, rv$active_source_id)
        if (nrow(disputed) == 0L) {
          shiny::showNotification(
            "No disputed or draft segments found.", type = "message")
          return()
        }
        .nav_to(disputed$selfirst[[1L]])
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    }

    shiny::observeEvent(input$btn_nav_next,      .nav_next())
    shiny::observeEvent(input$hotkey_nav_next,   .nav_next())
    shiny::observeEvent(input$btn_nav_prev,      .nav_prev())
    shiny::observeEvent(input$hotkey_nav_prev,   .nav_prev())
    shiny::observeEvent(input$btn_nav_disputed,  .nav_disputed())
    shiny::observeEvent(input$hotkey_nav_disputed, .nav_disputed())

    # ── Display filter reactive updates ────────────────────────────────────────

    shiny::observeEvent(input$highlight_opacity, {
      lv$highlight_op <- input$highlight_opacity
    })
    shiny::observeEvent(input$cb_mode, {
      lv$cb_mode          <- isTRUE(input$cb_mode)
      rv$colorblind_mode  <- isTRUE(input$cb_mode)
    })

    shiny::observeEvent(rv$colorblind_mode, {
      lv$cb_mode <- isTRUE(rv$colorblind_mode)
      shiny::updateCheckboxInput(session, "cb_mode",
                                 value = isTRUE(rv$colorblind_mode))
    }, ignoreInit = TRUE)

    # ── Keyboard shortcuts help modal ──────────────────────────────────────────

    .show_shortcuts <- function() {
      shiny::showModal(shiny::modalDialog(
        title     = "Keyboard Shortcuts",
        easyClose = TRUE,
        footer    = shiny::modalButton("Close"),
        size      = "m",
        shiny::tags$dl(
          shiny::tags$dt(shiny::tags$kbd("1"), "–", shiny::tags$kbd("9")),
          shiny::tags$dd("Select and apply the Nth code to selected text"),
          shiny::tags$dt(shiny::tags$kbd("Enter")),
          shiny::tags$dd("Apply the current code to the selected text"),
          shiny::tags$dt(shiny::tags$kbd("Esc")),
          shiny::tags$dd("Clear the current text selection"),
          shiny::tags$dt(shiny::tags$kbd("/")),
          shiny::tags$dd("Focus the code selector"),
          shiny::tags$dt(shiny::tags$kbd("n")),
          shiny::tags$dd("Jump to the next uncoded segment"),
          shiny::tags$dt(shiny::tags$kbd("p")),
          shiny::tags$dd("Jump to the previous uncoded segment"),
          shiny::tags$dt(shiny::tags$kbd("d")),
          shiny::tags$dd("Jump to the next disputed / draft segment"),
          shiny::tags$dt(shiny::tags$kbd("?")),
          shiny::tags$dd("Show this help")
        )
      ))
    }

    shiny::observeEvent(input$btn_shortcuts,  .show_shortcuts())
    shiny::observeEvent(input$hotkey_help,    .show_shortcuts())

    # ── Click-to-edit existing coding ──────────────────────────────────────────

    .show_edit_modal <- function(row) {
      lv$editing_coding <- row
      active <- codes_rv()[codes_rv()$deprecated == 0L, ]
      conf_val <- if (!is.null(row$confidence) && !is.na(row$confidence))
        as.character(row$confidence) else ""
      shiny::showModal(shiny::modalDialog(
        title     = "Edit coding",
        size      = "m",
        easyClose = TRUE,
        shiny::div(
          class = "qc-selection-preview mb-3",
          style = paste0("border-left-color:", row$code_color, ";"),
          row$seltext
        ),
        shiny::selectInput(ns("edit_code_id"), "Code",
          choices  = stats::setNames(active$id, active$name),
          selected = as.character(row$code_id)),
        shiny::selectInput(ns("edit_coding_status"), "Status",
          choices  = c("Validated" = "validated", "Draft" = "draft"),
          selected = row$coding_status %||% "validated"),
        shiny::selectInput(ns("edit_confidence"), "Confidence",
          choices  = c("Unrated" = "", "Low (25)" = "25", "Medium (50)" = "50",
                       "High (75)" = "75", "Certain (100)" = "100"),
          selected = conf_val),
        shiny::textAreaInput(ns("edit_memo"), "Memo",
          value = row$memo %||% "", rows = 2),
        footer = shiny::tagList(
          shiny::actionButton(ns("btn_delete_coding_edit"), "Delete",
            class = "btn-outline-danger me-auto"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_save_coding_edit"), "Save",
            class = "btn-primary")
        )
      ))
    }

    shiny::observeEvent(input$clicked_coding, {
      ids      <- as.integer(input$clicked_coding$coding_ids)
      relevant <- codings_rv()[codings_rv()$id %in% ids, ]
      if (nrow(relevant) == 0L) return()
      if (nrow(relevant) == 1L) {
        .show_edit_modal(relevant[1L, ])
      } else {
        # Disambiguation: multiple codings overlap at the clicked point
        choices <- stats::setNames(
          relevant$id,
          paste0(relevant$code_name, ' — "',
                 substr(relevant$seltext, 1L, 50L), '"')
        )
        lv$disambiguation <- relevant
        shiny::showModal(shiny::modalDialog(
          title     = "Multiple codings here",
          size      = "s",
          easyClose = TRUE,
          shiny::radioButtons(ns("disambig_sel"), "Select coding to edit:",
            choices = choices, selected = relevant$id[[1L]]),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("btn_disambig_pick"), "Edit",
              class = "btn-outline-primary")
          )
        ))
      }
    })

    shiny::observeEvent(input$btn_disambig_pick, {
      shiny::req(lv$disambiguation, input$disambig_sel)
      sel_id <- as.integer(input$disambig_sel)
      row    <- lv$disambiguation[lv$disambiguation$id == sel_id, ]
      shiny::removeModal()
      if (nrow(row) > 0L) .show_edit_modal(row[1L, ])
    })

    shiny::observeEvent(input$btn_save_coding_edit, {
      shiny::req(lv$editing_coding)
      row <- lv$editing_coding
      cid <- row$id
      tryCatch({
        new_code_id <- as.integer(input$edit_code_id)
        new_status  <- input$edit_coding_status %||% "validated"
        new_memo    <- input$edit_memo %||% ""
        new_conf_raw <- input$edit_confidence %||% ""
        new_conf    <- if (nchar(new_conf_raw) > 0L) as.integer(new_conf_raw) else NULL

        if (new_code_id != row$code_id)
          qc_reassign_coding(rv$project, cid, new_code_id)
        if (new_memo != (row$memo %||% ""))
          qc_update_coding_memo(rv$project, cid, new_memo)
        if (!identical(new_conf, if (!is.na(row$confidence)) row$confidence else NULL))
          qc_update_coding_confidence(rv$project, cid, new_conf)
        if (new_status != (row$coding_status %||% "validated"))
          .exec(rv$project$con,
            "UPDATE codings SET coding_status = ? WHERE id = ? AND status = 1",
            list(new_status, cid))

        rv$refresh_codes  <- rv$refresh_codes + 1L
        lv$editing_coding <- NULL
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    shiny::observeEvent(input$btn_delete_coding_edit, {
      shiny::req(lv$editing_coding)
      cid <- lv$editing_coding$id
      tryCatch({
        qc_delete_coding(rv$project, cid)
        rv$refresh_codes  <- rv$refresh_codes + 1L
        lv$editing_coding <- NULL
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Excerpt creation ────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_create_excerpt, {
      sel <- input$selection
      shiny::req(sel, rv$active_source_id)
      if (is.null(sel$start) || is.null(sel$end)) {
        shiny::showNotification("Select text first.", type = "warning")
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title     = "Create Excerpt",
        size      = "s",
        easyClose = TRUE,
        shiny::div(
          class = "qc-selection-preview mb-2",
          if (nchar(sel$text %||% "") > 0L) sel$text
          else shiny::span("(selected passage)", style = "color:var(--sat-text-muted);")
        ),
        shiny::textAreaInput(ns("excerpt_memo_input"), "Memo (optional)",
          rows = 3, placeholder = "Why is this passage notable?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_save_excerpt"), "Save Excerpt",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$btn_save_excerpt, {
      sel <- input$selection
      shiny::req(sel, rv$active_source_id)
      tryCatch({
        qc_add_excerpt(
          rv$project,
          source_id = rv$active_source_id,
          selfirst  = as.integer(sel$start),
          selast    = as.integer(sel$end),
          memo      = input$excerpt_memo_input %||% "",
          coder     = rv$current_coder %||% "default"
        )
        rv$refresh_codes <- rv$refresh_codes + 1L
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Excerpts table ─────────────────────────────────────────────────────────

    output$tbl_excerpts <- DT::renderDataTable({
      shiny::req(rv$active_source_id)
      df <- excerpts_rv()
      if (nrow(df) == 0L) {
        return(DT::datatable(
          tibble::tibble(message = "No excerpts yet."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      DT::datatable(
        dplyr::select(df, seltext, memo),
        class     = "table table-hover table-sm",
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength = 5, dom = "tp",
          columnDefs = list(
            list(targets = 0, className = "dt-truncate"),
            list(targets = 1, className = "dt-muted dt-truncate")
          )
        ),
        colnames = c("Passage", "Memo")
      )
    })

    shiny::observeEvent(input$tbl_excerpts_rows_selected, {
      row <- input$tbl_excerpts_rows_selected
      shiny::req(row)
      exc <- excerpts_rv()
      shiny::showModal(shiny::modalDialog(
        title     = "Excerpt",
        size      = "s",
        easyClose = TRUE,
        shiny::div(
          class = "qc-selection-preview mb-2",
          exc$seltext[[row]]
        ),
        shiny::textAreaInput(ns("edit_excerpt_memo"), "Memo",
          value = exc$memo[[row]] %||% "", rows = 3),
        footer = shiny::tagList(
          shiny::actionButton(ns("btn_delete_excerpt"), "Delete",
            class = "btn-outline-danger me-auto"),
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_save_excerpt_memo"), "Save",
            class = "btn-primary")
        )
      ))
      lv$editing_excerpt_id <- exc$id[[row]]
    })

    shiny::observeEvent(input$btn_save_excerpt_memo, {
      shiny::req(lv$editing_excerpt_id)
      tryCatch({
        qc_update_excerpt_memo(rv$project, lv$editing_excerpt_id,
                               input$edit_excerpt_memo %||% "")
        rv$refresh_codes <- rv$refresh_codes + 1L
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    shiny::observeEvent(input$btn_delete_excerpt, {
      shiny::req(lv$editing_excerpt_id)
      tryCatch({
        qc_delete_excerpt(rv$project, lv$editing_excerpt_id)
        rv$refresh_codes <- rv$refresh_codes + 1L
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Codings table ──────────────────────────────────────────────────────────

    output$tbl_codings <- DT::renderDataTable({
      shiny::req(rv$active_source_id)
      DT::datatable(
        dplyr::select(codings_rv(), code_name, seltext, memo),
        class     = "table table-hover table-sm",
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength = 10, dom = "tp",
          columnDefs = list(
            list(targets = 0, width = "110px"),
            list(targets = 1, className = "dt-truncate"),
            list(targets = 2, className = "dt-muted dt-truncate")
          )
        ),
        colnames = c("Code", "Passage", "Memo")
      )
    })

    shiny::observeEvent(input$tbl_codings_rows_selected, {
      row <- input$tbl_codings_rows_selected
      shiny::req(row)
      shiny::showModal(shiny::modalDialog(
        title = "Delete coding?",
        paste0('Remove the coding for "',
               codings_rv()$code_name[[row]], '"?'),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_del"), "Delete",
                              class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_del, {
      row <- input$tbl_codings_rows_selected
      shiny::req(row)
      qc_delete_coding(rv$project, codings_rv()$id[[row]])
      rv$refresh_codes <- rv$refresh_codes + 1L
      shiny::removeModal()
    })
  })
}
