mod_themes_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 270,

      shiny::div(
        class = "d-flex justify-content-between align-items-center mb-2",
        shiny::h5("Themes", class = "mb-0"),
        shiny::actionButton(ns("btn_new_theme"), "New",
          class = "btn-sm btn-primary")
      ),
      qc_help_details(
        "Themes help",
        shiny::p(
          "Themes collect evidence into analytical claims. Link a theme to ",
          "categories or direct codes so supporting excerpts appear automatically."
        )
      ),
      DT::dataTableOutput(ns("tbl_themes"))
    ),

    shiny::uiOutput(ns("detail_ui"))
  )
}

mod_themes_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lv <- shiny::reactiveValues(
      selected_id    = NULL,
      refresh_themes = 0L
    )

    # ── Theme list ─────────────────────────────────────────────────────────────

    themes_rv <- shiny::reactive({
      rv$refresh_codes
      lv$refresh_themes
      qc_list_themes(rv$project)
    })

    output$tbl_themes <- DT::renderDataTable({
      df <- themes_rv()
      if (nrow(df) == 0L) {
        return(DT::datatable(
          tibble::tibble(message = "No themes yet — click New to create one."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      disp <- df[, c("id", "name", "n_categories", "n_codes")]
      DT::datatable(
        disp,
        class     = "table table-hover table-sm",
        selection = "single",
        rownames  = FALSE,
        colnames  = c("ID", "Theme", "Cats", "Codes"),
        options   = list(
          dom        = "t",
          pageLength = 100,
          columnDefs = list(
            list(width = "38px", targets = 0),
            list(width = "44px", targets = c(2, 3), className = "text-center")
          )
        )
      )
    })

    # ── Row selection ──────────────────────────────────────────────────────────

    shiny::observeEvent(input$tbl_themes_rows_selected, {
      row <- input$tbl_themes_rows_selected
      lv$selected_id <- if (!is.null(row) && length(row) > 0L)
        themes_rv()$id[[row]] else NULL
    })

    # ── Detail UI (re-renders on selection change or save) ─────────────────────

    output$detail_ui <- shiny::renderUI({
      lv$refresh_themes
      id_val <- lv$selected_id

      if (is.null(id_val)) {
        return(shiny::div(
          class = "p-4 text-muted",
          shiny::p(
            "Select a theme from the list to view and edit it, or click ",
            shiny::tags$strong("New"), " to create one."
          ),
          shiny::p(shiny::tags$small(
            "Themes are analytical patterns that address your research question. ",
            "Each theme integrates multiple codes or categories and should be ",
            "expressed as a proposition, not merely a topic label."
          ))
        ))
      }

      theme_data <- tryCatch(
        qc_get_theme(rv$project, id_val),
        error = function(e) NULL
      )
      if (is.null(theme_data)) {
        return(shiny::p(class = "p-3 text-danger", "Could not load theme."))
      }
      th <- theme_data$theme

      shiny::tagList(

        # ── Definition card ───────────────────────────────────────────────────
        bslib::card(
          class = "mb-3",
          bslib::card_header(
            shiny::div(
              class = "d-flex justify-content-between align-items-center w-100",
              shiny::span(paste0("Theme #", id_val, " — ", th$name)),
              shiny::div(
                class = "d-flex gap-2",
                shiny::downloadButton(ns("dl_theme_docx"), "Word",
                  class = "btn-sm btn-outline-secondary"),
                shiny::downloadButton(ns("dl_theme_html"), "HTML",
                  class = "btn-sm btn-outline-secondary"),
                shiny::downloadButton(ns("dl_theme_txt"), "Text",
                  class = "btn-sm btn-outline-secondary"),
                shiny::actionButton(ns("btn_save_theme"), "Save",
                  class = "btn-sm btn-primary"),
                shiny::actionButton(ns("btn_delete_theme"), "Delete",
                  class = "btn-sm btn-outline-danger")
              )
            )
          ),
          bslib::card_body(
            qc_help_note(
              "Write themes as claims about the data. Use the central concept ",
              "for the organizing idea, the analytical statement for the claim, ",
              "and scope conditions for boundaries."
            ),
            shiny::textInput(ns("edit_name"), "Name",
              value = th$name[[1L]] %||% ""),
            shiny::textInput(ns("edit_central_concept"), "Central concept",
              placeholder = "The core organizing idea",
              value = th$central_concept[[1L]] %||% ""),
            shiny::textAreaInput(ns("edit_narrative"), "Analytical statement",
              rows = 3,
              placeholder = paste0(
                "Write a proposition, e.g. ‘Participants navigate tension ",
                "between X and Y through…’"
              ),
              value = th$narrative[[1L]] %||% ""),
            shiny::textAreaInput(ns("edit_definition"), "Definition",
              rows = 2,
              placeholder = "What belongs in this theme?",
              value = th$definition[[1L]] %||% ""),
            shiny::textAreaInput(ns("edit_scope"), "Scope conditions",
              rows = 2,
              placeholder = "What is included / excluded?",
              value = th$scope[[1L]] %||% "")
          )
        ),

        # ── Structure card ────────────────────────────────────────────────────
        bslib::card(
          class = "mb-3",
          bslib::card_header(
            shiny::div(
              class = "d-flex justify-content-between align-items-center w-100",
              "Structure",
              shiny::actionButton(ns("btn_edit_links"), "Edit links",
                class = "btn-sm btn-outline-secondary")
            )
          ),
          bslib::card_body(
            shiny::uiOutput(ns("structure_ui"))
          )
        ),

        # ── Excerpts card ─────────────────────────────────────────────────────
        bslib::card(
          bslib::card_header(shiny::textOutput(ns("excerpts_header"))),
          bslib::card_body(
            class = "p-0",
            style = "max-height:480px;overflow-y:auto;",
            shiny::uiOutput(ns("excerpts_ui"))
          )
        )
      )
    })

    # ── New theme modal ────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_new_theme, {
      shiny::showModal(shiny::modalDialog(
        title     = "New Theme",
        size      = "m",
        easyClose = FALSE,

        shiny::textInput(ns("new_name"), "Name",
          placeholder = "Short label"),
        shiny::textInput(ns("new_central_concept"), "Central concept",
          placeholder = "The core organizing idea"),
        shiny::textAreaInput(ns("new_narrative"), "Analytical statement",
          rows = 3,
          placeholder = paste0(
            "Write a proposition, e.g. ‘Participants navigate tension ",
            "between X and Y through…’"
          )),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_new_theme"), "Create",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_new_theme, {
      nm <- trimws(input$new_name %||% "")
      if (nchar(nm) == 0L) {
        shiny::showNotification("Theme name is required.", type = "warning")
        return()
      }
      tryCatch({
        row <- qc_add_theme(
          rv$project,
          name            = nm,
          central_concept = trimws(input$new_central_concept %||% ""),
          narrative       = trimws(input$new_narrative       %||% ""),
          created_by      = rv$current_coder %||% Sys.info()[["user"]]
        )
        lv$selected_id    <- row$id
        lv$refresh_themes <- lv$refresh_themes + 1L
        shiny::removeModal()
        shiny::showNotification(paste0("Theme '", nm, "' created."), type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Save theme ─────────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_save_theme, {
      shiny::req(lv$selected_id)
      nm <- trimws(input$edit_name %||% "")
      if (nchar(nm) == 0L) {
        shiny::showNotification("Theme name is required.", type = "warning")
        return()
      }
      tryCatch({
        qc_update_theme(
          rv$project,
          id              = lv$selected_id,
          name            = nm,
          central_concept = trimws(input$edit_central_concept %||% ""),
          narrative       = trimws(input$edit_narrative       %||% ""),
          definition      = trimws(input$edit_definition      %||% ""),
          scope           = trimws(input$edit_scope           %||% ""),
          changed_by      = rv$current_coder %||% Sys.info()[["user"]]
        )
        lv$refresh_themes <- lv$refresh_themes + 1L
        shiny::showNotification("Theme saved.", type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Delete theme ───────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_delete_theme, {
      shiny::req(lv$selected_id)
      th <- themes_rv()
      nm <- th$name[th$id == lv$selected_id]
      shiny::showModal(shiny::modalDialog(
        title     = "Delete theme?",
        size      = "s",
        easyClose = TRUE,
        shiny::p(paste0('Remove "', nm[[1L]], '"? This cannot be undone.')),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_delete_theme"), "Delete",
            class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_delete_theme, {
      shiny::req(lv$selected_id)
      tryCatch({
        qc_delete_theme(rv$project, lv$selected_id)
        lv$selected_id    <- NULL
        lv$refresh_themes <- lv$refresh_themes + 1L
        shiny::removeModal()
        shiny::showNotification("Theme deleted.", type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Theme exports ──────────────────────────────────────────────────────────

    output$dl_theme_docx <- shiny::downloadHandler(
      filename = function() {
        th <- tryCatch(
          qc_list_themes(rv$project),
          error = function(e) tibble::tibble(id = integer(), name = character())
        )
        nm <- if (!is.null(lv$selected_id) && lv$selected_id %in% th$id)
          gsub("[^A-Za-z0-9_-]", "_", th$name[th$id == lv$selected_id][[1L]])
        else "theme"
        paste0(nm, "_", Sys.Date(), ".docx")
      },
      content = function(file) {
        shiny::req(lv$selected_id)
        tryCatch({
          tmp <- qc_export_themes_report(
            rv$project,
            format    = "docx",
            theme_ids = lv$selected_id,
            include_excerpts  = TRUE,
            include_narrative = TRUE
          )
          file.copy(tmp, file)
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      }
    )

    output$dl_theme_html <- shiny::downloadHandler(
      filename = function() {
        th <- tryCatch(
          qc_list_themes(rv$project),
          error = function(e) tibble::tibble(id = integer(), name = character())
        )
        nm <- if (!is.null(lv$selected_id) && lv$selected_id %in% th$id)
          gsub("[^A-Za-z0-9_-]", "_", th$name[th$id == lv$selected_id][[1L]])
        else "theme"
        paste0(nm, "_", Sys.Date(), ".html")
      },
      content = function(file) {
        shiny::req(lv$selected_id)
        tryCatch({
          tmp <- qc_export_themes_report(
            rv$project,
            format    = "html",
            theme_ids = lv$selected_id,
            include_excerpts  = TRUE,
            include_narrative = TRUE
          )
          file.copy(tmp, file)
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      }
    )

    output$dl_theme_txt <- shiny::downloadHandler(
      filename = function() {
        th <- tryCatch(
          qc_list_themes(rv$project),
          error = function(e) tibble::tibble(id = integer(), name = character())
        )
        nm <- if (!is.null(lv$selected_id) && lv$selected_id %in% th$id)
          gsub("[^A-Za-z0-9_-]", "_", th$name[th$id == lv$selected_id][[1L]])
        else "theme"
        paste0(nm, "_", Sys.Date(), ".txt")
      },
      content = function(file) {
        shiny::req(lv$selected_id)
        tryCatch({
          tmp <- qc_export_themes_report(
            rv$project,
            format    = "txt",
            theme_ids = lv$selected_id,
            include_excerpts  = TRUE,
            include_narrative = TRUE
          )
          file.copy(tmp, file)
        }, error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
        })
      }
    )

    # ── Edit links modal ───────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_edit_links, {
      shiny::req(lv$selected_id)

      cats_raw  <- tryCatch(qc_list_categories(rv$project),
                            error = function(e) tibble::tibble())
      all_codes <- tryCatch(qc_list_codes(rv$project),
                            error = function(e) tibble::tibble())
      theme_data <- tryCatch(qc_get_theme(rv$project, lv$selected_id),
                             error = function(e) NULL)
      shiny::req(!is.null(theme_data))

      cat_choices <- if (nrow(cats_raw) > 0L) {
        uc <- cats_raw[!duplicated(cats_raw$category_id),
                       c("category_id", "category_name")]
        stats::setNames(uc$category_id, uc$category_name)
      } else character(0L)

      code_choices <- if (nrow(all_codes) > 0L)
        stats::setNames(all_codes$id, all_codes$name)
      else character(0L)

      shiny::showModal(shiny::modalDialog(
        title     = "Edit Theme Links",
        size      = "l",
        easyClose = FALSE,

        shiny::h6("Categories"),
        shiny::tags$small(class = "text-muted d-block mb-2",
          "All codes in a linked category contribute to this theme's excerpts."),
        shiny::selectizeInput(ns("links_cat_ids"), NULL,
          choices  = cat_choices,
          selected = theme_data$linked_cats$id,
          multiple = TRUE,
          options  = list(placeholder = "Select categories…")),

        shiny::hr(),

        shiny::h6("Direct code links"),
        shiny::tags$small(class = "text-muted d-block mb-2",
          "Individual codes linked directly, independent of any category."),
        shiny::selectizeInput(ns("links_code_ids"), NULL,
          choices  = code_choices,
          selected = theme_data$linked_codes$id,
          multiple = TRUE,
          options  = list(placeholder = "Select codes…")),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_links"), "Save links",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_links, {
      shiny::req(lv$selected_id)

      new_cat_ids  <- if (length(input$links_cat_ids)  > 0L)
        as.integer(input$links_cat_ids)  else integer(0L)
      new_code_ids <- if (length(input$links_code_ids) > 0L)
        as.integer(input$links_code_ids) else integer(0L)

      tryCatch({
        theme_data   <- qc_get_theme(rv$project, lv$selected_id)
        old_cat_ids  <- theme_data$linked_cats$id
        old_code_ids <- theme_data$linked_codes$id

        to_add_cats <- new_cat_ids[!new_cat_ids %in% old_cat_ids]
        to_rm_cats  <- old_cat_ids[!old_cat_ids %in% new_cat_ids]
        if (length(to_add_cats) > 0L)
          qc_link_theme_categories(rv$project, lv$selected_id, to_add_cats)
        for (cid in to_rm_cats)
          qc_unlink_theme_category(rv$project, lv$selected_id, cid)

        to_add_codes <- new_code_ids[!new_code_ids %in% old_code_ids]
        to_rm_codes  <- old_code_ids[!old_code_ids %in% new_code_ids]
        if (length(to_add_codes) > 0L)
          qc_link_theme_codes(rv$project, lv$selected_id, to_add_codes)
        for (cid in to_rm_codes)
          qc_unlink_theme_code(rv$project, lv$selected_id, cid)

        lv$refresh_themes <- lv$refresh_themes + 1L
        shiny::removeModal()
        shiny::showNotification("Links updated.", type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Structure UI ───────────────────────────────────────────────────────────

    output$structure_ui <- shiny::renderUI({
      lv$refresh_themes
      id_val <- lv$selected_id
      if (is.null(id_val)) return(NULL)

      theme_data <- tryCatch(qc_get_theme(rv$project, id_val),
                             error = function(e) NULL)
      if (is.null(theme_data))
        return(shiny::p(class = "text-muted", "Could not load structure."))

      cats  <- theme_data$linked_cats
      codes <- theme_data$linked_codes

      if (nrow(cats) == 0L && nrow(codes) == 0L) {
        return(shiny::p(class = "text-muted",
          "No structure yet — click ",
          shiny::tags$strong("Edit links"), " to add categories or codes."))
      }

      cats_raw <- if (nrow(cats) > 0L) {
        tryCatch(qc_list_categories(rv$project),
                 error = function(e) tibble::tibble())
      } else tibble::tibble()

      lbl_style <- "font-size:.7rem;text-transform:uppercase;letter-spacing:.05em;"
      parts     <- list()

      if (nrow(cats) > 0L) {
        parts <- c(parts, list(
          shiny::h6(class = "text-muted mb-2", style = lbl_style, "Categories")
        ))
        for (i in seq_len(nrow(cats))) {
          cat_row   <- cats[i, ]
          cat_codes <- if (nrow(cats_raw) > 0L)
            cats_raw[!is.na(cats_raw$code_id) &
                     cats_raw$category_id == cat_row$id, ]
          else tibble::tibble()

          code_nodes <- if (nrow(cat_codes) > 0L) {
            lapply(seq_len(nrow(cat_codes)), function(j) {
              r   <- cat_codes[j, ]
              col <- r$code_color %||% "#aaa"
              shiny::div(
                class = "d-flex align-items-center gap-2 ms-3 mt-1",
                shiny::tags$span(style = paste0(
                  "width:8px;height:8px;border-radius:50%;flex-shrink:0;",
                  "display:inline-block;background:", col, ";"
                )),
                shiny::tags$small(r$code_name)
              )
            })
          } else list()

          parts <- c(parts, list(
            shiny::div(
              class = "mb-2",
              shiny::div(
                class = "d-flex align-items-center gap-2",
                shiny::tags$small(shiny::icon("folder-open", class = "text-muted")),
                shiny::tags$span(cat_row$name, style = "font-size:.9rem;font-weight:600;")
              ),
              do.call(shiny::tagList, code_nodes)
            )
          ))
        }
      }

      if (nrow(codes) > 0L) {
        if (nrow(cats) > 0L) parts <- c(parts, list(shiny::hr(class = "my-2")))
        parts <- c(parts, list(
          shiny::h6(class = "text-muted mb-2", style = lbl_style, "Direct codes")
        ))
        for (i in seq_len(nrow(codes))) {
          r   <- codes[i, ]
          col <- r$color %||% "#4E79A7"
          parts <- c(parts, list(
            shiny::div(
              class = "d-flex align-items-center gap-2 mb-1",
              shiny::tags$span(style = paste0(
                "width:10px;height:10px;border-radius:50%;flex-shrink:0;",
                "display:inline-block;background:", col, ";"
              )),
              shiny::tags$span(r$name, style = "font-size:.9rem;"),
              shiny::tags$small(class = "text-muted ms-auto",
                paste0(r$n_codings, " coding", if (r$n_codings != 1L) "s" else ""))
            )
          ))
        }
      }

      do.call(shiny::tagList, parts)
    })

    # ── Excerpts ───────────────────────────────────────────────────────────────

    output$excerpts_header <- shiny::renderText({
      lv$refresh_themes
      id_val <- lv$selected_id
      if (is.null(id_val)) return("Excerpts")
      n <- tryCatch(nrow(qc_theme_excerpts(rv$project, id_val)),
                    error = function(e) 0L)
      paste0("Excerpts (", n, ")")
    })

    output$excerpts_ui <- shiny::renderUI({
      lv$refresh_themes
      id_val <- lv$selected_id
      if (is.null(id_val)) return(NULL)

      excerpts <- tryCatch(
        qc_theme_excerpts(rv$project, id_val),
        error = function(e) tibble::tibble()
      )

      if (nrow(excerpts) == 0L) {
        return(shiny::p(class = "text-muted p-3",
          "No excerpts — link categories or codes to populate this view."))
      }

      doc_names <- unique(excerpts$doc_name)

      shiny::div(
        class = "p-2",
        lapply(doc_names, function(dn) {
          rows <- excerpts[excerpts$doc_name == dn, ]
          shiny::div(
            class = "mb-3",
            shiny::div(
              class = "fw-semibold text-muted mb-1",
              style = "font-size:.75rem;text-transform:uppercase;letter-spacing:.05em;",
              shiny::icon("file-alt", class = "me-1"), dn
            ),
            lapply(seq_len(nrow(rows)), function(i) {
              r   <- rows[i, ]
              col <- r$code_color %||% "#4E79A7"
              shiny::div(
                class = "mb-2 ps-2",
                style = paste0("border-left:3px solid ", col, ";"),
                shiny::div(
                  class = "d-flex justify-content-between mb-1",
                  shiny::tags$small(
                    class = "text-uppercase text-muted fw-semibold",
                    style = "font-size:.65rem;letter-spacing:.05em;",
                    r$code_name
                  ),
                  if (!is.na(r$coder) && nzchar(r$coder %||% ""))
                    shiny::tags$small(class = "text-muted",
                      style = "font-size:.7rem;", r$coder)
                ),
                shiny::tags$em(
                  style = "font-size:.875rem;",
                  paste0(
                    "“",
                    substr(r$seltext, 1L, 300L),
                    if (nchar(r$seltext) > 300L) "…" else "",
                    "”"
                  )
                )
              )
            })
          )
        })
      )
    })
  })
}
