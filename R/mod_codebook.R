mod_codebook_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),

    # -- Left: add / edit form --------------------------------------------
    bslib::card(
      bslib::card_header(shiny::textOutput(ns("form_header"))),
      shiny::div(
        class = "p-2",
        qc_help_details(
          "Codebook form help",
          shiny::p(
            "A useful code has a clear name, a definition, and criteria that ",
            "separate matching passages from near misses."
          ),
          qc_help_list(c(
            "Use categories to group codes for filtering and theme building.",
            "Optional weights can store direction, intensity, or valence when that is meaningful.",
            "Editing a code keeps history so changes can be audited."
          ))
        ),
        shiny::textInput(ns("code_name"),  "Name"),
        shiny::uiOutput(ns("name_validation")),
        colourpicker::colourInput(ns("code_color"), "Colour",
                                  value = "#4E79A7", showColour = "both"),
        shiny::textAreaInput(ns("code_definition"), "Definition", rows = 3),
        shiny::textAreaInput(ns("code_criteria"),   "Criteria (include/exclude)", rows = 3),
        shiny::textAreaInput(ns("code_memo"), "Memo", rows = 2),

        # -- Weight (optional) --------------------------------------------
        shiny::tags$details(
          class = "qc-panel-details",
          shiny::tags$summary(
            shiny::textOutput(ns("weight_summary"), inline = TRUE)
          ),
          shiny::div(
            class = "qc-panel-details-body",
            shiny::checkboxInput(ns("weight_enabled"), "Assign a weight to this code",
              value = FALSE),
            shiny::uiOutput(ns("weight_controls"))
          )
        ),

        shiny::uiOutput(ns("action_buttons")),
        shiny::hr(),
        shiny::h6("Categories"),
        shiny::textInput(ns("cat_name"), "New category name"),
        shiny::actionButton(ns("btn_add_cat"), "Add Category",
                            class = "btn-outline-primary w-100"),
        shiny::hr(),
        shiny::h6("Assign code to category"),
        shiny::selectInput(ns("sel_code_assign"), "Code",   choices = NULL),
        shiny::selectInput(ns("sel_cat_assign"),  "Category", choices = NULL),
        shiny::actionButton(ns("btn_assign"), "Assign",
                            class = "btn-outline-secondary w-100")
      )
    ),

    # -- Right: codes table -----------------------------------------------
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center w-100",
          "Codebook",
          shiny::div(
            class = "d-flex gap-2 align-items-center",
            shiny::actionButton(ns("btn_new_code_hdr"),
                                shiny::icon("plus"),
                                class = "btn-sm btn-outline-success",
                                title = "New code \u2014 clear current selection"),
            shiny::actionButton(ns("btn_validate"), "Validate",
                                class = "btn-sm btn-outline-info"),
            shiny::actionButton(ns("btn_snapshots"), "Snapshots",
                                class = "btn-sm btn-outline-secondary"),
            shiny::actionButton(ns("btn_import"), "Import",
                                class = "btn-sm btn-outline-secondary"),
            shiny::selectInput(ns("export_format"), label = NULL,
                               choices = c("CSV" = "csv", "JSON" = "json"),
                               width = "80px"),
            shiny::downloadButton(ns("dl_export"), "Export",
                                  class = "btn-sm btn-outline-secondary")
          )
        )
      ),
      qc_help_note(
        "Click a row to edit it. Double-click to deselect. Use History or ",
        "Review Codings after selecting a code to inspect its change trail or ",
        "assigned passages."
      ),
      DT::dataTableOutput(ns("tbl_codes"))
    )
  )
}

mod_codebook_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tbl_codes_proxy <- DT::dataTableProxy("tbl_codes")

    lv <- shiny::reactiveValues(
      selected_id   = NULL,
      selected_name = NULL,
      review_segs   = NULL,
      name_error    = NULL
    )

    # -- Data reactives ----------------------------------------------------

    codes <- shiny::reactive({
      rv$refresh_codes
      qc_list_codes(rv$project)
    })

    cats <- shiny::reactive({
      rv$refresh_codes
      .query(rv$project$con,
             "SELECT id, name FROM code_categories
              WHERE  status = 1 ORDER BY name")
    })

    # -- Weight controls ---------------------------------------------------

    output$weight_controls <- shiny::renderUI({
      if (!isTRUE(input$weight_enabled)) return(NULL)
      shiny::tagList(
        shiny::sliderInput(ns("code_weight"), "Weight",
          min = -1, max = 1, value = 0, step = 0.1, ticks = FALSE),
        shiny::div(
          style = paste0(
            "font-size:0.78rem;color:var(--sat-text-muted);",
            "display:flex;justify-content:space-between;",
            "margin-top:-10px;margin-bottom:8px;"),
          shiny::span("\u22121  strongly negative"),
          shiny::span("0  neutral"),
          shiny::span("+1  strongly positive")
        ),
        shiny::textInput(ns("code_weight_desc"),
          label       = "What does this weight mean for this code?",
          placeholder = "e.g. indicates emotional valence of the passage"
        )
      )
    })

    output$weight_summary <- shiny::renderText({
      if (!isTRUE(input$weight_enabled)) return("Weight (optional)")
      wt <- input$code_weight
      if (is.null(wt)) return("Weight (optional)")
      paste0("Weight: ", wt)
    })

    output$name_validation <- shiny::renderUI({
      msg <- lv$name_error
      if (is.null(msg)) return(NULL)
      shiny::div(
        class = "text-danger",
        style = "font-size:0.8rem;margin-top:-6px;margin-bottom:6px;",
        msg
      )
    })

    shiny::observeEvent(input$code_name, {
      lv$name_error <- NULL
    }, ignoreNULL = FALSE)

    # -- Form header -------------------------------------------------------

    output$form_header <- shiny::renderText({
      if (is.null(lv$selected_id)) "Add Code"
      else paste0("Edit: ", lv$selected_name)
    })

    # -- Context-aware action buttons --------------------------------------

    output$action_buttons <- shiny::renderUI({
      if (is.null(lv$selected_id)) {
        shiny::actionButton(ns("btn_add_code"), "Add Code",
                            class = "btn-success w-100")
      } else {
        shiny::tagList(
          shiny::div(
            class = "d-flex gap-2 mb-2",
            shiny::actionButton(ns("btn_save_code"), "Save",
                                class = "btn-primary flex-grow-1"),
            shiny::actionButton(ns("btn_cancel_edit"), "Cancel",
                                class = "btn-outline-secondary")
          ),
          shiny::div(
            class = "d-flex gap-2 mb-2",
            shiny::actionButton(ns("btn_view_history"), "History",
                                class = "btn-outline-info flex-grow-1"),
            shiny::actionButton(ns("btn_review_codings"), "Review Codings",
                                class = "btn-outline-info flex-grow-1")
          ),
          shiny::div(
            class = "d-flex gap-2 mb-2",
            shiny::actionButton(ns("btn_merge"), "Merge into\u2026",
                                class = "btn-outline-warning flex-grow-1"),
            shiny::actionButton(ns("btn_split"), "Split\u2026",
                                class = "btn-outline-warning flex-grow-1")
          ),
          shiny::actionButton(ns("btn_delete_code"), "Delete Code",
                              class = "btn-outline-danger w-100")
        )
      }
    })

    # -- Codes table -------------------------------------------------------

    output$tbl_codes <- DT::renderDataTable({
      all_codes <- codes()
      df <- dplyr::select(all_codes, id, name, color, weight, n_codings,
                          categories, definition, criteria, memo, parent_id)
      df$name  <- .indent_code_names(df$name, df$id, df$parent_id)
      df       <- dplyr::select(df, -parent_id)
      df$color <- sprintf(
        '<span class="qc-swatch" style="background:%s;" title="%s"></span>',
        df$color, df$color
      )
      DT::datatable(
        df,
        class     = "table table-hover",
        selection = "single",
        rownames  = FALSE,
        escape    = which(names(df) != "color"),
        options   = list(
          pageLength = 20, dom = "ftp",
          columnDefs = list(
            list(targets = 0, width = "50px"),
            list(targets = 2, width = "44px", className = "text-center"),
            list(targets = 3, width = "55px", className = "text-center text-muted"),
            list(targets = 4, width = "70px", className = "text-center"),
            list(targets = c(6, 7, 8), className = "dt-muted dt-truncate")
          )
        ),
        colnames = c("ID", "Name", "Color", "Weight", "Codings",
                     "Categories", "Definition", "Criteria", "Memo")
      )
    })

    # -- Row click -> enter edit mode ---------------------------------------

    shiny::observeEvent(input$tbl_codes_rows_selected, {
      row <- input$tbl_codes_rows_selected
      if (length(row) == 0L) {
        .reset_form(session)
        lv$selected_id   <- NULL
        lv$selected_name <- NULL
        return()
      }
      d <- codes()
      lv$selected_id   <- d$id[[row]]
      lv$selected_name <- d$name[[row]]
      shiny::updateTextInput(session, "code_name", value = d$name[[row]])
      colourpicker::updateColourInput(session, "code_color",
                                      value = d$color[[row]])
      shiny::updateTextAreaInput(session, "code_definition",
                                 value = d$definition[[row]] %||% "")
      shiny::updateTextAreaInput(session, "code_criteria",
                                 value = d$criteria[[row]] %||% "")
      shiny::updateTextAreaInput(session, "code_memo", value = d$memo[[row]])

      # Populate weight controls
      wt <- d$weight[[row]]
      has_weight <- !is.null(wt) && !is.na(wt)
      shiny::updateCheckboxInput(session, "weight_enabled", value = has_weight)
      if (has_weight) {
        shiny::updateSliderInput(session, "code_weight", value = wt)
        shiny::updateTextInput(session, "code_weight_desc",
          value = d$weight_description[[row]] %||% "")
      }
    })

    # -- Add code ----------------------------------------------------------

    shiny::observeEvent(input$btn_add_code, {
      nm <- trimws(input$code_name %||% "")
      if (nchar(nm) == 0L) {
        lv$name_error <- "Name is required."
        return()
      }
      if (tolower(nm) %in% tolower(codes()$name)) {
        lv$name_error <- paste0("'", nm, "' already exists.")
        return()
      }
      lv$name_error <- NULL
      wt <- if (isTRUE(input$weight_enabled)) input$code_weight %||% NULL else NULL
      wd <- if (isTRUE(input$weight_enabled))
        trimws(input$code_weight_desc %||% "") else ""
      tryCatch({
        qc_add_code(rv$project,
                    name               = nm,
                    color              = input$code_color,
                    definition         = input$code_definition,
                    criteria           = input$code_criteria,
                    memo               = input$code_memo,
                    weight             = wt,
                    weight_description = wd)
        rv$refresh_codes <- rv$refresh_codes + 1L
        .reset_form(session)
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # -- Save edits --------------------------------------------------------

    shiny::observeEvent(input$btn_save_code, {
      shiny::req(lv$selected_id)
      nm <- trimws(input$code_name %||% "")
      if (nchar(nm) == 0L) {
        lv$name_error <- "Name is required."
        return()
      }
      existing_lower <- tolower(codes()$name[codes()$id != lv$selected_id])
      if (tolower(nm) %in% existing_lower) {
        lv$name_error <- paste0("'", nm, "' already exists.")
        return()
      }
      lv$name_error <- NULL
      wt <- if (isTRUE(input$weight_enabled)) input$code_weight %||% NA else NA
      wd <- if (isTRUE(input$weight_enabled))
        trimws(input$code_weight_desc %||% "") else ""
      tryCatch({
        qc_update_code(rv$project, lv$selected_id,
                       name               = nm,
                       color              = input$code_color,
                       definition         = input$code_definition,
                       criteria           = input$code_criteria,
                       memo               = input$code_memo,
                       weight             = wt,
                       weight_description = wd)
        rv$refresh_codes <- rv$refresh_codes + 1L
        lv$selected_id   <- NULL
        lv$selected_name <- NULL
        .reset_form(session)
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # -- New / clear selection ---------------------------------------------

    shiny::observeEvent(input$btn_new_code_hdr, {
      lv$selected_id   <- NULL
      lv$selected_name <- NULL
      lv$name_error    <- NULL
      .reset_form(session)
      DT::selectRows(tbl_codes_proxy, NULL)
    })

    # -- Cancel edit -------------------------------------------------------

    shiny::observeEvent(input$btn_cancel_edit, {
      lv$selected_id   <- NULL
      lv$selected_name <- NULL
      .reset_form(session)
      DT::selectRows(tbl_codes_proxy, NULL)
    })

    # -- View history modal ------------------------------------------------

    shiny::observeEvent(input$btn_view_history, {
      shiny::req(lv$selected_id)
      hist <- qc_code_history(rv$project, lv$selected_id)
      output$tbl_history_modal <- DT::renderDataTable({
        DT::datatable(
          dplyr::select(hist, operation, field, old_value, new_value,
                        changed_at),
          rownames = FALSE,
          options  = list(pageLength = 15, dom = "tp",
                          order = list(list(4, "desc"))),
          colnames = c("Operation", "Field", "Old value", "New value",
                       "Changed at")
        )
      })
      shiny::showModal(shiny::modalDialog(
        title    = paste0("History \u2014 ", lv$selected_name),
        DT::dataTableOutput(ns("tbl_history_modal")),
        size     = "l",
        easyClose = TRUE,
        footer   = shiny::modalButton("Close")
      ))
    })

    # -- Review codings modal ----------------------------------------------

    output$tbl_review <- DT::renderDataTable({
      shiny::req(!is.null(lv$review_segs))
      segs <- dplyr::select(lv$review_segs,
                            coding_id, source_name, seltext,
                            selfirst, selast, memo)
      DT::datatable(
        segs,
        selection = "single",
        rownames  = FALSE,
        options   = list(
          pageLength = 15, dom = "ftp",
          columnDefs = list(list(visible = FALSE, targets = 0))
        ),
        colnames = c("ID", "Document", "Passage", "Start", "End", "Memo")
      )
    })

    shiny::observeEvent(input$btn_review_codings, {
      shiny::req(lv$selected_id)
      lv$review_segs <- qc_get_coded_segments(rv$project,
                                               code_ids = lv$selected_id)
      other_codes <- codes()[codes()$id != lv$selected_id, ]

      shiny::showModal(shiny::modalDialog(
        title    = paste0("Codings \u2014 ", lv$selected_name),
        size     = "l",
        easyClose = TRUE,
        DT::dataTableOutput(ns("tbl_review")),
        shiny::hr(),
        shiny::div(
          class = "d-flex gap-2 align-items-end",
          shiny::div(
            class = "flex-grow-1",
            shiny::selectInput(
              ns("sel_reassign_target"),
              "Reassign selected passage to:",
              choices = stats::setNames(other_codes$id, other_codes$name)
            )
          ),
          shiny::actionButton(ns("btn_do_reassign"), "Reassign",
                              class = "btn-primary"),
          shiny::actionButton(ns("btn_do_delete_coding"), "Delete",
                              class = "btn-outline-danger")
        ),
        footer = shiny::modalButton("Close")
      ))
    })

    shiny::observeEvent(input$btn_do_reassign, {
      row <- input$tbl_review_rows_selected
      shiny::req(row, lv$review_segs, input$sel_reassign_target)
      coding_id <- lv$review_segs$coding_id[[row]]
      qc_reassign_coding(rv$project, coding_id,
                         as.integer(input$sel_reassign_target))
      rv$refresh_codes <- rv$refresh_codes + 1L
      lv$review_segs   <- qc_get_coded_segments(rv$project,
                                                 code_ids = lv$selected_id)
      shiny::showNotification("Passage reassigned.", type = "message")
    })

    shiny::observeEvent(input$btn_do_delete_coding, {
      row <- input$tbl_review_rows_selected
      shiny::req(row, lv$review_segs)
      coding_id <- lv$review_segs$coding_id[[row]]
      qc_delete_coding(rv$project, coding_id)
      rv$refresh_codes <- rv$refresh_codes + 1L
      lv$review_segs   <- qc_get_coded_segments(rv$project,
                                                 code_ids = lv$selected_id)
      shiny::showNotification("Coding deleted.", type = "message")
    })

    # -- Merge modal -------------------------------------------------------

    shiny::observeEvent(input$btn_merge, {
      shiny::req(lv$selected_id)
      other_codes <- codes()[codes()$id != lv$selected_id, ]
      if (nrow(other_codes) == 0L) {
        shiny::showNotification(
          "No other codes to merge into.", type = "warning")
        return()
      }
      n_segs <- codes()$n_codings[codes()$id == lv$selected_id]
      n_segs <- if (length(n_segs) == 0L || is.na(n_segs[[1L]])) 0L else as.integer(n_segs[[1L]])
      shiny::showModal(shiny::modalDialog(
        title = paste0('Merge "', lv$selected_name, '" into another code'),
        shiny::p(paste0(
          n_segs, if (n_segs == 1L) ' passage' else ' passages',
          ' tagged as "', lv$selected_name, '" will move to ',
          'the target code. "', lv$selected_name, '" will then be deleted. ',
          'This cannot be undone.'
        )),
        shiny::selectInput(
          ns("sel_merge_target"), "Merge into:",
          choices = stats::setNames(other_codes$id, other_codes$name)
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_merge"), "Merge",
                              class = "btn-warning")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_merge, {
      shiny::req(lv$selected_id, input$sel_merge_target)
      tryCatch({
        qc_merge_codes(rv$project,
                       from_ids = lv$selected_id,
                       into_id  = as.integer(input$sel_merge_target))
        rv$refresh_codes <- rv$refresh_codes + 1L
        lv$selected_id   <- NULL
        lv$selected_name <- NULL
        .reset_form(session)
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # -- Split modal -------------------------------------------------------

    shiny::observeEvent(input$btn_split, {
      shiny::req(lv$selected_id)
      n_segs <- codes()$n_codings[codes()$id == lv$selected_id]
      n_segs <- if (length(n_segs) == 0L || is.na(n_segs[[1L]])) 0L else as.integer(n_segs[[1L]])
      shiny::showModal(shiny::modalDialog(
        title = paste0('Split "', lv$selected_name, '" into new codes'),
        shiny::p(paste0(
          'Two new codes will be created. "', lv$selected_name, '" (',
          n_segs, if (n_segs == 1L) ' passage' else ' passages',
          ') is kept intact \u2014 use "Review Codings" to reassign its passages ',
          'to the new codes, then delete it when done.'
        )),
        shiny::textInput(ns("split_name1"), "New code 1 name"),
        shiny::textInput(ns("split_name2"), "New code 2 name"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_split"), "Create codes",
                              class = "btn-warning")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_split, {
      n1 <- trimws(input$split_name1)
      n2 <- trimws(input$split_name2)
      shiny::req(lv$selected_id, nchar(n1) > 0, nchar(n2) > 0)
      tryCatch({
        qc_split_code(rv$project, lv$selected_id, c(n1, n2))
        rv$refresh_codes <- rv$refresh_codes + 1L
        shiny::removeModal()
        shiny::showNotification(
          paste0('Created "', n1, '" and "', n2,
                 '". Use "Review Codings" to reassign passages.'),
          type = "message", duration = 8L
        )
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # -- Delete confirm modal ----------------------------------------------

    shiny::observeEvent(input$btn_delete_code, {
      shiny::req(lv$selected_id)
      shiny::showModal(shiny::modalDialog(
        title = "Delete code?",
        paste0('Delete "', lv$selected_name,
               '" and all its codings? This cannot be undone.'),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_delete"), "Delete",
                              class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_delete, {
      shiny::req(lv$selected_id)
      qc_delete_code(rv$project, lv$selected_id)
      rv$refresh_codes <- rv$refresh_codes + 1L
      lv$selected_id   <- NULL
      lv$selected_name <- NULL
      .reset_form(session)
      shiny::removeModal()
    })

    # -- Validate codebook -----------------------------------------------------

    shiny::observeEvent(input$btn_validate, {
      issues <- tryCatch(
        qc_validate_codebook(rv$project),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          return(NULL)
        }
      )
      if (is.null(issues)) return()

      output$tbl_validation <- DT::renderDataTable({
        if (nrow(issues) == 0L) {
          return(DT::datatable(
            tibble::tibble(result = "No issues found \u2014 codebook looks good!"),
            rownames = FALSE, options = list(dom = "t")
          ))
        }
        df <- dplyr::select(issues, severity, code_name, issue_type, message)
        DT::datatable(
          df,
          rownames = FALSE,
          options  = list(pageLength = 30, dom = "tp",
                          order = list(list(0, "asc"))),
          colnames = c("Severity", "Code", "Issue type", "Message")
        ) |>
          DT::formatStyle("severity",
            target          = "row",
            backgroundColor = DT::styleEqual(
              c("error", "warning", "info"),
              c("#fce4e4", "#fff3cd", "#e8f4fd")
            )
          )
      })

      n_errors   <- sum(issues$severity == "error",   na.rm = TRUE)
      n_warnings <- sum(issues$severity == "warning",  na.rm = TRUE)
      n_info     <- sum(issues$severity == "info",     na.rm = TRUE)

      shiny::showModal(shiny::modalDialog(
        title = "Codebook Validation",
        size  = "l",
        easyClose = TRUE,
        if (nrow(issues) > 0L) shiny::div(
          class = "d-flex gap-2 mb-3",
          shiny::span(class = "badge bg-danger",  paste0(n_errors,   " error(s)")),
          shiny::span(class = "badge bg-warning text-dark", paste0(n_warnings, " warning(s)")),
          shiny::span(class = "badge bg-info text-dark",    paste0(n_info,     " info"))
        ),
        DT::dataTableOutput(ns("tbl_validation")),
        footer = shiny::modalButton("Close")
      ))
    })

    # -- Snapshots modal --------------------------------------------------------

    shiny::observeEvent(input$btn_snapshots, {
      snaps <- tryCatch(qc_list_snapshots(rv$project),
                        error = function(e) NULL)
      snap_choices <- if (!is.null(snaps) && nrow(snaps) > 0L) {
        lbl <- paste0(format(as.POSIXct(snaps$created_at), "%d %b %Y %H:%M"),
                      " \u2014 ", ifelse(nchar(snaps$label) > 0, snaps$label, "(no label)"),
                      " (", snaps$n_codes, " codes)")
        stats::setNames(snaps$id, lbl)
      } else {
        c("No snapshots yet" = "")
      }

      output$tbl_snaps <- DT::renderDataTable({
        if (is.null(snaps) || nrow(snaps) == 0L) {
          return(DT::datatable(
            tibble::tibble(message = "No snapshots yet."),
            rownames = FALSE, options = list(dom = "t")
          ))
        }
        DT::datatable(
          dplyr::select(snaps, id, label, n_codes, created_at),
          rownames = FALSE,
          options  = list(pageLength = 15, dom = "tp",
                          order = list(list(3, "desc"))),
          colnames = c("ID", "Label", "Codes", "Created at")
        )
      })

      shiny::showModal(shiny::modalDialog(
        title     = "Codebook Snapshots",
        size      = "l",
        easyClose = TRUE,
        shiny::h6("Existing snapshots"),
        DT::dataTableOutput(ns("tbl_snaps")),
        shiny::hr(),
        shiny::h6("Take a snapshot"),
        shiny::div(
          class = "d-flex gap-2 align-items-end",
          shiny::div(
            class = "flex-grow-1",
            shiny::textInput(ns("snap_label"), "Label",
                             placeholder = "e.g. after round 1 coding")
          ),
          shiny::actionButton(ns("btn_take_snapshot"), "Take snapshot",
                              class = "btn-primary")
        ),
        shiny::hr(),
        shiny::h6("Diff two snapshots"),
        bslib::layout_columns(
          col_widths = c(5, 5, 2),
          shiny::selectInput(ns("diff_snap1"), "Baseline", choices = snap_choices),
          shiny::selectInput(ns("diff_snap2"), "Comparison", choices = snap_choices),
          shiny::div(
            class = "mt-4",
            shiny::actionButton(ns("btn_diff_snaps"), "Diff",
                                class = "btn-outline-secondary")
          )
        ),
        shiny::uiOutput(ns("diff_result")),
        footer = shiny::modalButton("Close")
      ))
    })

    shiny::observeEvent(input$btn_take_snapshot, {
      label <- trimws(input$snap_label %||% "")
      tryCatch({
        row <- qc_snapshot_codebook(rv$project, label = label)
        shiny::showNotification(
          paste0("Snapshot saved (id ", row$id, ")."), type = "message")
        shiny::updateTextInput(session, "snap_label", value = "")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    shiny::observeEvent(input$btn_diff_snaps, {
      id1 <- as.integer(input$diff_snap1)
      id2 <- as.integer(input$diff_snap2)
      if (is.na(id1) || is.na(id2)) {
        shiny::showNotification("Select two snapshots to compare.", type = "warning")
        return()
      }
      if (id1 == id2) {
        shiny::showNotification("Select two different snapshots.", type = "warning")
        return()
      }
      diff <- tryCatch(
        qc_diff_snapshots(rv$project, id1, id2),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(diff)) return()

      output$diff_result <- shiny::renderUI({
        if (nrow(diff) == 0L) {
          return(shiny::p(class = "text-success mt-2",
                          "No differences \u2014 snapshots are identical."))
        }
        output$tbl_diff <- DT::renderDataTable({
          DT::datatable(
            dplyr::select(diff, change_type, code_name, field,
                          old_value, new_value),
            rownames = FALSE,
            options  = list(pageLength = 20, dom = "ftp", scrollX = TRUE),
            colnames = c("Change", "Code", "Field", "Before", "After")
          ) |>
            DT::formatStyle("change_type",
              target          = "row",
              backgroundColor = DT::styleEqual(
                c("added", "removed", "changed"),
                c("#d4edda", "#f8d7da", "#fff3cd")
              )
            )
        })
        shiny::tagList(
          shiny::hr(),
          shiny::h6(paste0(nrow(diff), " difference(s) found")),
          DT::dataTableOutput(ns("tbl_diff"))
        )
      })
    })

    # -- Import modal ------------------------------------------------------

    shiny::observeEvent(input$btn_import, {
      shiny::showModal(shiny::modalDialog(
        title = "Import Codebook",
        shiny::fileInput(ns("import_file"), "Choose file",
                         accept = c(".csv", ".json",
                                    "text/csv", "application/json")),
        shiny::selectInput(ns("import_format"), "Format",
                           choices = c("CSV" = "csv", "JSON" = "json")),
        shiny::checkboxInput(ns("import_skip"),
                             "Skip codes that already exist",
                             value = TRUE),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_do_import"), "Import",
                              class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$btn_do_import, {
      shiny::req(input$import_file)
      tryCatch({
        result <- qc_import_codebook(
          rv$project,
          path          = input$import_file$datapath,
          format        = input$import_format,
          skip_existing = isTRUE(input$import_skip)
        )
        rv$refresh_codes <- rv$refresh_codes + 1L
        shiny::removeModal()
        shiny::showNotification(
          paste0("Imported ", result$imported, " code(s), skipped ",
                 result$skipped, " duplicate(s)."),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # -- Export download handler -------------------------------------------

    output$dl_export <- shiny::downloadHandler(
      filename = function() {
        fmt <- input$export_format %||% "csv"
        paste0("codebook_", format(Sys.Date(), "%Y%m%d"), ".", fmt)
      },
      content = function(file) {
        fmt <- input$export_format %||% "csv"
        qc_export_codebook(rv$project, file, format = fmt)
      }
    )

    # -- Category management -----------------------------------------------

    shiny::observe({
      shiny::updateSelectInput(session, "sel_code_assign",
        choices = stats::setNames(codes()$id, codes()$name))
      shiny::updateSelectInput(session, "sel_cat_assign",
        choices = stats::setNames(cats()$id, cats()$name))
    })

    shiny::observeEvent(input$btn_add_cat, {
      shiny::req(nchar(trimws(input$cat_name)) > 0)
      tryCatch({
        qc_add_category(rv$project, name = trimws(input$cat_name))
        rv$refresh_codes <- rv$refresh_codes + 1L
        shinyjs::reset("cat_name")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    shiny::observeEvent(input$btn_assign, {
      shiny::req(input$sel_code_assign, input$sel_cat_assign)
      qc_link_code_category(rv$project,
        code_id     = as.integer(input$sel_code_assign),
        category_id = as.integer(input$sel_cat_assign)
      )
      rv$refresh_codes <- rv$refresh_codes + 1L
    })
  })
}

# Prefix code names with indent markers based on parent_id depth.
.indent_code_names <- function(names, ids, parent_ids) {
  depth <- integer(length(ids))
  id_map <- stats::setNames(seq_along(ids), as.character(ids))
  for (i in seq_along(ids)) {
    pid <- parent_ids[i]
    d   <- 0L
    while (!is.na(pid) && !is.null(pid) && as.character(pid) %in% names(id_map)) {
      d   <- d + 1L
      idx <- id_map[[as.character(pid)]]
      pid <- parent_ids[idx]
      if (d > 10L) break  # guard against circular refs
    }
    depth[i] <- d
  }
  ifelse(depth == 0L, names,
    paste0(strrep("   ", depth), "\u2514 ", names))
}

# Clear all form inputs and return to add-mode defaults
.reset_form <- function(session) {
  shiny::updateTextInput(session, "code_name", value = "")
  colourpicker::updateColourInput(session, "code_color", value = "#4E79A7")
  shiny::updateTextAreaInput(session, "code_definition", value = "")
  shiny::updateTextAreaInput(session, "code_criteria",   value = "")
  shiny::updateTextAreaInput(session, "code_memo",       value = "")
  shiny::updateCheckboxInput(session, "weight_enabled",  value = FALSE)
}
