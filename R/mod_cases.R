mod_cases_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 290,

      shiny::h5("New Case"),
      qc_help_note(
        "A case represents one participant, respondent, site, or unit of analysis. ",
        "Link documents to a case to group evidence by source."
      ),
      shiny::textInput(ns("new_case_name"), "Name", placeholder = "e.g. Participant 01"),
      shiny::textAreaInput(ns("new_case_memo"), "Memo", rows = 2,
        placeholder = "Optional description…"),
      shiny::actionButton(ns("btn_add_case"), "Add case",
        class = "btn-primary w-100"),

      shiny::hr(),

      shiny::div(
        class = "d-grid gap-2",
        shiny::actionButton(ns("btn_edit_case"), "Edit selected",
          class = "btn-outline-secondary"),
        shiny::actionButton(ns("btn_delete_case"), "Delete selected",
          class = "btn-outline-danger")
      )
    ),

    shiny::div(
      id = "sat-main-content",

      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "d-flex justify-content-between align-items-center w-100",
            "Cases",
            shiny::div(
              class = "d-flex gap-2 align-items-center",
              shiny::downloadButton(ns("dl_attributes_csv"),  "CSV",
                class = "btn-sm btn-outline-secondary"),
              shiny::downloadButton(ns("dl_attributes_xlsx"), "Excel",
                class = "btn-sm btn-outline-secondary"),
              shiny::downloadButton(ns("dl_attributes_json"), "JSON",
                class = "btn-sm btn-outline-secondary")
            )
          )
        ),
        qc_help_note(
          "Select a row to manage attributes and linked documents."
        ),
        DT::dataTableOutput(ns("tbl_cases"))
      ),

      shiny::uiOutput(ns("case_detail_ui"))
    )
  )
}

mod_cases_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lv <- shiny::reactiveValues(
      selected_id   = NULL,
      refresh_cases = 0L,
      refresh_attrs = 0L,
      refresh_links = 0L
    )

    # ── Case list ──────────────────────────────────────────────────────────────

    cases <- shiny::reactive({
      lv$refresh_cases
      rv$refresh_docs
      qc_list_cases(rv$project)
    })

    output$tbl_cases <- DT::renderDataTable({
      df <- cases()
      DT::datatable(
        dplyr::select(df, id, name, n_sources, memo),
        class      = "table table-hover",
        selection  = "single",
        rownames   = FALSE,
        colnames   = c("ID", "Name", "Docs linked", "Memo"),
        options    = list(
          pageLength = 15,
          dom        = "ftp",
          order      = list(list(1, "asc"))
        )
      )
    })

    shiny::observeEvent(input$tbl_cases_rows_selected, {
      sel <- input$tbl_cases_rows_selected
      if (length(sel) == 0L) {
        lv$selected_id <- NULL
        return()
      }
      df <- shiny::isolate(cases())
      lv$selected_id <- df$id[sel]
    })

    # ── Add case ───────────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_add_case, {
      name <- trimws(input$new_case_name %||% "")
      if (nchar(name) == 0L) {
        shiny::showNotification("Enter a case name.", type = "warning")
        return()
      }
      tryCatch({
        qc_add_case(rv$project, name, input$new_case_memo %||% "")
        lv$refresh_cases <- lv$refresh_cases + 1L
        shiny::updateTextInput(session, "new_case_name",  value = "")
        shiny::updateTextAreaInput(session, "new_case_memo", value = "")
        shiny::showNotification(paste0("Case '", name, "' added."), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    # ── Edit case ──────────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_edit_case, {
      cid <- lv$selected_id
      if (is.null(cid)) {
        shiny::showNotification("Select a case first.", type = "warning")
        return()
      }
      df <- shiny::isolate(cases())
      row <- df[df$id == cid, , drop = FALSE]
      shiny::showModal(shiny::modalDialog(
        title     = "Edit Case",
        easyClose = TRUE,
        shiny::textInput(ns("edit_case_name"), "Name", value = row$name),
        shiny::textAreaInput(ns("edit_case_memo"), "Memo", rows = 3,
          value = row$memo %||% ""),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_edit_case"), "Save",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_edit_case, {
      cid  <- lv$selected_id
      name <- trimws(input$edit_case_name %||% "")
      memo <- input$edit_case_memo %||% ""
      if (nchar(name) == 0L) {
        shiny::showNotification("Name cannot be empty.", type = "warning")
        return()
      }
      tryCatch({
        qc_update_case(rv$project, cid, name = name, memo = memo)
        lv$refresh_cases <- lv$refresh_cases + 1L
        shiny::removeModal()
        shiny::showNotification("Case updated.", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    # ── Delete case ────────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_delete_case, {
      cid <- lv$selected_id
      if (is.null(cid)) {
        shiny::showNotification("Select a case first.", type = "warning")
        return()
      }
      df   <- shiny::isolate(cases())
      name <- df$name[df$id == cid]
      shiny::showModal(shiny::modalDialog(
        title     = "Delete Case",
        easyClose = TRUE,
        shiny::p("Delete case ", shiny::tags$strong(name), "? This cannot be undone."),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_delete_case"), "Delete",
            class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_delete_case, {
      cid <- lv$selected_id
      tryCatch({
        qc_delete_case(rv$project, cid)
        lv$selected_id   <- NULL
        lv$refresh_cases <- lv$refresh_cases + 1L
        shiny::removeModal()
        shiny::showNotification("Case deleted.", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    # ── Case detail panel ──────────────────────────────────────────────────────

    output$case_detail_ui <- shiny::renderUI({
      cid <- lv$selected_id
      if (is.null(cid)) return(NULL)

      df   <- shiny::isolate(cases())
      name <- df$name[df$id == cid]

      bslib::card(
        class = "mt-3",
        bslib::card_header(paste0("Case: ", name)),
        bslib::navset_tab(
          bslib::nav_panel(
            "Attributes",
            shiny::div(
              class = "p-3",
              qc_help_note(
                "Attributes are free-form key-value pairs (e.g. age_group = adult, ",
                "site = north). Use consistent variable names across cases."
              ),
              bslib::layout_columns(
                col_widths = c(4, 4, 4),
                shiny::selectizeInput(ns("attr_variable"), "Variable",
                  choices = NULL,
                  options = list(
                    placeholder  = "e.g. age_group",
                    create       = TRUE,
                    createOnBlur = TRUE
                  )),
                shiny::textInput(ns("attr_value"), "Value",
                  placeholder = "e.g. adult"),
                shiny::div(
                  class = "mt-4",
                  shiny::actionButton(ns("btn_set_attr"), "Set attribute",
                    class = "btn-primary")
                )
              ),
              DT::dataTableOutput(ns("tbl_attributes"))
            )
          ),
          bslib::nav_panel(
            "Documents",
            shiny::div(
              class = "p-3",
              qc_help_note(
                "Link documents to this case so evidence can be grouped ",
                "by participant or site in the Query and Export panels."
              ),
              bslib::layout_columns(
                col_widths = c(8, 4),
                shiny::selectizeInput(ns("link_doc_id"), "Link a document",
                  choices = NULL,
                  options = list(placeholder = "Search documents…")),
                shiny::div(
                  class = "mt-4",
                  shiny::actionButton(ns("btn_link_doc"), "Link",
                    class = "btn-outline-primary")
                )
              ),
              DT::dataTableOutput(ns("tbl_linked_docs"))
            )
          )
        )
      )
    })

    # ── Attributes sub-panel ───────────────────────────────────────────────────

    attrs <- shiny::reactive({
      lv$selected_id
      lv$refresh_attrs
      cid <- lv$selected_id
      if (is.null(cid)) return(tibble::tibble(variable = character(), value = character()))
      tryCatch(qc_list_case_attributes(rv$project, cid),
               error = function(e) tibble::tibble(variable = character(), value = character()))
    })

    output$tbl_attributes <- DT::renderDataTable({
      df <- attrs()
      DT::datatable(
        df,
        class      = "table table-sm table-hover",
        selection  = "single",
        rownames   = FALSE,
        colnames   = c("Variable", "Value"),
        options    = list(dom = "tp", pageLength = 20)
      )
    })

    shiny::observeEvent(input$btn_set_attr, {
      cid <- lv$selected_id
      if (is.null(cid)) return()
      variable <- trimws(input$attr_variable[[1L]] %||% "")
      value    <- trimws(input$attr_value    %||% "")
      if (nchar(variable) == 0L) {
        shiny::showNotification("Enter a variable name.", type = "warning")
        return()
      }
      tryCatch({
        qc_set_case_attribute(rv$project, cid, variable, value)
        lv$refresh_attrs <- lv$refresh_attrs + 1L
        shiny::updateSelectizeInput(session, "attr_variable", selected = character(0))
        shiny::updateTextInput(session, "attr_value", value = "")
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$tbl_attributes_rows_selected, {
      sel <- input$tbl_attributes_rows_selected
      if (length(sel) == 0L) return()
      df  <- shiny::isolate(attrs())
      var <- df$variable[sel]
      shiny::showModal(shiny::modalDialog(
        title     = "Delete Attribute",
        easyClose = TRUE,
        shiny::p("Delete attribute ", shiny::tags$strong(var), "?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_delete_attr"), "Delete",
            class = "btn-danger",
            `data-variable` = var)
        )
      ))
      lv$pending_attr_var <- var
    })

    shiny::observeEvent(input$btn_confirm_delete_attr, {
      cid <- lv$selected_id
      var <- lv$pending_attr_var
      if (is.null(cid) || is.null(var)) return()
      tryCatch({
        qc_delete_case_attribute(rv$project, cid, var)
        lv$refresh_attrs   <- lv$refresh_attrs + 1L
        lv$pending_attr_var <- NULL
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    # ── Documents sub-panel ────────────────────────────────────────────────────

    shiny::observe({
      lv$selected_id
      rv$refresh_docs
      docs <- tryCatch(qc_list_documents(rv$project),
                       error = function(e) tibble::tibble(id = integer(), name = character()))
      choices <- stats::setNames(docs$id, docs$name)
      shiny::updateSelectizeInput(session, "link_doc_id",
        choices = choices, server = TRUE)
    })

    shiny::observe({
      lv$refresh_attrs
      vars <- tryCatch(
        .query(rv$project$con,
          "SELECT DISTINCT variable FROM case_attributes WHERE status = 1 ORDER BY variable"
        )$variable,
        error = function(e) character()
      )
      shiny::updateSelectizeInput(session, "attr_variable",
        choices = stats::setNames(vars, vars), server = TRUE)
    })

    linked_docs <- shiny::reactive({
      lv$selected_id
      lv$refresh_links
      cid <- lv$selected_id
      if (is.null(cid)) return(tibble::tibble(id = integer(), name = character()))
      tryCatch({
        all_docs <- qc_list_documents(rv$project)
        raw <- .query(rv$project$con,
          "SELECT source_id FROM case_source_links WHERE case_id = ? AND status = 1",
          list(as.integer(cid))
        )
        if (nrow(raw) == 0L)
          return(tibble::tibble(id = integer(), name = character()))
        all_docs[all_docs$id %in% raw$source_id, c("id", "name"), drop = FALSE]
      }, error = function(e) tibble::tibble(id = integer(), name = character()))
    })

    output$tbl_linked_docs <- DT::renderDataTable({
      df <- linked_docs()
      DT::datatable(
        df,
        class      = "table table-sm table-hover",
        selection  = "single",
        rownames   = FALSE,
        colnames   = c("ID", "Document"),
        caption    = "Select a row and click Unlink to remove the association.",
        options    = list(dom = "tp", pageLength = 20)
      )
    })

    shiny::observeEvent(input$btn_link_doc, {
      cid <- lv$selected_id
      if (is.null(cid)) return()
      doc_id <- as.integer(input$link_doc_id)
      if (length(doc_id) == 0L || is.na(doc_id)) {
        shiny::showNotification("Select a document to link.", type = "warning")
        return()
      }
      tryCatch({
        qc_link_case_source(rv$project, cid, doc_id)
        lv$refresh_links <- lv$refresh_links + 1L
        lv$refresh_cases <- lv$refresh_cases + 1L
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    shiny::observeEvent(input$tbl_linked_docs_rows_selected, {
      sel <- input$tbl_linked_docs_rows_selected
      if (length(sel) == 0L) return()
      df     <- shiny::isolate(linked_docs())
      doc_id <- df$id[sel]
      doc_nm <- df$name[sel]
      shiny::showModal(shiny::modalDialog(
        title     = "Unlink Document",
        easyClose = TRUE,
        shiny::p("Unlink document ", shiny::tags$strong(doc_nm), " from this case?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_unlink_doc"), "Unlink",
            class = "btn-danger")
        )
      ))
      lv$pending_doc_id <- doc_id
    })

    shiny::observeEvent(input$btn_confirm_unlink_doc, {
      cid    <- lv$selected_id
      doc_id <- lv$pending_doc_id
      if (is.null(cid) || is.null(doc_id)) return()
      tryCatch({
        qc_unlink_case_source(rv$project, cid, doc_id)
        lv$refresh_links  <- lv$refresh_links + 1L
        lv$refresh_cases  <- lv$refresh_cases + 1L
        lv$pending_doc_id <- NULL
        shiny::removeModal()
      }, error = function(e) {
        shiny::showNotification(paste0("Error: ", conditionMessage(e)), type = "error")
      })
    })

    # ── Attributes exports ─────────────────────────────────────────────────────

    .attrs_df <- function() {
      tryCatch(qc_case_attributes_wide(rv$project), error = function(e) tibble::tibble())
    }

    output$dl_attributes_csv <- shiny::downloadHandler(
      filename = function() paste0("case_attributes_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(.attrs_df(), file, row.names = FALSE)
    )

    output$dl_attributes_xlsx <- shiny::downloadHandler(
      filename = function() paste0("case_attributes_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        tryCatch(.write_xlsx(list(attributes = .attrs_df()), file),
          error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
      }
    )

    output$dl_attributes_json <- shiny::downloadHandler(
      filename = function() paste0("case_attributes_", Sys.Date(), ".json"),
      content  = function(file) {
        if (!requireNamespace("jsonlite", quietly = TRUE)) {
          shiny::showNotification("Install 'jsonlite' for JSON export.", type = "error")
          return()
        }
        writeLines(jsonlite::toJSON(.attrs_df(), auto_unbox = TRUE, pretty = TRUE), file)
      }
    )
  })
}
