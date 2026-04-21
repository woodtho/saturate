mod_coding_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$script(src = system.file("app/coding.js", package = "qualcoder"))
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),

      # Left: document text with server-rendered highlights
      bslib::card(
        bslib::card_header(shiny::textOutput(ns("doc_title"))),
        shiny::uiOutput(ns("text_display"))
      ),

      # Right: coding controls
      bslib::card(
        bslib::card_header("Apply Code"),
        shiny::div(
          class = "p-2",
          shiny::h6("Selected text"),
          shiny::verbatimTextOutput(ns("selected_text"), placeholder = TRUE),
          shiny::selectInput(ns("sel_code"), "Code", choices = character(0)),
          shiny::textAreaInput(ns("seg_memo"), "Segment memo", rows = 2),
          shiny::actionButton(ns("btn_apply"), "Apply Code",
                              class = "btn-success w-100"),
          shiny::hr(),
          shiny::h6("Codings in this document"),
          DT::dataTableOutput(ns("tbl_codings"))
        )
      )
    )
  )
}

mod_coding_server <- function(id, rv, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Tell JS the Shiny input namespace prefix for this session
    session$sendCustomMessage("qc_set_ns", list(ns_prefix = ns("")))

    # ── Document display ────────────────────────────────────────────────────

    codings_rv <- shiny::reactive({
      shiny::req(rv$active_source_id)
      rv$refresh_codes
      qc_list_codings(rv$project, rv$active_source_id)
    })

    doc_rv <- shiny::reactive({
      shiny::req(rv$active_source_id)
      qc_get_document(rv$project, rv$active_source_id)
    })

    output$doc_title <- shiny::renderText({
      shiny::req(doc_rv())
      doc_rv()$name
    })

    output$text_display <- shiny::renderUI({
      shiny::req(doc_rv())
      build_highlighted_html(doc_rv()$content, codings_rv())
    })

    # ── Code selector ────────────────────────────────────────────────────────

    shiny::observe({
      rv$refresh_codes
      codes <- qc_list_codes(rv$project)
      shiny::updateSelectInput(session, "sel_code",
        choices  = stats::setNames(codes$id, codes$name),
        selected = character(0)
      )
    })

    # ── Selection from JS ────────────────────────────────────────────────────

    output$selected_text <- shiny::renderText({
      shiny::req(input$selection)
      input$selection$text
    })

    # ── Apply code ────────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_apply, {
      shiny::req(input$selection, input$sel_code, rv$active_source_id)
      sel <- input$selection
      tryCatch(
        {
          qc_add_coding(
            project   = rv$project,
            source_id = rv$active_source_id,
            code_id   = as.integer(input$sel_code),
            selfirst  = as.integer(sel$start),
            selast    = as.integer(sel$end),
            memo      = input$seg_memo
          )
          rv$refresh_codes <- rv$refresh_codes + 1L
          shinyjs::reset("seg_memo")
        },
        error = function(e) shiny::showNotification(conditionMessage(e), type = "error")
      )
    })

    # ── Codings table ─────────────────────────────────────────────────────────

    output$tbl_codings <- DT::renderDataTable({
      shiny::req(rv$active_source_id)
      d <- codings_rv()
      DT::datatable(
        dplyr::select(d, code_name, seltext, memo),
        selection = "single",
        rownames  = FALSE,
        options   = list(pageLength = 10, dom = "tp"),
        colnames  = c("Code", "Passage", "Memo")
      )
    })

    # ── Delete coding ─────────────────────────────────────────────────────────

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
