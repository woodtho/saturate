mod_codebook_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),

    # Left: add / edit code
    bslib::card(
      bslib::card_header("Add Code"),
      shiny::div(class = "p-2",
        shiny::textInput(ns("code_name"),  "Name"),
        shiny::textInput(ns("code_color"), "Hex colour", value = "#4E79A7"),
        shiny::div(
          style = "display:flex; gap:8px; align-items:center; margin-bottom:8px;",
          shiny::uiOutput(ns("color_preview")),
          shiny::span("Preview", style = "font-size:0.85rem; color:#6c757d;")
        ),
        shiny::textAreaInput(ns("code_memo"), "Memo", rows = 2),
        shiny::actionButton(ns("btn_add_code"), "Add Code",
                            class = "btn-success w-100"),
        shiny::hr(),
        shiny::h6("Categories"),
        shiny::textInput(ns("cat_name"), "New category name"),
        shiny::actionButton(ns("btn_add_cat"), "Add Category",
                            class = "btn-outline-primary w-100"),
        shiny::hr(),
        shiny::h6("Assign code to category"),
        shiny::selectInput(ns("sel_code_assign"), "Code",     choices = NULL),
        shiny::selectInput(ns("sel_cat_assign"),  "Category", choices = NULL),
        shiny::actionButton(ns("btn_assign"), "Assign",
                            class = "btn-outline-secondary w-100")
      )
    ),

    # Right: codes table
    bslib::card(
      bslib::card_header("Codebook"),
      DT::dataTableOutput(ns("tbl_codes"))
    )
  )
}

mod_codebook_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    codes <- shiny::reactive({
      rv$refresh_codes
      qc_list_codes(rv$project)
    })

    cats <- shiny::reactive({
      rv$refresh_codes
      .query(rv$project$con,
        "SELECT id, name FROM code_categories WHERE status = 1 ORDER BY name")
    })

    output$color_preview <- shiny::renderUI({
      col <- input$code_color
      if (grepl("^#[0-9A-Fa-f]{6}$", col)) {
        shiny::div(style = paste0(
          "width:28px; height:28px; border-radius:4px; ",
          "background:", col, "; border:1px solid #ccc;"
        ))
      }
    })

    output$tbl_codes <- DT::renderDataTable({
      d <- codes()
      DT::datatable(
        dplyr::select(d, id, name, color, n_codings, categories, memo),
        selection = "single",
        rownames  = FALSE,
        options   = list(pageLength = 20, dom = "ftp"),
        colnames  = c("ID", "Name", "Colour", "Codings", "Categories", "Memo")
      )
    })

    shiny::observe({
      shiny::updateSelectInput(session, "sel_code_assign",
        choices = stats::setNames(codes()$id, codes()$name))
      shiny::updateSelectInput(session, "sel_cat_assign",
        choices = stats::setNames(cats()$id, cats()$name))
    })

    shiny::observeEvent(input$btn_add_code, {
      shiny::req(nchar(trimws(input$code_name)) > 0)
      tryCatch(
        {
          qc_add_code(rv$project,
                      name  = input$code_name,
                      color = input$code_color,
                      memo  = input$code_memo)
          rv$refresh_codes <- rv$refresh_codes + 1L
          shinyjs::reset("code_name")
          shinyjs::reset("code_memo")
        },
        error = function(e) shiny::showNotification(conditionMessage(e), type = "error")
      )
    })

    shiny::observeEvent(input$btn_add_cat, {
      shiny::req(nchar(trimws(input$cat_name)) > 0)
      tryCatch(
        {
          qc_add_category(rv$project, name = input$cat_name)
          rv$refresh_codes <- rv$refresh_codes + 1L
          shinyjs::reset("cat_name")
        },
        error = function(e) shiny::showNotification(conditionMessage(e), type = "error")
      )
    })

    shiny::observeEvent(input$btn_assign, {
      shiny::req(input$sel_code_assign, input$sel_cat_assign)
      qc_link_code_category(rv$project,
        code_id     = as.integer(input$sel_code_assign),
        category_id = as.integer(input$sel_cat_assign)
      )
      rv$refresh_codes <- rv$refresh_codes + 1L
    })

    # Delete selected code
    shiny::observeEvent(input$tbl_codes_rows_selected, {
      row <- input$tbl_codes_rows_selected
      shiny::req(row)
      code_name <- codes()$name[[row]]
      shiny::showModal(shiny::modalDialog(
        title = "Delete code?",
        paste0("Delete code \"", code_name, "\" and all its codings?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_del_code"), "Delete",
                              class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_del_code, {
      row <- input$tbl_codes_rows_selected
      shiny::req(row)
      qc_delete_code(rv$project, codes()$id[[row]])
      rv$refresh_codes <- rv$refresh_codes + 1L
      shiny::removeModal()
    })
  })
}
