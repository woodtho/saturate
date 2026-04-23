mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,

      shiny::h5("Graph settings"),
      qc_help_details(
        "Graph help",
        shiny::p(
          "Graphs show relationships already present in the coded project. ",
          "Use filters and minimum thresholds to remove weak or noisy links."
        ),
        qc_help_list(c(
          "Document similarity connects documents that share codes.",
          "Bipartite shows documents and codes in one network.",
          "Code co-occurrence connects codes that appear in the same documents."
        ))
      ),
      shiny::selectInput(ns("graph_type"), "Type",
        choices = c(
          "Document similarity"  = "similarity",
          "Bipartite (docs + codes)" = "bipartite",
          "Code co-occurrence"   = "cooccurrence"
        )
      ),
      shiny::sliderInput(ns("min_shared"), "Min. shared codes / docs",
                         min = 1L, max = 10L, value = 1L, step = 1L),
      shiny::selectizeInput(ns("filter_codes"), "Filter codes",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "All codes")),
      shiny::selectizeInput(ns("filter_sources"), "Filter documents",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "All documents")),
      shiny::actionButton(ns("btn_draw"), "Draw graph",
                          class = "btn-primary w-100"),
      shiny::hr(),
      shiny::h6("Selected node"),
      shiny::uiOutput(ns("node_info")),
      shiny::hr(),
      qc_help_note(
        shiny::p("Node size reflects coding volume."),
        shiny::p(
          "Edge weight reflects shared codes for similarity graphs or ",
          "co-document count for co-occurrence graphs."
        ),
        shiny::p("Requires the ", shiny::tags$code("visNetwork"), " package.")
      )
    ),

    bslib::card(
      bslib::card_header(shiny::textOutput(ns("graph_title"))),
      shiny::uiOutput(ns("graph_or_msg"))
    )
  )
}

mod_graph_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    has_visnet <- requireNamespace("visNetwork", quietly = TRUE)

    output$graph_or_msg <- shiny::renderUI({
      if (!has_visnet) {
        shiny::div(
          class = "alert alert-warning m-3",
          shiny::tags$b("visNetwork not installed."),
          " Run ",
          shiny::tags$code('install.packages("visNetwork")'),
          " then restart the app."
        )
      } else {
        visNetwork::visNetworkOutput(ns("graph"), height = "72vh")
      }
    })

    shiny::observe({
      rv$refresh_codes
      rv$refresh_docs
      codes <- qc_list_codes(rv$project)
      docs  <- qc_list_documents(rv$project)
      shiny::updateSelectizeInput(session, "filter_codes",
        choices = stats::setNames(codes$id, codes$name), server = TRUE)
      shiny::updateSelectizeInput(session, "filter_sources",
        choices = stats::setNames(docs$id, docs$name),   server = TRUE)
    })

    output$graph_title <- shiny::renderText({
      switch(input$graph_type %||% "similarity",
        similarity   = "Document similarity network (shared codes)",
        bipartite    = "Bipartite network (documents ↔ codes)",
        cooccurrence = "Code co-occurrence network"
      )
    })

    graph_data <- shiny::eventReactive(input$btn_draw, {
      int_or_null <- function(x) if (length(x) > 0L) as.integer(x) else NULL
      qc_document_graph(
        rv$project,
        type       = input$graph_type,
        code_ids   = int_or_null(input$filter_codes),
        source_ids = int_or_null(input$filter_sources),
        min_shared = as.integer(input$min_shared)
      )
    }, ignoreNULL = FALSE)

    lv <- shiny::reactiveValues(selected_node = NULL)

    output$graph <- visNetwork::renderVisNetwork({
      shiny::req(has_visnet)
      gd <- graph_data()

      if (nrow(gd$nodes) == 0L) {
        shiny::showNotification(
          "No graph data — try relaxing the filters or minimum threshold.",
          type = "warning")
        return(NULL)
      }

      net <- visNetwork::visNetwork(gd$nodes, gd$edges,
                                    width = "100%", height = "72vh") |>
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1L,
                                  hover = TRUE),
          nodesIdSelection = TRUE
        ) |>
        visNetwork::visPhysics(
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(gravitationalConstant = -80)
        ) |>
        visNetwork::visEdges(smooth = list(type = "continuous")) |>
        visNetwork::visInteraction(navigationButtons = TRUE,
                                   tooltipDelay = 200)

      # Colour groups
      if (input$graph_type == "bipartite") {
        net <- net |>
          visNetwork::visGroups(groupname = "document",
                                color = "#AEC6CF", shape = "box") |>
          visNetwork::visGroups(groupname = "code",
                                shape = "ellipse") |>
          visNetwork::visLegend(addNodes = list(
            list(label = "Document", shape = "box",
                 color = "#AEC6CF"),
            list(label = "Code",     shape = "ellipse",
                 color = "#4E79A7")
          ), useGroups = FALSE)
      }

      visNetwork::visEvents(net,
        selectNode = paste0(
          "function(nodes) { Shiny.setInputValue('", ns("selected_node"),
          "', nodes.nodes[0], {priority:'event'}); }"
        )
      )
    })

    shiny::observeEvent(input$selected_node, {
      lv$selected_node <- input$selected_node
    })

    output$node_info <- shiny::renderUI({
      node_id <- lv$selected_node
      shiny::req(!is.null(node_id))
      gd <- graph_data()
      row <- gd$nodes[gd$nodes$id == node_id, ]
      if (nrow(row) == 0L) return(NULL)
      shiny::tagList(
        shiny::strong(row$label[[1L]]),
        shiny::br(),
        shiny::span(
          paste0("Group: ", row$group[[1L]]),
          style = "color:var(--sat-text-muted); font-size:0.85rem;"
        ),
        if (!is.null(row$value) && !is.na(row$value[[1L]])) {
          shiny::div(
            paste0("Weight: ", row$value[[1L]]),
            style = "font-size:0.85rem;"
          )
        }
      )
    })
  })
}
