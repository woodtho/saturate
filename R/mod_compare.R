mod_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "p-3",

    # ── Controls row ──────────────────────────────────────────────────────────
    bslib::card(
      bslib::card_body(
        shiny::div(
          class = "d-flex gap-3 align-items-end flex-wrap",
          shiny::div(
            shiny::tags$label("Compare by", class = "form-label"),
            shiny::selectInput(ns("mode"), label = NULL,
              choices = c("Two documents" = "docs",
                          "Two coders (same document)" = "coders"),
              width = "220px")
          ),
          shiny::uiOutput(ns("ctrl_shared_doc")),
          shiny::uiOutput(ns("ctrl_left")),
          shiny::uiOutput(ns("ctrl_right")),
          shiny::div(
            class = "d-flex align-items-center gap-2",
            style = "padding-bottom:0.25rem;",
            shiny::checkboxInput(ns("sync_scroll"), "Sync scroll", FALSE),
            shiny::actionButton(ns("btn_refresh"), "Refresh",
              class = "btn-sm btn-outline-secondary")
          )
        )
      )
    ),

    qc_help_details(
      "Compare help",
      shiny::p(
        "Use document comparison to inspect how coding differs across sources. ",
        "Use coder comparison on one document to find agreement, unique work, ",
        "and conflicting interpretations."
      ),
      qc_help_list(c(
        "Sync scroll keeps the two text panes aligned while reading.",
        "Refresh reloads the comparison after coding or filter changes.",
        "The Differences table summarizes code-level or segment-level mismatches."
      ))
    ),

    # ── Side-by-side text ─────────────────────────────────────────────────────
    bslib::layout_columns(
      col_widths = c(6, 6),

      bslib::card(
        class = "qc-compare-panel",
        bslib::card_header(shiny::textOutput(ns("left_title"))),
        shiny::div(
          class = "p-2",
          shiny::uiOutput(ns("left_text"))
        )
      ),

      bslib::card(
        class = "qc-compare-panel",
        bslib::card_header(shiny::textOutput(ns("right_title"))),
        shiny::div(
          class = "p-2",
          shiny::uiOutput(ns("right_text"))
        )
      )
    ),

    # ── Differences table ─────────────────────────────────────────────────────
    bslib::card(
      bslib::card_header("Differences"),
      shiny::div(
        class = "p-2",
        shiny::uiOutput(ns("diff_summary")),
        DT::dataTableOutput(ns("tbl_diff"))
      )
    ),

    # ── Reliability statistics (coders mode only) ─────────────────────────────
    shiny::uiOutput(ns("reliability_card"))
  )
}

mod_compare_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Dynamic controls ────────────────────────────────────────────────────

    docs_rv <- shiny::reactive({
      rv$refresh_docs
      qc_list_documents(rv$project)
    })

    coders_rv <- shiny::reactive({
      rv$refresh_codes
      qc_list_coders(rv$project)$coder
    })

    # Shared doc picker (coders mode only)
    output$ctrl_shared_doc <- shiny::renderUI({
      if (input$mode != "coders") return(NULL)
      docs <- docs_rv()
      if (nrow(docs) == 0L) return(NULL)
      shiny::div(
        shiny::tags$label("Document", class = "form-label"),
        shiny::selectInput(ns("shared_doc"),
          label   = NULL,
          choices = stats::setNames(docs$id, docs$name),
          width   = "240px")
      )
    })

    output$ctrl_left <- shiny::renderUI({
      if (input$mode == "docs") {
        docs <- docs_rv()
        if (nrow(docs) == 0L) return(NULL)
        shiny::div(
          shiny::tags$label("Left document", class = "form-label"),
          shiny::selectInput(ns("left_doc"),
            label   = NULL,
            choices = stats::setNames(docs$id, docs$name),
            width   = "240px")
        )
      } else {
        coders <- coders_rv()
        shiny::div(
          shiny::tags$label("Left coder", class = "form-label"),
          shiny::selectInput(ns("left_coder"),
            label   = NULL,
            choices = c("(all)" = "", stats::setNames(coders, coders)),
            width   = "180px")
        )
      }
    })

    output$ctrl_right <- shiny::renderUI({
      if (input$mode == "docs") {
        docs <- docs_rv()
        if (nrow(docs) == 0L) return(NULL)
        shiny::div(
          shiny::tags$label("Right document", class = "form-label"),
          shiny::selectInput(ns("right_doc"),
            label    = NULL,
            choices  = stats::setNames(docs$id, docs$name),
            selected = if (length(docs$id) >= 2L) as.character(docs$id[[2L]])
                       else                         as.character(docs$id[[1L]]),
            width   = "240px")
        )
      } else {
        coders <- coders_rv()
        shiny::div(
          shiny::tags$label("Right coder", class = "form-label"),
          shiny::selectInput(ns("right_coder"),
            label    = NULL,
            choices  = c("(all)" = "", stats::setNames(coders, coders)),
            selected = if (length(coders) >= 2L) coders[[2L]] else "",
            width   = "180px")
        )
      }
    })

    # ── Left side data ────────────────────────────────────────────────────────

    left_doc_rv <- shiny::reactive({
      input$btn_refresh
      shiny::req(input$mode)
      if (input$mode == "docs") {
        shiny::req(input$left_doc)
        qc_get_document(rv$project, as.integer(input$left_doc))
      } else {
        shiny::req(input$shared_doc)
        qc_get_document(rv$project, as.integer(input$shared_doc))
      }
    })

    left_codings_rv <- shiny::reactive({
      input$btn_refresh
      shiny::req(left_doc_rv())
      coder <- if (input$mode == "coders") {
        cv <- input$left_coder %||% ""
        if (nchar(cv) > 0L) cv else NULL
      } else NULL
      qc_list_codings(rv$project, left_doc_rv()$id, coder = coder)
    })

    # ── Right side data ───────────────────────────────────────────────────────

    right_doc_rv <- shiny::reactive({
      input$btn_refresh
      shiny::req(input$mode)
      if (input$mode == "docs") {
        shiny::req(input$right_doc)
        qc_get_document(rv$project, as.integer(input$right_doc))
      } else {
        shiny::req(input$shared_doc)
        qc_get_document(rv$project, as.integer(input$shared_doc))
      }
    })

    right_codings_rv <- shiny::reactive({
      input$btn_refresh
      shiny::req(right_doc_rv())
      coder <- if (input$mode == "coders") {
        cv <- input$right_coder %||% ""
        if (nchar(cv) > 0L) cv else NULL
      } else NULL
      qc_list_codings(rv$project, right_doc_rv()$id, coder = coder)
    })

    # ── Panel titles ──────────────────────────────────────────────────────────

    output$left_title <- shiny::renderText({
      shiny::req(left_doc_rv())
      if (input$mode == "coders") {
        lc <- input$left_coder %||% "(all coders)"
        paste0(left_doc_rv()$name, " — ", if (nchar(lc) > 0L) lc else "(all coders)")
      } else {
        left_doc_rv()$name
      }
    })

    output$right_title <- shiny::renderText({
      shiny::req(right_doc_rv())
      if (input$mode == "coders") {
        rc <- input$right_coder %||% "(all coders)"
        paste0(right_doc_rv()$name, " — ", if (nchar(rc) > 0L) rc else "(all coders)")
      } else {
        right_doc_rv()$name
      }
    })

    # ── Text display ──────────────────────────────────────────────────────────

    output$left_text <- shiny::renderUI({
      shiny::req(left_doc_rv())
      build_highlighted_html(left_doc_rv()$content, left_codings_rv())
    })

    output$right_text <- shiny::renderUI({
      shiny::req(right_doc_rv())
      build_highlighted_html(right_doc_rv()$content, right_codings_rv())
    })

    # ── Scroll sync ───────────────────────────────────────────────────────────

    shiny::observe({
      session$sendCustomMessage("qc_compare_sync",
        list(enabled = isTRUE(input$sync_scroll)))
    })

    # ── Differences ───────────────────────────────────────────────────────────

    diff_rv <- shiny::reactive({
      lc    <- left_codings_rv()
      rc    <- right_codings_rv()
      mode  <- input$mode %||% "docs"

      left_label  <- if (mode == "coders") {
        lv <- input$left_coder  %||% ""; if (nchar(lv) > 0L) lv else "Left"
      } else {
        ld <- left_doc_rv();  if (!is.null(ld)) ld$name else "Left"
      }
      right_label <- if (mode == "coders") {
        rv2 <- input$right_coder %||% ""; if (nchar(rv2) > 0L) rv2 else "Right"
      } else {
        rd <- right_doc_rv(); if (!is.null(rd)) rd$name else "Right"
      }

      if (mode == "docs") {
        .doc_diff(lc, rc, left_label, right_label)
      } else {
        .coder_diff(lc, rc, left_label, right_label)
      }
    })

    output$diff_summary <- shiny::renderUI({
      df <- diff_rv()
      if (is.null(df) || nrow(df) == 0L) return(NULL)
      if ("status" %in% names(df)) {
        counts <- table(df$status)
        parts  <- vapply(names(counts), function(s)
          paste0(counts[[s]], " ", s), character(1L))
        shiny::p(shiny::tags$small(
          paste(parts, collapse = ", "), class = "text-muted"
        ))
      } else {
        shiny::p(shiny::tags$small(
          paste0(nrow(df), " codes compared"), class = "text-muted"
        ))
      }
    })

    output$tbl_diff <- DT::renderDataTable({
      df <- diff_rv()
      if (is.null(df) || nrow(df) == 0L) {
        return(DT::datatable(
          tibble::tibble(message = "No differences found."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      DT::datatable(df,
        class    = "table table-hover",
        rownames = FALSE,
        options  = list(pageLength = 20, dom = "ftp", scrollX = TRUE)
      )
    })

    # ── Reliability statistics ─────────────────────────────────────────────────

    output$reliability_card <- shiny::renderUI({
      if (input$mode != "coders") return(NULL)
      bslib::card(
        class = "mt-3",
        bslib::card_header(
          shiny::div(
            class = "d-flex justify-content-between align-items-center w-100",
            "Reliability Statistics",
            shiny::actionButton(ns("btn_reliability"), "Compute",
              class = "btn-sm btn-outline-secondary")
          )
        ),
        shiny::div(
          class = "p-2",
          qc_help_note(
            "Cohen's Kappa measures agreement between two coders on a single code, ",
            "beyond chance. Values > 0.6 suggest substantial agreement."
          ),
          shiny::uiOutput(ns("reliability_summary")),
          DT::dataTableOutput(ns("tbl_reliability"))
        )
      )
    })

    reliability_rv <- shiny::eventReactive(input$btn_reliability, {
      tryCatch({
        qc_agreement_matrix(rv$project)
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        NULL
      })
    }, ignoreNULL = FALSE)

    output$reliability_summary <- shiny::renderUI({
      df <- reliability_rv()
      if (is.null(df) || nrow(df) == 0L) return(NULL)
      mean_kappa  <- round(mean(df$kappa, na.rm = TRUE), 3L)
      mean_pct    <- round(mean(df$pct_agree, na.rm = TRUE), 1L)
      n_pairs     <- nrow(df)
      shiny::div(
        class = "d-flex gap-3 mb-3 flex-wrap",
        shiny::div(class = "qc-stat-pill",
          shiny::span(class = "qc-stat-value", mean_kappa),
          shiny::span(class = "qc-stat-label", "mean kappa")
        ),
        shiny::div(class = "qc-stat-pill",
          shiny::span(class = "qc-stat-value", paste0(mean_pct, "%")),
          shiny::span(class = "qc-stat-label", "mean % agreement")
        ),
        shiny::div(class = "qc-stat-pill",
          shiny::span(class = "qc-stat-value", n_pairs),
          shiny::span(class = "qc-stat-label", "code-pair comparisons")
        )
      )
    })

    output$tbl_reliability <- DT::renderDataTable({
      df <- reliability_rv()
      if (is.null(df)) return(NULL)
      if (nrow(df) == 0L) {
        return(DT::datatable(
          tibble::tibble(message = paste0(
            "No shared codings found. Both coders need to have coded ",
            "the same documents for statistics to be computed."
          )),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      display <- dplyr::select(df, code_name, coder1, coder2,
                                n_docs, pct_agree, kappa, n11, n10, n01)
      DT::datatable(
        display,
        class    = "table table-hover table-sm",
        rownames = FALSE,
        options  = list(
          pageLength = 25, dom = "ftp",
          order = list(list(5, "asc")),
          columnDefs = list(
            list(targets = c(3, 6, 7, 8), width = "70px",
                 className = "text-center"),
            list(targets = 4, width = "80px", className = "text-center")
          )
        ),
        colnames = c("Code", "Coder 1", "Coder 2",
                     "Docs", "% Agree", "Kappa",
                     "Both", "Coder 1 only", "Coder 2 only")
      ) |>
        DT::formatRound("kappa", digits = 3L) |>
        DT::formatRound("pct_agree", digits = 1L) |>
        DT::formatStyle("kappa",
          backgroundColor = DT::styleInterval(
            c(0.4, 0.6, 0.8),
            c("#fce4e4", "#fff3cd", "#d4edda", "#c3e6cb")
          )
        )
    })
  })
}

# ── Helpers ────────────────────────────────────────────────────────────────────

# For docs mode: code-level count comparison across two documents.
.doc_diff <- function(left_c, right_c, left_label, right_label) {
  lnames <- if (nrow(left_c)  > 0L) left_c$code_name  else character(0)
  rnames <- if (nrow(right_c) > 0L) right_c$code_name else character(0)
  all_codes <- sort(unique(c(lnames, rnames)))
  if (length(all_codes) == 0L) return(tibble::tibble())

  lt <- table(lnames); rt <- table(rnames)
  lc <- as.integer(lt[all_codes]); lc[is.na(lc)] <- 0L
  rc <- as.integer(rt[all_codes]); rc[is.na(rc)] <- 0L

  df <- tibble::tibble(code_name = all_codes, l = lc, r = rc)
  names(df)[2L] <- left_label
  names(df)[3L] <- right_label
  df
}

# For coders mode: segment-level comparison showing unique/agreed/conflict rows.
.coder_diff <- function(left_c, right_c, left_label, right_label) {
  if (nrow(left_c) == 0L && nrow(right_c) == 0L) return(tibble::tibble())

  make_rows <- function(codings, label) {
    if (nrow(codings) == 0L) return(NULL)
    data.frame(
      coder_label = label,
      code_name   = codings$code_name,
      selfirst    = codings$selfirst,
      selast      = codings$selast,
      seltext     = substr(codings$seltext, 1L, 80L),
      stringsAsFactors = FALSE
    )
  }

  lrows <- make_rows(left_c,  left_label)
  rrows <- make_rows(right_c, right_label)
  combined <- do.call(rbind, Filter(Negate(is.null), list(lrows, rrows)))
  if (is.null(combined) || nrow(combined) == 0L) return(tibble::tibble())
  combined <- combined[order(combined$selfirst), ]

  # Label each row: "agreed", "conflict", or "unique"
  combined$status <- vapply(seq_len(nrow(combined)), function(i) {
    side  <- combined$coder_label[[i]]
    s1    <- combined$selfirst[[i]]
    e1    <- combined$selast[[i]]
    code1 <- combined$code_name[[i]]
    other <- if (side == left_label) rrows else lrows
    if (is.null(other)) return("unique")
    ov <- other$selfirst <= e1 & other$selast >= s1
    if (!any(ov)) return("unique")
    if (any(ov & other$code_name == code1)) "agreed" else "conflict"
  }, character(1L))

  tibble::as_tibble(combined)
}
