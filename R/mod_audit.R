mod_audit_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "p-3",

    # ── Filters ───────────────────────────────────────────────────────────────
    bslib::card(
      bslib::card_body(
        shiny::div(
          class = "d-flex gap-3 align-items-end flex-wrap",

          shiny::div(
            shiny::tags$label("Event type", class = "form-label"),
            shiny::selectInput(ns("filter_type"),
              label   = NULL,
              choices = c("All" = "all", "Coding" = "coding", "Code" = "code"),
              width   = "150px")
          ),

          shiny::div(
            shiny::tags$label("Operation", class = "form-label"),
            shiny::selectInput(ns("filter_op"),
              label   = NULL,
              choices = c("All" = "", "create", "delete", "update", "reassign"),
              width   = "150px")
          ),

          shiny::uiOutput(ns("filter_doc_ui")),

          shiny::div(
            shiny::tags$label("From", class = "form-label"),
            shiny::dateInput(ns("filter_from"), label = NULL,
              value = NULL, width = "140px")
          ),

          shiny::div(
            shiny::tags$label("To", class = "form-label"),
            shiny::dateInput(ns("filter_to"), label = NULL,
              value = NULL, width = "140px")
          ),

          shiny::div(
            class = "d-flex gap-2 align-items-end",
            shiny::actionButton(ns("btn_refresh"), "Refresh",
              class = "btn-sm btn-outline-secondary"),
            shiny::downloadButton(ns("btn_export"), "Export CSV",
              class = "btn-sm btn-outline-secondary")
          )
        )
      )
    ),

    qc_help_details(
      "Audit help",
      shiny::p(
        "Use the audit trail to review project changes, reconstruct coding ",
        "decisions, and export a defensible record of code and coding edits."
      ),
      qc_help_list(c(
        "Event type separates coding changes from codebook changes.",
        "Operation filters actions such as create, update, delete, and reassign.",
        "Date filters are inclusive for the selected To day."
      ))
    ),

    # ── Summary badges ────────────────────────────────────────────────────────
    shiny::uiOutput(ns("summary_badges")),

    # ── Unified audit table ───────────────────────────────────────────────────
    bslib::card(
      bslib::card_header("Audit Trail"),
      shiny::div(
        class = "p-2",
        DT::dataTableOutput(ns("tbl_audit"))
      )
    )
  )
}

mod_audit_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Document picker ───────────────────────────────────────────────────────

    docs_rv <- shiny::reactive({
      rv$refresh_docs
      qc_list_documents(rv$project)
    })

    output$filter_doc_ui <- shiny::renderUI({
      docs <- docs_rv()
      shiny::div(
        shiny::tags$label("Document", class = "form-label"),
        shiny::selectInput(session$ns("filter_doc"),
          label   = NULL,
          choices = c("All" = "", stats::setNames(docs$id, docs$name)),
          width   = "220px")
      )
    })

    # ── Combined audit data ───────────────────────────────────────────────────

    audit_rv <- shiny::reactive({
      input$btn_refresh
      rv$refresh_codes

      type    <- input$filter_type %||% "all"
      op      <- input$filter_op   %||% ""
      doc_id  <- input$filter_doc  %||% ""
      from_dt <- input$filter_from
      to_dt   <- input$filter_to

      src_id  <- if (nchar(doc_id) > 0L) as.integer(doc_id) else NULL
      op_val  <- if (nchar(op)     > 0L) op else NULL
      from_v  <- if (!is.null(from_dt) && !is.na(from_dt)) as.POSIXct(from_dt) else NULL
      to_v    <- if (!is.null(to_dt)   && !is.na(to_dt))
        as.POSIXct(to_dt) + 86400 else NULL  # inclusive of the whole to-day

      rows <- list()

      # Coding audit rows
      if (type %in% c("all", "coding")) {
        ca <- tryCatch(
          qc_coding_audit(rv$project,
            source_id = src_id,
            operation = op_val,
            from_date = from_v,
            to_date   = to_v),
          error = function(e) tibble::tibble()
        )
        if (nrow(ca) > 0L) {
          rows[["coding"]] <- tibble::tibble(
            event_type  = "coding",
            operation   = ca$operation,
            field       = ca$field %||% NA_character_,
            old_value   = ca$old_value,
            new_value   = ca$new_value,
            document    = ca$source_name,
            code        = ca$code_name,
            text        = .trunc(ca$seltext, 80L),
            coder       = ca$coder,
            changed_by  = ca$changed_by,
            changed_at  = ca$changed_at
          )
        }
      }

      # Code history rows
      if (type %in% c("all", "code") && is.null(src_id)) {
        ch <- tryCatch(
          qc_code_history(rv$project),
          error = function(e) tibble::tibble()
        )
        if (!is.null(ch) && nrow(ch) > 0L) {
          if (!is.null(op_val))
            ch <- ch[ch$operation == op_val, ]
          if (!is.null(from_v))
            ch <- ch[as.POSIXct(ch$changed_at) >= from_v, ]
          if (!is.null(to_v))
            ch <- ch[as.POSIXct(ch$changed_at) <= to_v, ]

          if (nrow(ch) > 0L) {
            rows[["code"]] <- tibble::tibble(
              event_type  = "code",
              operation   = ch$operation,
              field       = ch$field       %||% NA_character_,
              old_value   = ch$old_value   %||% NA_character_,
              new_value   = ch$new_value   %||% NA_character_,
              document    = NA_character_,
              code        = ch$code_name   %||% NA_character_,
              text        = NA_character_,
              coder       = NA_character_,
              changed_by  = NA_character_,
              changed_at  = ch$changed_at
            )
          }
        }
      }

      if (length(rows) == 0L) {
        return(tibble::tibble(
          event_type = character(), operation = character(), field = character(),
          old_value  = character(), new_value = character(),
          document   = character(), code      = character(),
          text       = character(), coder     = character(),
          changed_by = character(), changed_at = as.POSIXct(character())
        ))
      }

      combined <- do.call(rbind, rows)
      combined[order(combined$changed_at, decreasing = TRUE), ]
    })

    # ── Summary badges ────────────────────────────────────────────────────────

    output$summary_badges <- shiny::renderUI({
      df <- audit_rv()
      if (nrow(df) == 0L) return(NULL)

      counts <- table(df$event_type, df$operation)
      badges <- character()
      for (etype in rownames(counts)) {
        for (op in colnames(counts)) {
          n <- counts[etype, op]
          if (n > 0L)
            badges <- c(badges, paste0(n, " ", etype, " ", op))
        }
      }

      shiny::div(
        class = "d-flex flex-wrap gap-2 mb-3",
        lapply(badges, function(b)
          shiny::tags$span(class = "badge bg-secondary", b)
        )
      )
    })

    # ── Audit table ───────────────────────────────────────────────────────────

    output$tbl_audit <- DT::renderDataTable({
      df <- audit_rv()

      if (nrow(df) == 0L) {
        return(DT::datatable(
          tibble::tibble(message = "No audit entries match the current filters."),
          rownames = FALSE,
          options  = list(dom = "t")
        ))
      }

      # Format timestamp
      df$changed_at <- format(df$changed_at, "%Y-%m-%d %H:%M:%S")

      DT::datatable(df,
        class    = "table table-hover table-sm",
        rownames = FALSE,
        colnames = c("Type", "Operation", "Field", "Old value", "New value",
                     "Document", "Code", "Text", "Coder", "Changed by",
                     "Changed at"),
        options  = list(
          pageLength = 25,
          dom        = "ftp",
          scrollX    = TRUE,
          columnDefs = list(
            list(width = "80px",  targets = c(0, 1, 2)),
            list(width = "120px", targets = c(3, 4)),
            list(width = "160px", targets = c(5, 6)),
            list(width = "200px", targets = 7),
            list(width = "90px",  targets = c(8, 9)),
            list(width = "140px", targets = 10)
          )
        )
      )
    })

    # ── CSV export ────────────────────────────────────────────────────────────

    output$btn_export <- shiny::downloadHandler(
      filename = function() {
        paste0("audit_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df <- audit_rv()
        if (nrow(df) > 0L)
          df$changed_at <- format(df$changed_at, "%Y-%m-%d %H:%M:%S")
        utils::write.csv(df, file, row.names = FALSE)
      }
    )
  })
}

# ── Internal helper ────────────────────────────────────────────────────────────

.trunc <- function(x, n) {
  ifelse(!is.na(x) & nchar(x) > n,
         paste0(substr(x, 1L, n), "…"),
         x)
}
