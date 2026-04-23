mod_member_check_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "p-3",

    # ── Checks table ───────────────────────────────────────────────────────────
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center w-100",
          "Member Checks",
          shiny::actionButton(ns("btn_new_check"), "New Check",
            class = "btn-sm btn-primary")
        )
      ),
      bslib::card_body(
        qc_help_note(
          "Create a member check when you want a participant or stakeholder to ",
          "review coded passages. Select a row to view exported items and record ",
          "responses."
        ),
        DT::dataTableOutput(ns("tbl_checks"))
      )
    ),

    # ── Selected check detail ──────────────────────────────────────────────────
    bslib::card(
      class = "mt-3",
      bslib::card_header(shiny::textOutput(ns("detail_title"))),
      bslib::card_body(
        qc_help_details(
          "Response recording help",
          shiny::p(
            "Export a check for review, then record each item as confirmed, ",
            "disputed, other, or pending. Bulk actions should only be used when ",
            "the same status applies to every item in the selected check."
          )
        ),
        shiny::div(
          class = "d-flex gap-2 mb-3 flex-wrap",
          shiny::downloadButton(ns("btn_dl_html"), "Export HTML",
            class = "btn-sm btn-outline-secondary"),
          shiny::downloadButton(ns("btn_dl_txt"),  "Export TXT",
            class = "btn-sm btn-outline-secondary"),
          shiny::downloadButton(ns("btn_dl_docx"), "Export Word",
            class = "btn-sm btn-outline-secondary"),
          shiny::div(class = "ms-auto d-flex gap-2",
            shiny::actionButton(ns("btn_confirm_all"), "Confirm All",
              class = "btn-sm btn-success"),
            shiny::actionButton(ns("btn_dispute_all"), "Dispute All",
              class = "btn-sm btn-danger")
          )
        ),
        shiny::uiOutput(ns("check_items_ui"))
      )
    )
  )
}

mod_member_check_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    lv  <- shiny::reactiveValues(
      selected_check_id = NULL,
      refresh_checks    = 0L
    )

    # ── Checks table ──────────────────────────────────────────────────────────

    checks_rv <- shiny::reactive({
      rv$refresh_docs
      lv$refresh_checks
      qc_list_member_checks(rv$project)
    })

    output$tbl_checks <- DT::renderDataTable({
      df <- checks_rv()
      if (nrow(df) == 0L) {
        return(DT::datatable(
          tibble::tibble(message = "No member checks yet. Click 'New Check' to create one."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      disp <- df
      disp$sent_at     <- format(df$sent_at, "%Y-%m-%d")
      disp$response_at <- ifelse(is.na(df$response_at), "—",
                                 format(df$response_at, "%Y-%m-%d"))
      DT::datatable(
        disp[, c("id", "doc_name", "participant_label", "status",
                 "n_items", "n_confirmed", "n_disputed",
                 "sent_at", "response_at")],
        class     = "table table-hover",
        selection = "single",
        rownames  = FALSE,
        colnames  = c("ID", "Document", "Participant", "Status",
                      "Items", "✓", "✗", "Sent", "Response"),
        options   = list(
          pageLength = 15, dom = "ftp",
          columnDefs = list(
            list(width = "50px",  targets = 0),
            list(width = "80px",  targets = c(3, 4, 5, 6)),
            list(width = "100px", targets = c(7, 8))
          )
        )
      )
    })

    # ── New Check modal ────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_new_check, {
      docs  <- qc_list_documents(rv$project)
      codes <- qc_list_codes(rv$project)
      doc_choices  <- stats::setNames(docs$id,  docs$name)
      code_choices <- stats::setNames(codes$id, codes$name)

      shiny::showModal(shiny::modalDialog(
        title     = "New Member Check",
        size      = "l",
        easyClose = FALSE,

        shiny::selectInput(ns("mc_source_id"), "Document",
          choices = doc_choices),
        shiny::textInput(ns("mc_participant"), "Participant label",
          placeholder = "e.g. Participant A"),
        shiny::selectizeInput(ns("mc_code_ids"),
          "Restrict to codes (optional — blank = all)",
          choices  = code_choices,
          multiple = TRUE,
          options  = list(placeholder = "All codes")),

        shiny::hr(),
        shiny::h6("Return instructions"),
        shiny::textInput(ns("mc_return_by"), "Return by",
          placeholder = "e.g. 2026-05-01 or 'within 2 weeks'"),
        shiny::textInput(ns("mc_return_to"), "Return to (email / contact)",
          placeholder = "e.g. researcher@university.edu"),
        shiny::textAreaInput(ns("mc_return_instructions"),
          "Instructions for participant",
          rows = 3,
          placeholder = paste0(
            "e.g. Please read each passage and note whether the interpretation ",
            "captures your experience. Return via email.")),

        shiny::hr(),
        shiny::h6("Participant’s written response (optional)"),
        shiny::tags$small(
          class = "text-muted d-block mb-1",
          "Paste the participant’s reply here to record it alongside the check."
        ),
        shiny::textAreaInput(ns("mc_paste_response"), NULL,
          rows = 5,
          placeholder = "Paste participant response text…"),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("btn_confirm_create_check"), "Create Check",
            class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_create_check, {
      shiny::req(
        nchar(trimws(input$mc_participant %||% "")) > 0,
        input$mc_source_id
      )
      code_ids <- if (length(input$mc_code_ids) > 0L)
        as.integer(input$mc_code_ids) else NULL
      tryCatch({
        qc_create_member_check(
          rv$project,
          source_id           = as.integer(input$mc_source_id),
          participant_label   = trimws(input$mc_participant),
          code_ids            = code_ids,
          created_by          = rv$current_coder %||% Sys.info()[["user"]],
          return_by           = trimws(input$mc_return_by           %||% ""),
          return_to           = trimws(input$mc_return_to           %||% ""),
          return_instructions = trimws(input$mc_return_instructions %||% ""),
          notes               = trimws(input$mc_paste_response      %||% "")
        )
        shiny::removeModal()
        shiny::showNotification("Member check created.", type = "message")
        lv$refresh_checks <- lv$refresh_checks + 1L
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Row selection → detail ────────────────────────────────────────────────

    shiny::observeEvent(input$tbl_checks_rows_selected, {
      row <- input$tbl_checks_rows_selected
      shiny::req(row)
      lv$selected_check_id <- checks_rv()$id[[row]]
    })

    # ── Detail panel title ────────────────────────────────────────────────────

    output$detail_title <- shiny::renderText({
      id_val <- lv$selected_check_id
      if (is.null(id_val)) return("Select a check to view details")
      checks <- checks_rv()
      check  <- checks[checks$id == id_val, ]
      if (nrow(check) == 0L) return("Check not found")
      paste0("Check #", id_val, " — ",
             check$participant_label, " | ", check$doc_name,
             " [", check$status, "]")
    })

    # ── Bulk actions ──────────────────────────────────────────────────────────

    shiny::observeEvent(input$btn_confirm_all, {
      shiny::req(lv$selected_check_id)
      tryCatch({
        qc_bulk_set_member_status(rv$project, lv$selected_check_id,
                                   status = "confirmed")
        shiny::showNotification("All items confirmed.", type = "message")
        lv$refresh_checks <- lv$refresh_checks + 1L
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    shiny::observeEvent(input$btn_dispute_all, {
      shiny::req(lv$selected_check_id)
      tryCatch({
        qc_bulk_set_member_status(rv$project, lv$selected_check_id,
                                   status = "disputed")
        shiny::showNotification("All items disputed.", type = "message")
        lv$refresh_checks <- lv$refresh_checks + 1L
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # ── Download handlers ─────────────────────────────────────────────────────

    output$btn_dl_html <- shiny::downloadHandler(
      filename = function() {
        id_val <- shiny::isolate(lv$selected_check_id)
        paste0("member_check_", id_val %||% "export", ".html")
      },
      content = function(file) {
        id_val <- shiny::isolate(lv$selected_check_id)
        shiny::req(id_val)
        html <- qc_export_member_check(rv$project, id_val, format = "html")
        writeLines(html, file, useBytes = TRUE)
      }
    )

    output$btn_dl_txt <- shiny::downloadHandler(
      filename = function() {
        id_val <- shiny::isolate(lv$selected_check_id)
        paste0("member_check_", id_val %||% "export", ".txt")
      },
      content = function(file) {
        id_val <- shiny::isolate(lv$selected_check_id)
        shiny::req(id_val)
        txt <- qc_export_member_check(rv$project, id_val, format = "txt")
        writeLines(txt, file, useBytes = TRUE)
      }
    )

    output$btn_dl_docx <- shiny::downloadHandler(
      filename = function() {
        id_val <- shiny::isolate(lv$selected_check_id)
        paste0("member_check_", id_val %||% "export", ".docx")
      },
      content = function(file) {
        id_val <- shiny::isolate(lv$selected_check_id)
        if (is.null(id_val)) return()
        if (!requireNamespace("officer", quietly = TRUE)) {
          shiny::showNotification(
            "Install the 'officer' package to export Word documents.",
            type = "error")
          return()
        }
        tryCatch({
          tmp <- qc_export_member_check(rv$project, id_val, format = "docx")
          file.copy(tmp, file)
        }, error = function(e) {
          shiny::showNotification(
            paste0("DOCX export failed: ", conditionMessage(e)),
            type = "error")
        })
      }
    )

    # ── Check items UI ────────────────────────────────────────────────────────

    output$check_items_ui <- shiny::renderUI({
      id_val <- lv$selected_check_id
      lv$refresh_checks
      if (is.null(id_val)) {
        return(shiny::p(class = "text-muted",
                        "Select a row in the table above to record responses."))
      }

      checks <- checks_rv()
      check  <- checks[checks$id == id_val, ]

      ret_hdr <- if (nrow(check) > 0L) {
        ret_by  <- trimws(check$return_by[[1L]]  %||% "")
        ret_to  <- trimws(check$return_to[[1L]]  %||% "")
        ret_ins <- trimws(check$return_instructions[[1L]] %||% "")
        notes_v <- trimws(check$notes[[1L]] %||% "")
        has_ret <- any(nchar(c(ret_by, ret_to, ret_ins)) > 0L)

        blocks <- list()
        if (has_ret) {
          blocks <- c(blocks, list(shiny::div(
            class = "alert alert-light border mb-3",
            style = "font-size:0.875rem;",
            if (nchar(ret_ins) > 0L) shiny::p(class = "mb-1", ret_ins),
            shiny::div(
              class = "d-flex gap-4",
              if (nchar(ret_by) > 0L)
                shiny::span(shiny::tags$strong("Return by: "), ret_by),
              if (nchar(ret_to) > 0L)
                shiny::span(shiny::tags$strong("Return to: "), ret_to)
            )
          )))
        }
        if (nchar(notes_v) > 0L) {
          blocks <- c(blocks, list(shiny::div(
            class = "qc-code-info mb-3",
            shiny::tags$strong("Participant response:"),
            shiny::tags$br(),
            shiny::tags$span(
              style = "white-space:pre-wrap;font-size:0.875rem;",
              notes_v)
          )))
        }
        if (length(blocks) > 0L) do.call(shiny::tagList, blocks)
      }

      items <- tryCatch(.query(rv$project$con,
        "SELECT mci.coding_id, mci.item_status, mci.participant_response,
                cod.seltext, cod.memo, c.name AS code_name, c.color AS code_color
         FROM   member_check_items mci
         JOIN   codings cod ON cod.id = mci.coding_id
         JOIN   codes   c   ON c.id  = cod.code_id
         WHERE  mci.check_id = ?
         ORDER  BY cod.selfirst",
        list(id_val)
      ), error = function(e) tibble::tibble())

      if (nrow(items) == 0L) {
        return(shiny::tagList(
          ret_hdr,
          shiny::p(class = "text-muted", "No items in this check.")
        ))
      }

      shiny::tagList(
        ret_hdr,
        shiny::h6(paste0(nrow(items), " item(s) — click Save to record each response")),
        lapply(seq_len(nrow(items)), function(i) {
          r          <- items[i, ]
          col        <- r$code_color %||% "#4E79A7"
          item_pfx   <- paste0("item_", id_val, "_", r$coding_id, "_")
          badge_cls  <- switch(r$item_status,
            confirmed = "badge bg-success",
            disputed  = "badge bg-danger",
            "badge bg-secondary"
          )
          shiny::div(
            class = "mb-3 p-2",
            style = paste0("border-left:3px solid ", col,
                           ";background:var(--sat-surface-card);border-radius:4px;"),
            shiny::div(
              class = "d-flex justify-content-between align-items-center mb-1",
              shiny::tags$small(
                class = "text-muted text-uppercase",
                style = "letter-spacing:.04em;",
                r$code_name
              ),
              shiny::tags$span(class = badge_cls, r$item_status)
            ),
            shiny::p(
              class = "fst-italic mb-2",
              style = "font-size:.9rem;",
              paste0("“", substr(r$seltext, 1L, 250L),
                     if (nchar(r$seltext) > 250L) "…" else "",
                     "”")
            ),
            shiny::div(
              class = "d-flex gap-2 align-items-start",
              shiny::div(
                style = "min-width:160px;",
                shiny::selectInput(ns(paste0(item_pfx, "status")), NULL,
                  choices  = c("Pending"   = "pending",
                               "Confirmed" = "confirmed",
                               "Disputed"  = "disputed",
                               "Other"     = "other"),
                  selected = r$item_status)
              ),
              shiny::div(
                class = "flex-grow-1",
                shiny::textInput(ns(paste0(item_pfx, "response")), NULL,
                  value       = r$participant_response %||% "",
                  placeholder = "Participant comment (optional)")
              ),
              shiny::actionButton(
                ns(paste0(item_pfx, "save")),
                "Save",
                class   = "btn-sm btn-outline-primary mt-1",
                onclick = paste0(
                  "Shiny.setInputValue('", ns("record_response"), "',",
                  "{check_id:", id_val, ",coding_id:", r$coding_id,
                  ",ts:Date.now()},{priority:'event'});"
                )
              )
            )
          )
        })
      )
    })

    # ── Record response (JS → R bridge) ──────────────────────────────────────

    shiny::observeEvent(input$record_response, {
      payload   <- input$record_response
      shiny::req(payload$check_id, payload$coding_id)
      check_id  <- as.integer(payload$check_id)
      coding_id <- as.integer(payload$coding_id)
      pfx       <- paste0("item_", check_id, "_", coding_id, "_")
      status_v  <- input[[paste0(pfx, "status")]]   %||% "pending"
      resp_v    <- input[[paste0(pfx, "response")]] %||% ""
      tryCatch({
        qc_record_member_response(rv$project, check_id, coding_id,
                                   response = resp_v, status = status_v)
        shiny::showNotification("Response recorded.", type = "message")
        lv$refresh_checks <- lv$refresh_checks + 1L
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })
  })
}
