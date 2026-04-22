#' Create a member check record
#'
#' Records that a set of coded segments has been prepared for participant
#' review. Use [qc_export_member_check()] to generate the shareable document
#' and [qc_record_member_response()] to log the participant's feedback.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param participant_label Character. Identifier for the participant (e.g.
#'   name, pseudonym, or participant ID).
#' @param code_ids Integer vector or `NULL`. Restrict to specific codes. `NULL`
#'   includes all active codings on the document.
#' @param created_by Character or `NULL`. Records who created this check.
#'   Defaults to the current system user.
#'
#' @return A one-row tibble: `id`, `source_id`, `participant_label`, `status`,
#'   `sent_at`.
#' @export
qc_create_member_check <- function(project, source_id, participant_label,
                                    code_ids = NULL, created_by = NULL,
                                    return_by = "", return_to = "",
                                    return_instructions = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)

  source_id           <- as.integer(source_id)
  participant_label   <- as.character(participant_label)
  created_by          <- created_by %||% Sys.info()[["user"]]
  code_ids_str        <- if (!is.null(code_ids))
    paste(as.integer(code_ids), collapse = ",") else ""
  return_by           <- as.character(return_by %||% "")
  return_to           <- as.character(return_to %||% "")
  return_instructions <- as.character(return_instructions %||% "")

  doc <- qc_get_document(project, source_id)

  check_row <- .query(project$con,
    "INSERT INTO member_checks
       (source_id, participant_label, code_ids_filter, created_by,
        return_by, return_to, return_instructions)
     VALUES (?, ?, ?, ?, ?, ?, ?)
     RETURNING id, source_id, participant_label, status, sent_at",
    list(source_id, participant_label, code_ids_str, created_by,
         return_by, return_to, return_instructions)
  )

  codings <- qc_list_codings(project, source_id)
  if (!is.null(code_ids))
    codings <- codings[codings$code_id %in% as.integer(code_ids), ]

  if (nrow(codings) > 0L) {
    for (i in seq_len(nrow(codings))) {
      .exec(project$con,
        "INSERT INTO member_check_items (check_id, coding_id) VALUES (?, ?)",
        list(check_row$id, codings$id[[i]])
      )
    }
  }

  cli::cli_alert_success(
    "Member check #{check_row$id} created for '{participant_label}' on '{doc$name}'.")
  check_row
}

#' Export a member check as a shareable document
#'
#' Generates an HTML or plain-text document showing the coded segments for
#' participant review.
#'
#' @param project A `qc_project` object.
#' @param check_id Integer. Member check id (from [qc_list_member_checks()]).
#' @param path Character or `NULL`. Output file path. When `NULL`, returns the
#'   content as a character string.
#' @param format One of `"html"` (default) or `"txt"`.
#'
#' @return Invisibly, the file path written, or the content string when
#'   `path = NULL`.
#' @export
qc_export_member_check <- function(project, check_id, path = NULL,
                                    format = c("html", "txt", "docx")) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  format   <- match.arg(format)
  check_id <- as.integer(check_id)

  check <- .query(project$con,
    "SELECT mc.id,
            mc.source_id,
            mc.participant_label,
            mc.code_ids_filter,
            mc.created_by,
            mc.sent_at,
            mc.response_at,
            mc.status,
            mc.notes,
            COALESCE(mc.return_by,           '') AS return_by,
            COALESCE(mc.return_to,           '') AS return_to,
            COALESCE(mc.return_instructions, '') AS return_instructions,
            s.name AS doc_name
     FROM   member_checks mc
     JOIN   sources s ON s.id = mc.source_id
     WHERE  mc.id = ?",
    list(check_id)
  )
  if (nrow(check) == 0L)
    rlang::abort(paste0("No member check with id = ", check_id))

  items <- .query(project$con,
    "SELECT mci.coding_id, mci.item_status, mci.participant_response,
            cod.selfirst, cod.selast, cod.seltext, cod.memo,
            c.name  AS code_name,
            c.color AS code_color
     FROM   member_check_items mci
     JOIN   codings cod ON cod.id = mci.coding_id
     JOIN   codes   c   ON c.id  = cod.code_id
     WHERE  mci.check_id = ?
     ORDER  BY cod.selfirst",
    list(check_id)
  )

  proj_info <- qc_project_info(project)

  if (format == "docx") {
    if (!requireNamespace("officer", quietly = TRUE))
      rlang::abort("Package 'officer' is required for DOCX export. Install with: install.packages('officer')")
    doc <- .mc_docx(check, items, proj_info)
    if (!is.null(path)) {
      print(doc, target = path)
      cli::cli_alert_success("Member check exported to {.file {path}}")
      invisible(path)
    } else {
      tmp <- tempfile(fileext = ".docx")
      print(doc, target = tmp)
      invisible(tmp)
    }
  } else {
    out <- if (format == "txt")
      .mc_txt(check, items, proj_info)
    else
      .mc_html(check, items, proj_info)
    if (!is.null(path)) {
      writeLines(out, path, useBytes = TRUE)
      cli::cli_alert_success("Member check exported to {.file {path}}")
      invisible(path)
    } else {
      invisible(out)
    }
  }
}

#' Set all items in a member check to the same status
#'
#' Convenience helper for bulk confirm or dispute. Calls
#' [qc_record_member_response()] for every item in the check.
#'
#' @param project A `qc_project` object.
#' @param check_id Integer. Member check id.
#' @param status One of `"confirmed"` or `"disputed"`.
#'
#' @return Invisibly, the updated check list.
#' @export
qc_bulk_set_member_status <- function(project, check_id,
                                       status = c("confirmed", "disputed")) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  status   <- match.arg(status)
  check_id <- as.integer(check_id)

  items <- .query(project$con,
    "SELECT coding_id FROM member_check_items WHERE check_id = ?",
    list(check_id)
  )
  for (cid in items$coding_id) {
    qc_record_member_response(project, check_id, cid,
                               response = "", status = status)
  }
  invisible(qc_list_member_checks(project))
}

#' Record a participant's response to a member check item
#'
#' Updates one coding item within a member check. After every update the
#' overall check status is recalculated: `"confirmed"` when all items are
#' confirmed, `"disputed"` when any item is disputed, `"partial"` when mixed,
#' `"pending"` when nothing has been recorded yet.
#'
#' @param project A `qc_project` object.
#' @param check_id Integer. Member check id.
#' @param coding_id Integer. Coding id to update.
#' @param response Character. Free-text participant comment.
#' @param status One of `"confirmed"`, `"disputed"`, or `"other"`.
#'
#' @return Invisibly, the updated check list (from [qc_list_member_checks()]).
#' @export
qc_record_member_response <- function(project, check_id, coding_id,
                                       response = "",
                                       status = c("confirmed", "disputed", "other")) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  status    <- match.arg(status)
  check_id  <- as.integer(check_id)
  coding_id <- as.integer(coding_id)

  .exec(project$con,
    "UPDATE member_check_items
     SET    item_status          = ?,
            participant_response = ?,
            response_at          = now()
     WHERE  check_id = ? AND coding_id = ?",
    list(status, response, check_id, coding_id)
  )

  statuses <- .query(project$con,
    "SELECT item_status FROM member_check_items WHERE check_id = ?",
    list(check_id)
  )$item_status

  overall <- if      (all(statuses == "confirmed")) "confirmed"
             else if (any(statuses == "disputed"))  "disputed"
             else if (all(statuses == "pending"))   "pending"
             else                                   "partial"

  .exec(project$con,
    "UPDATE member_checks SET status = ?, response_at = now() WHERE id = ?",
    list(overall, check_id)
  )

  invisible(qc_list_member_checks(project))
}

#' List all member checks in the project
#'
#' @param project A `qc_project` object.
#' @param source_id Integer or `NULL`. Restrict to a single document.
#'
#' @return A tibble: `id`, `doc_name`, `participant_label`, `status`,
#'   `n_items`, `n_confirmed`, `n_disputed`, `sent_at`, `response_at`.
#' @export
qc_list_member_checks <- function(project, source_id = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  w_src <- if (!is.null(source_id))
    paste0("AND mc.source_id = ", as.integer(source_id)) else ""

  .query(project$con, paste0("
    SELECT mc.id,
           s.name                AS doc_name,
           mc.participant_label,
           mc.status,
           mc.sent_at,
           mc.response_at,
           mc.notes,
           COALESCE(mc.return_by,           '') AS return_by,
           COALESCE(mc.return_to,           '') AS return_to,
           COALESCE(mc.return_instructions, '') AS return_instructions,
           COUNT(mci.id)         AS n_items,
           SUM(CASE WHEN mci.item_status = 'confirmed' THEN 1 ELSE 0 END)
                                 AS n_confirmed,
           SUM(CASE WHEN mci.item_status = 'disputed'  THEN 1 ELSE 0 END)
                                 AS n_disputed
    FROM   member_checks mc
    JOIN   sources s             ON s.id = mc.source_id
    LEFT   JOIN member_check_items mci ON mci.check_id = mc.id
    WHERE  1 = 1 ", w_src, "
    GROUP  BY mc.id, s.name, mc.participant_label, mc.status,
              mc.sent_at, mc.response_at, mc.notes,
              mc.return_by, mc.return_to, mc.return_instructions
    ORDER  BY mc.sent_at DESC
  "))
}

# ── Private export builders ────────────────────────────────────────────────────

.mc_html <- function(check, items, proj_info) {
  esc <- function(x) htmltools::htmlEscape(as.character(x %||% ""))

  items_html <- if (nrow(items) == 0L) {
    "<p><em>No coded passages to review.</em></p>"
  } else {
    paste(vapply(seq_len(nrow(items)), function(i) {
      r     <- items[i, ]
      col   <- r$code_color %||% "#4E79A7"
      extra <- if (!is.na(r$memo) && nchar(r$memo) > 0L)
        paste0("<p class='note'><strong>Researcher note:</strong> ", esc(r$memo), "</p>")
      else ""
      paste0(
        "<div class='passage'>",
        "<p class='code-label'>", esc(r$code_name), "</p>",
        "<blockquote style='border-left-color:", col, ";'>",
        "&ldquo;", esc(r$seltext), "&rdquo;",
        "</blockquote>",
        extra,
        "<p class='prompt'>Does this interpretation match your experience?",
        " <strong>Yes &nbsp;/&nbsp; No &nbsp;/&nbsp; Other:</strong>",
        " <span class='blank'></span></p>",
        "</div>"
      )
    }, character(1L)), collapse = "\n")
  }

  paste0("<!DOCTYPE html>
<html lang='en'>
<head>
<meta charset='UTF-8'>
<meta name='viewport' content='width=device-width,initial-scale=1'>
<title>Member Check &mdash; ", esc(check$doc_name), "</title>
<style>
body{font-family:Georgia,serif;max-width:720px;margin:2rem auto;padding:0 1rem;
     line-height:1.75;color:#222;}
h1{font-size:1.35rem;margin-bottom:.2rem;}
.meta{color:#666;font-size:.85rem;margin-bottom:2rem;}
h2{font-size:.95rem;margin:2rem 0 .5rem;text-transform:uppercase;
   letter-spacing:.06em;color:#444;border-bottom:1px solid #ddd;
   padding-bottom:.25rem;}
.passage{margin:1.5rem 0;padding:1rem 1rem 1rem 1.25rem;
         background:#fafafa;border-radius:4px;}
.code-label{margin:0 0 .4rem;font-size:.75rem;color:#666;
            text-transform:uppercase;letter-spacing:.06em;}
blockquote{margin:.25rem 0 .75rem;padding-left:1rem;
           border-left:4px solid #4E79A7;font-style:italic;color:#333;}
.note{margin:.25rem 0;font-size:.85rem;color:#555;}
.prompt{margin:.5rem 0 0;font-size:.875rem;}
.blank{display:inline-block;min-width:12rem;border-bottom:1px solid #999;}
.general{min-height:100px;border:1px solid #ccc;padding:.5rem;
         border-radius:4px;margin:.5rem 0;}
footer{margin-top:3rem;padding-top:1rem;border-top:1px solid #ddd;
       font-size:.75rem;color:#aaa;}
</style>
</head>
<body>
<h1>Member Check</h1>
<div class='meta'>
  <strong>Project:</strong> ", esc(proj_info$name), "<br>
  <strong>Document:</strong> ", esc(check$doc_name), "<br>
  <strong>Participant:</strong> ", esc(check$participant_label), "<br>
  <strong>Date:</strong> ", esc(format(check$sent_at, "%d %B %Y")), "
</div>
<p>We have identified the following themes in your responses. Please indicate
whether each interpretation accurately reflects your experience.</p>
<h2>Coded passages</h2>
", items_html, "
<h2>General comments</h2>
<div class='general'>&nbsp;</div>
<h2>Return instructions</h2>
", if (nchar(check$return_instructions[[1L]] %||% "") > 0L)
    paste0("<p>", esc(check$return_instructions[[1L]]), "</p>")
  else "", "
<p>",
  if (nchar(check$return_by[[1L]] %||% "") > 0L)
    paste0("<strong>Return by:</strong> ", esc(check$return_by[[1L]]), "<br>")
  else "Please return by: <strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</strong><br>",
"",
  if (nchar(check$return_to[[1L]] %||% "") > 0L)
    paste0("<strong>Return to:</strong> ", esc(check$return_to[[1L]]))
  else "Return to: <strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</strong>",
"</p>
<footer>Generated by saturate &bull; ", esc(format(Sys.time(), "%Y-%m-%d %H:%M")), "</footer>
</body>
</html>")
}

.mc_txt <- function(check, items, proj_info) {
  sep <- strrep("-", 60)
  header <- paste0(
    "MEMBER CHECK\n", strrep("=", 60), "\n",
    "Project:     ", proj_info$name,          "\n",
    "Document:    ", check$doc_name,           "\n",
    "Participant: ", check$participant_label,  "\n",
    "Date:        ", format(check$sent_at, "%d %B %Y"), "\n",
    strrep("=", 60), "\n\n",
    "Please review the following passages and indicate whether each\n",
    "interpretation accurately reflects your experience.\n\n"
  )

  if (nrow(items) == 0L) return(paste0(header, "(No coded passages.)\n"))

  items_txt <- paste(vapply(seq_len(nrow(items)), function(i) {
    r <- items[i, ]
    paste0(
      "[", i, "] CODE: ", r$code_name, "\n", sep, "\n",
      '"', trimws(r$seltext), '"', "\n\n",
      if (!is.na(r$memo) && nchar(r$memo) > 0L)
        paste0("Researcher note: ", r$memo, "\n\n") else "",
      "Accurate? Yes / No / Comment: __________________________________\n"
    )
  }, character(1L)), collapse = "\n")

  ret_by  <- trimws(check$return_by[[1L]]  %||% "")
  ret_to  <- trimws(check$return_to[[1L]]  %||% "")
  ret_ins <- trimws(check$return_instructions[[1L]] %||% "")

  return_block <- paste0(
    strrep("=", 60), "\n",
    "Return instructions\n",
    strrep("-", 60), "\n",
    if (nchar(ret_ins) > 0L) paste0(ret_ins, "\n") else "",
    if (nchar(ret_by) > 0L)
      paste0("Return by: ", ret_by, "\n")
    else
      "Return by: ______________________________\n",
    if (nchar(ret_to) > 0L)
      paste0("Return to: ", ret_to, "\n")
    else
      "Return to: ______________________________\n"
  )

  paste0(header, items_txt, "\n",
         strrep("=", 60), "\n",
         "General comments:\n\n\n",
         return_block,
         strrep("=", 60), "\n",
         "Generated by saturate | ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
}

.mc_docx <- function(check, items, proj_info) {
  doc <- officer::read_docx()

  add <- function(d, ...) officer::body_add_par(d, ...)

  doc <- add(doc, "Member Check", style = "heading 1")
  doc <- add(doc, paste0("Project: ",     as.character(proj_info$name[[1L]])))
  doc <- add(doc, paste0("Document: ",    as.character(check$doc_name[[1L]])))
  doc <- add(doc, paste0("Participant: ", as.character(check$participant_label[[1L]])))
  doc <- add(doc, paste0("Date: ",        format(check$sent_at[[1L]], "%d %B %Y")))
  doc <- add(doc, "")
  doc <- add(doc,
    "We have identified the following themes in your responses. Please indicate whether each interpretation accurately reflects your experience.")
  doc <- add(doc, "")
  doc <- add(doc, "Coded Passages", style = "heading 2")

  if (nrow(items) == 0L) {
    doc <- add(doc, "(No coded passages to review.)")
  } else {
    for (i in seq_len(nrow(items))) {
      r <- items[i, ]
      doc <- add(doc, as.character(r$code_name[[1L]]), style = “heading 3”)
      doc <- add(doc, paste0(““”, trimws(as.character(r$seltext[[1L]])), “””))
      memo_v <- as.character(r$memo[[1L]] %||% "")
      if (!is.na(memo_v) && nchar(memo_v) > 0L)
        doc <- add(doc, paste0("Researcher note: ", memo_v))
      doc <- add(doc, "Does this interpretation match your experience? Yes / No / Comment:")
      doc <- add(doc, "")
    }
  }

  doc <- add(doc, "General Comments", style = "heading 2")
  doc <- add(doc, "")
  doc <- add(doc, "")

  ret_by  <- trimws(check$return_by[[1L]]  %||% "")
  ret_to  <- trimws(check$return_to[[1L]]  %||% "")
  ret_ins <- trimws(check$return_instructions[[1L]] %||% "")

  doc <- add(doc, "Return Instructions", style = "heading 2")
  if (nchar(ret_ins) > 0L) doc <- add(doc, ret_ins)
  doc <- add(doc, if (nchar(ret_by) > 0L)
    paste0("Return by: ", ret_by) else "Return by: ________________________________")
  doc <- add(doc, if (nchar(ret_to) > 0L)
    paste0("Return to: ", ret_to) else "Return to: ________________________________")
  doc <- add(doc, "")
  doc <- add(doc,
    paste0("Generated by saturate • ", format(Sys.time(), "%Y-%m-%d %H:%M")))

  doc
}
