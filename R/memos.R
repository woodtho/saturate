#' Add an entry to the project analytical journal
#'
#' Appends a timestamped memo to the project-level reflexivity / analytical
#' journal. Entries are append-only (no updates, only soft deletes) so the
#' research audit trail is preserved.
#'
#' @param project A `qc_project` object.
#' @param content Character. The memo text (supports Markdown).
#' @param type Character. One of `"analytical"`, `"reflexivity"`, `"decision"`,
#'   `"methodological"`, or any custom label.
#' @param created_by Character or `NULL`. Researcher identifier; defaults to
#'   the system username.
#'
#' @return A one-row tibble: `id`, `content`, `memo_type`, `created_by`,
#'   `created_at`.
#' @export
qc_add_project_memo <- function(project, content,
                                 type       = "analytical",
                                 created_by = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!is_string(content)) rlang::abort("`content` must be a single string.")
  by <- created_by %||% Sys.info()[["user"]] %||% ""
  .query(project$con,
    "INSERT INTO project_memos (content, memo_type, created_by)
     VALUES (?, ?, ?)
     RETURNING id, content, memo_type, created_by, created_at",
    list(content, type %||% "analytical", by)
  )
}

#' List project journal entries
#'
#' @param project A `qc_project` object.
#' @param type Character or `NULL`. Filter to a specific memo type; pass `NULL`
#'   to return all types.
#'
#' @return A tibble: `id`, `content`, `memo_type`, `created_by`, `created_at`,
#'   ordered newest-first.
#' @export
qc_list_project_memos <- function(project, type = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (is.null(type)) {
    .query(project$con,
      "SELECT id, content, memo_type, created_by, created_at
       FROM   project_memos
       WHERE  status = 1
       ORDER  BY created_at DESC"
    )
  } else {
    .query(project$con,
      "SELECT id, content, memo_type, created_by, created_at
       FROM   project_memos
       WHERE  memo_type = ? AND status = 1
       ORDER  BY created_at DESC",
      list(as.character(type))
    )
  }
}

# -- Internal export helpers ---------------------------------------------------

.export_memos_docx <- function(project) {
  if (!requireNamespace("officer", quietly = TRUE))
    rlang::abort("Install the 'officer' package to export Word documents.")

  info <- qc_project_info(project)
  df   <- qc_list_project_memos(project)
  doc  <- officer::read_docx()

  doc <- officer::body_add_par(doc,
    paste0(info$name, " \u2014 Research Journal"), style = "heading 1")
  doc <- officer::body_add_par(doc,
    paste0("Exported ", format(Sys.Date(), "%B %d, %Y"),
           " \u2014 ", nrow(df), " entr", if (nrow(df) == 1L) "y" else "ies"),
    style = "Normal")

  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    ts  <- format(as.POSIXct(row$created_at), "%d %b %Y %H:%M")
    doc <- officer::body_add_break(doc)
    doc <- officer::body_add_par(doc,
      paste0(tools::toTitleCase(row$memo_type %||% "other"),
             "  \u2014  ", ts, "  \u2014  ", row$created_by),
      style = "heading 2")
    doc <- officer::body_add_par(doc, as.character(row$content), style = "Normal")
  }

  path <- tempfile(fileext = ".docx")
  print(doc, target = path)
  path
}

.export_memos_html <- function(project) {
  info <- qc_project_info(project)
  df   <- qc_list_project_memos(project)

  type_colour <- c(
    analytical     = "#3b82f6",
    reflexivity    = "#06b6d4",
    decision       = "#f59e0b",
    methodological = "#6b7280",
    other          = "#374151"
  )

  esc <- htmltools::htmlEscape

  pieces <- c(
    "<!DOCTYPE html><html lang='en'><head>",
    "<meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    paste0("<title>", esc(info$name), " \u2014 Journal</title>"),
    "<style>",
    "body{font-family:Georgia,'Times New Roman',serif;max-width:760px;margin:2rem auto;padding:0 1.5rem;color:#1a1a1a;line-height:1.7}",
    "h1{font-size:1.6rem;border-bottom:3px solid #2c3e50;padding-bottom:.5rem;color:#2c3e50;margin-bottom:.25rem}",
    ".meta{color:#6b7280;font-size:.85rem;margin:.5rem 0 2rem}",
    ".entry{margin-bottom:2rem;padding-bottom:1.5rem;border-bottom:1px solid #e9ecef}",
    ".entry-header{display:flex;flex-wrap:wrap;gap:.6rem;align-items:baseline;margin-bottom:.6rem}",
    ".badge{display:inline-block;border-radius:3px;padding:2px 8px;font-size:.75rem;font-weight:600;color:#fff}",
    ".entry-meta{font-size:.82rem;color:#6b7280}",
    ".entry-content{white-space:pre-wrap;margin:0}",
    "</style></head><body>",
    paste0("<h1>", esc(info$name), " \u2014 Research Journal</h1>"),
    paste0("<p class='meta'>Exported ", format(Sys.Date(), "%B %d, %Y"),
           " &mdash; ", nrow(df), " entr", if (nrow(df) == 1L) "y" else "ies", "</p>")
  )

  if (nrow(df) == 0L) {
    pieces <- c(pieces, "<p><em>No journal entries.</em></p>")
  } else {
    for (i in seq_len(nrow(df))) {
      row    <- df[i, , drop = FALSE]
      ts     <- format(as.POSIXct(row$created_at), "%d %b %Y %H:%M")
      colour <- type_colour[[row$memo_type %||% "other"]] %||% "#374151"
      pieces <- c(pieces,
        "<div class='entry'>",
        "<div class='entry-header'>",
        sprintf("<span class='badge' style='background:%s'>%s</span>",
                esc(colour), esc(tools::toTitleCase(row$memo_type %||% "other"))),
        sprintf("<span class='entry-meta'>%s &mdash; %s</span>",
                esc(ts), esc(as.character(row$created_by))),
        "</div>",
        paste0("<p class='entry-content'>", esc(as.character(row$content)), "</p>"),
        "</div>"
      )
    }
  }

  pieces <- c(pieces, "</body></html>")
  path   <- tempfile(fileext = ".html")
  writeLines(paste(pieces, collapse = "\n"), path, useBytes = FALSE)
  path
}

#' Export the project analytical journal
#'
#' Writes all journal entries to a file. Supported formats: `"docx"` (Word),
#' `"html"`, `"txt"` (plain text), and `"csv"`.
#'
#' @param project A `qc_project` object.
#' @param path Character. Output file path. If `NULL`, a temp file is created
#'   and its path returned.
#' @param format Character. One of `"docx"`, `"html"`, `"txt"`, `"csv"`.
#'
#' @return The output file path, invisibly.
#' @export
qc_export_journal <- function(project, path = NULL, format = "docx") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  format <- match.arg(format, c("docx", "html", "txt", "csv"))

  tmp <- switch(format,
    docx = .export_memos_docx(project),
    html = .export_memos_html(project),
    txt  = {
      df  <- qc_list_project_memos(project)
      out <- tempfile(fileext = ".txt")
      lines <- character(0)
      for (i in seq_len(nrow(df))) {
        row   <- df[i, , drop = FALSE]
        ts    <- format(as.POSIXct(row$created_at), "%d %b %Y %H:%M")
        lines <- c(lines,
          paste0("[", toupper(row$memo_type), "] ", ts, " -- ", row$created_by),
          as.character(row$content), "")
      }
      writeLines(lines, out)
      out
    },
    csv  = {
      df  <- qc_list_project_memos(project)
      out <- tempfile(fileext = ".csv")
      utils::write.csv(df, out, row.names = FALSE)
      out
    }
  )

  if (is.null(path)) return(invisible(tmp))
  file.copy(tmp, path, overwrite = TRUE)
  invisible(path)
}

#' Delete a project journal entry (soft delete)
#'
#' @param project A `qc_project` object.
#' @param id Integer. Memo id.
#'
#' @return Invisibly `NULL`.
#' @export
qc_delete_project_memo <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE project_memos SET status = 0 WHERE id = ?",
    list(as.integer(id)))
  invisible(NULL)
}
