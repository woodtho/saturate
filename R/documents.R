#' Import a text document into the project
#'
#' @param project A `qc_project` object.
#' @param path Character. Path to a file. When `NULL`, `content` must be supplied.
#' @param content Character scalar. Raw document text (used when `path = NULL`).
#' @param name Character. Display name. Defaults to filename without extension.
#' @param memo Character. Initial memo text.
#'
#' @return A one-row tibble: `id`, `name`, `created_at`.
#' @export
qc_import_document <- function(project,
                               path    = NULL,
                               content = NULL,
                               name    = NULL,
                               memo    = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)

  .assert_unlocked(project)
  if (!is.null(path) && !is.null(content))
    rlang::abort("Supply `path` or `content`, not both.")
  if (is.null(path) && is.null(content))
    rlang::abort("Supply either `path` or `content`.")

  if (!is.null(path)) {
    path <- fs::path_abs(path)
    if (!fs::file_exists(path)) rlang::abort(paste0("File not found: ", path))
    if (is.null(name)) name <- fs::path_ext_remove(fs::path_file(path))
    if (requireNamespace("readtext", quietly = TRUE)) {
      rt      <- readtext::readtext(path)
      content <- rt$text[[1L]]
    } else {
      content <- paste(readLines(path, warn = FALSE), collapse = "\n")
    }
  }

  if (!is_string(content)) rlang::abort("`content` must be a single string.")
  if (is.null(name)) rlang::abort("`name` must be supplied when `path = NULL`.")
  if (!is_string(name)) rlang::abort("`name` must be a single string.")

  .query(project$con,
    "INSERT INTO sources (name, content, memo) VALUES (?, ?, ?) RETURNING id, name, created_at",
    list(name, content, memo %||% "")
  )
}

#' List all documents in the project
#'
#' @param project A `qc_project` object.
#' @param include_content Logical. Include the full `content` column.
#'
#' @return A tibble with columns `id`, `name`, `memo`, `n_codings`,
#'   `created_at` (and `content` when `include_content = TRUE`).
#' @export
qc_list_documents <- function(project, include_content = FALSE) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  content_col <- if (include_content) "s.content," else ""
  .query(project$con, paste0("
    SELECT s.id, s.name, ", content_col, " s.memo,
           COUNT(c.id) AS n_codings,
           s.created_at
    FROM   sources s
    LEFT   JOIN codings c ON c.source_id = s.id AND c.status = 1
    WHERE  s.status = 1
    GROUP  BY s.id, s.name, ", if (include_content) "s.content, " else "", "s.memo, s.created_at
    ORDER  BY s.created_at
  "))
}

#' Retrieve a single document's full text
#'
#' @param project A `qc_project` object.
#' @param id Integer. The document id.
#'
#' @return A one-row tibble: `id`, `name`, `content`, `memo`.
#' @export
qc_get_document <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  out <- .query(project$con,
    "SELECT id, name, content, memo FROM sources WHERE id = ? AND status = 1",
    list(as.integer(id))
  )
  if (nrow(out) == 0L) rlang::abort(paste0("No document with id = ", id))
  out
}

#' Update the memo on a document
#'
#' @param project A `qc_project` object.
#' @param id Integer. Document id.
#' @param memo Character. New memo text.
#'
#' @return The updated one-row tibble (same shape as `qc_get_document()`).
#' @export
qc_update_document_memo <- function(project, id, memo) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE sources SET memo = ? WHERE id = ? AND status = 1",
    list(memo, as.integer(id))
  )
  qc_get_document(project, id)
}

#' Remove a document (soft delete)
#'
#' Also soft-deletes all codings attached to this document.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Document id.
#'
#' @return Invisibly, the number of codings also soft-deleted.
#' @export
qc_delete_document <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id <- as.integer(id)
  n  <- .exec(project$con,
    "UPDATE codings SET status = 0 WHERE source_id = ? AND status = 1",
    list(id)
  )
  .soft_delete(project$con, "sources", "id", id)
  invisible(n)
}
