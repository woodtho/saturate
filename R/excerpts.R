#' Add an excerpt to a document
#'
#' Creates a labelled passage (selfirst–selast) within a document, separate
#' from any coding. Excerpts can carry a memo and are displayed in the coding
#' view as a distinct underline highlight.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param selfirst Integer. 1-based start character position.
#' @param selast Integer. 1-based end character position (inclusive).
#' @param memo Character. Note about why this passage was excerpted.
#' @param coder Character. Coder identifier.
#'
#' @return A one-row tibble: `id`, `source_id`, `selfirst`, `selast`,
#'   `seltext`, `memo`, `coder`, `created_at`.
#' @export
qc_add_excerpt <- function(project, source_id, selfirst, selast,
                            memo = "", coder = "default") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  source_id <- as.integer(source_id)
  selfirst  <- as.integer(selfirst)
  selast    <- as.integer(selast)
  if (selfirst > selast) rlang::abort("`selfirst` must be <= `selast`.")

  content <- .query(project$con,
    "SELECT content FROM sources WHERE id = ? AND status = 1",
    list(source_id))$content
  if (length(content) == 0L) rlang::abort("Document not found.")
  seltext <- substr(content, selfirst, selast)

  .query(project$con,
    "INSERT INTO excerpts (source_id, selfirst, selast, seltext, memo, coder)
     VALUES (?, ?, ?, ?, ?, ?)
     RETURNING id, source_id, selfirst, selast, seltext, memo, coder, created_at",
    list(source_id, selfirst, selast, seltext, memo %||% "", coder %||% "default")
  )
}

#' List excerpts
#'
#' @param project A `qc_project` object.
#' @param source_id Integer or `NULL`. Filter to a single document.
#'
#' @return A tibble: `id`, `source_id`, `source_name`, `selfirst`, `selast`,
#'   `seltext`, `memo`, `coder`, `created_at`.
#' @export
qc_list_excerpts <- function(project, source_id = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  if (is.null(source_id)) {
    .query(project$con,
      "SELECT e.id, e.source_id, s.name AS source_name,
              e.selfirst, e.selast, e.seltext, e.memo, e.coder, e.created_at
       FROM   excerpts e
       JOIN   sources  s ON s.id = e.source_id
       WHERE  e.status = 1
       ORDER  BY e.source_id, e.selfirst"
    )
  } else {
    .query(project$con,
      "SELECT e.id, e.source_id, s.name AS source_name,
              e.selfirst, e.selast, e.seltext, e.memo, e.coder, e.created_at
       FROM   excerpts e
       JOIN   sources  s ON s.id = e.source_id
       WHERE  e.source_id = ? AND e.status = 1
       ORDER  BY e.selfirst",
      list(as.integer(source_id))
    )
  }
}

#' Update an excerpt's memo
#'
#' @param project A `qc_project` object.
#' @param id Integer. Excerpt id.
#' @param memo Character. New memo text.
#'
#' @return Invisibly `NULL`.
#' @export
qc_update_excerpt_memo <- function(project, id, memo) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE excerpts SET memo = ? WHERE id = ? AND status = 1",
    list(as.character(memo %||% ""), as.integer(id)))
  invisible(NULL)
}

#' Delete an excerpt (soft delete)
#'
#' @param project A `qc_project` object.
#' @param id Integer. Excerpt id.
#'
#' @return Invisibly `NULL`.
#' @export
qc_delete_excerpt <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE excerpts SET status = 0 WHERE id = ? AND status = 1",
    list(as.integer(id)))
  invisible(NULL)
}
