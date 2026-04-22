#' Add an annotation to a document
#'
#' Annotations are free-text notes attached to a character position (or the
#' document as a whole) and are distinct from coded segments — they do not
#' assign a code label, they express a thought or question about the text.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param annotation Character. The annotation text.
#' @param position Integer or `NULL`. 1-based character position in the
#'   document. `NULL` attaches the annotation to the document as a whole.
#' @param coder Character. Coder identifier (default `"default"`).
#'
#' @return A one-row tibble: `id`, `source_id`, `position`, `annotation`,
#'   `coder`, `created_at`.
#' @export
qc_add_annotation <- function(project, source_id, annotation,
                               position = NULL, coder = "default") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  if (!is_string(annotation)) rlang::abort("`annotation` must be a string.")
  source_id <- as.integer(source_id)

  if (is.null(position)) {
    .query(project$con,
      "INSERT INTO annotations (source_id, annotation, coder)
       VALUES (?, ?, ?)
       RETURNING id, source_id, position, annotation, coder, created_at",
      list(source_id, annotation, coder %||% "default")
    )
  } else {
    .query(project$con,
      "INSERT INTO annotations (source_id, position, annotation, coder)
       VALUES (?, ?, ?, ?)
       RETURNING id, source_id, position, annotation, coder, created_at",
      list(source_id, as.integer(position), annotation,
           coder %||% "default")
    )
  }
}

#' List annotations
#'
#' @param project A `qc_project` object.
#' @param source_id Integer or `NULL`. Restrict to a single document.
#' @param coder Character or `NULL`. Restrict to a single coder.
#'
#' @return A tibble: `id`, `source_id`, `source_name`, `position`,
#'   `annotation`, `coder`, `created_at`. Ordered by document then position.
#' @export
qc_list_annotations <- function(project, source_id = NULL, coder = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  conds  <- "a.status = 1"
  params <- list()
  if (!is.null(source_id)) {
    conds  <- paste0(conds, " AND a.source_id = ?")
    params <- c(params, list(as.integer(source_id)))
  }
  if (!is.null(coder)) {
    conds  <- paste0(conds, " AND a.coder = ?")
    params <- c(params, list(as.character(coder)))
  }
  .query(project$con, paste0("
    SELECT a.id, a.source_id, s.name AS source_name,
           a.position, a.annotation, a.coder, a.created_at
    FROM   annotations a
    JOIN   sources s ON s.id = a.source_id
    WHERE  ", conds, "
    ORDER  BY a.source_id, a.position NULLS LAST
  "), params)
}

#' Update annotation text
#'
#' @param project A `qc_project` object.
#' @param id Integer. Annotation id.
#' @param annotation Character. New text.
#'
#' @return Invisibly `TRUE`.
#' @export
qc_update_annotation <- function(project, id, annotation) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  if (!is_string(annotation)) rlang::abort("`annotation` must be a string.")
  .exec(project$con,
    "UPDATE annotations SET annotation = ? WHERE id = ? AND status = 1",
    list(annotation, as.integer(id))
  )
  invisible(TRUE)
}

#' Delete an annotation (soft delete)
#'
#' @param project A `qc_project` object.
#' @param id Integer. Annotation id.
#'
#' @return Invisibly `1L`.
#' @export
qc_delete_annotation <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  .soft_delete(project$con, "annotations", "id", as.integer(id))
  invisible(1L)
}
