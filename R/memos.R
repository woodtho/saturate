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
