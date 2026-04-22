#' Add a theme
#'
#' Creates a reflexive TA theme object. A theme differs from a code in that it
#' captures a pattern of *meaning* rather than a descriptive label: the
#' `central_concept` names the core idea, and `narrative` holds the analytic
#' story the researcher writes to justify the theme.
#'
#' @param project A `qc_project` object.
#' @param name Character. Short theme label.
#' @param central_concept Character. The organising idea of the theme in one
#'   sentence.
#' @param narrative Character. Extended analytical justification.
#'
#' @return A one-row tibble: `id`, `name`, `central_concept`, `narrative`,
#'   `created_at`.
#' @export
qc_add_theme <- function(project, name, central_concept = "", narrative = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!is_string(name)) rlang::abort("`name` must be a single string.")
  .query(project$con,
    "INSERT INTO themes (name, central_concept, narrative)
     VALUES (?, ?, ?)
     RETURNING id, name, central_concept, narrative, created_at",
    list(name, central_concept %||% "", narrative %||% "")
  )
}

#' List themes
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble: `id`, `name`, `central_concept`, `narrative`, `created_at`,
#'   `n_codes` (number of codes linked to each theme).
#' @export
qc_list_themes <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .query(project$con,
    "SELECT t.id, t.name, t.central_concept, t.narrative, t.created_at,
            COUNT(l.code_id) AS n_codes
     FROM   themes t
     LEFT   JOIN theme_code_links l
            ON l.theme_id = t.id AND l.status = 1
     WHERE  t.status = 1
     GROUP  BY t.id, t.name, t.central_concept, t.narrative, t.created_at
     ORDER  BY t.name"
  )
}

#' Update a theme's fields
#'
#' @param project A `qc_project` object.
#' @param id Integer. Theme id.
#' @param name,central_concept,narrative Character or `NULL`. Fields to update.
#'
#' @return Invisibly `NULL`.
#' @export
qc_update_theme <- function(project, id,
                             name            = NULL,
                             central_concept = NULL,
                             narrative       = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  id <- as.integer(id)
  updates <- list(name = name, central_concept = central_concept,
                  narrative = narrative)
  for (col in names(updates)) {
    val <- updates[[col]]
    if (is.null(val)) next
    .exec(project$con,
      paste0("UPDATE themes SET ", col, " = ? WHERE id = ? AND status = 1"),
      list(as.character(val), id))
  }
  invisible(NULL)
}

#' Delete a theme (soft delete)
#'
#' @param project A `qc_project` object.
#' @param id Integer. Theme id.
#'
#' @return Invisibly `NULL`.
#' @export
qc_delete_theme <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE themes SET status = 0 WHERE id = ? AND status = 1",
    list(as.integer(id)))
  invisible(NULL)
}

#' Link a code to a theme
#'
#' @param project A `qc_project` object.
#' @param theme_id Integer. Theme id.
#' @param code_id Integer. Code id.
#'
#' @return Invisibly `NULL`.
#' @export
qc_link_theme_code <- function(project, theme_id, code_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "INSERT INTO theme_code_links (theme_id, code_id)
     VALUES (?, ?)
     ON CONFLICT (theme_id, code_id) DO UPDATE SET status = 1",
    list(as.integer(theme_id), as.integer(code_id)))
  invisible(NULL)
}

#' Unlink a code from a theme
#'
#' @param project A `qc_project` object.
#' @param theme_id Integer. Theme id.
#' @param code_id Integer. Code id.
#'
#' @return Invisibly `NULL`.
#' @export
qc_unlink_theme_code <- function(project, theme_id, code_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE theme_code_links SET status = 0
     WHERE theme_id = ? AND code_id = ?",
    list(as.integer(theme_id), as.integer(code_id)))
  invisible(NULL)
}
