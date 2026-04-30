#' Add a case (respondent / subject)
#'
#' @param project A `qc_project` object.
#' @param name Character. Case label. Must be unique.
#' @param memo Character.
#'
#' @return A one-row tibble: `id`, `name`, `memo`, `created_at`.
#' @export
qc_add_case <- function(project, name, memo = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!is_string(name)) rlang::abort("`name` must be a single string.")
  .query(project$con,
    "INSERT INTO cases (name, memo) VALUES (?, ?) RETURNING id, name, memo, created_at",
    list(name, memo %||% "")
  )
}

#' List all cases
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble: `id`, `name`, `memo`, `n_sources`, `created_at`.
#' @export
qc_list_cases <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .query(project$con, "
    SELECT ca.id, ca.name, ca.memo,
           COUNT(DISTINCT l.source_id) AS n_sources,
           ca.created_at
    FROM   cases ca
    LEFT   JOIN case_source_links l ON l.case_id = ca.id AND l.status = 1
    WHERE  ca.status = 1
    GROUP  BY ca.id, ca.name, ca.memo, ca.created_at
    ORDER  BY ca.name
  ")
}

#' Link a case to a document
#'
#' @param project A `qc_project` object.
#' @param case_id Integer.
#' @param source_id Integer.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_link_case_source <- function(project, case_id, source_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "INSERT INTO case_source_links (case_id, source_id)
     VALUES (?, ?)
     ON CONFLICT (case_id, source_id) DO UPDATE SET status = 1",
    list(as.integer(case_id), as.integer(source_id))
  )
  invisible(TRUE)
}

#' Remove a case-document link
#'
#' @param project A `qc_project` object.
#' @param case_id Integer.
#' @param source_id Integer.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_unlink_case_source <- function(project, case_id, source_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE case_source_links SET status = 0
     WHERE case_id = ? AND source_id = ?",
    list(as.integer(case_id), as.integer(source_id))
  )
  invisible(TRUE)
}

#' Set a case attribute value (upsert)
#'
#' @param project A `qc_project` object.
#' @param case_id Integer.
#' @param variable Character. Attribute name (e.g. `"age_group"`).
#' @param value Character.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_set_case_attribute <- function(project, case_id, variable, value) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "INSERT INTO case_attributes (case_id, variable, value)
     VALUES (?, ?, ?)
     ON CONFLICT (case_id, variable)
     DO UPDATE SET value = excluded.value, status = 1",
    list(as.integer(case_id), variable, as.character(value))
  )
  invisible(TRUE)
}

#' Update a case name and/or memo
#'
#' @param project A `qc_project` object.
#' @param case_id Integer.
#' @param name Character. New name (or `NULL` to leave unchanged).
#' @param memo Character. New memo (or `NULL` to leave unchanged).
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_update_case <- function(project, case_id, name = NULL, memo = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!is.null(name)) {
    if (!is_string(name)) rlang::abort("`name` must be a single string.")
    .exec(project$con,
      "UPDATE cases SET name = ? WHERE id = ? AND status = 1",
      list(trimws(name), as.integer(case_id))
    )
  }
  if (!is.null(memo)) {
    .exec(project$con,
      "UPDATE cases SET memo = ? WHERE id = ? AND status = 1",
      list(as.character(memo), as.integer(case_id))
    )
  }
  invisible(TRUE)
}

#' Soft-delete a case
#'
#' @param project A `qc_project` object.
#' @param case_id Integer.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_delete_case <- function(project, case_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .soft_delete(project$con, "cases", "id", as.integer(case_id))
  invisible(TRUE)
}

#' List attributes for one case (long format)
#'
#' @param project A `qc_project` object.
#' @param case_id Integer.
#'
#' @return A tibble: `variable`, `value`.
#' @export
qc_list_case_attributes <- function(project, case_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .query(project$con,
    "SELECT variable, value FROM case_attributes
     WHERE case_id = ? AND status = 1
     ORDER BY variable",
    list(as.integer(case_id))
  )
}

#' Delete a case attribute
#'
#' @param project A `qc_project` object.
#' @param case_id Integer.
#' @param variable Character. Attribute name to remove.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_delete_case_attribute <- function(project, case_id, variable) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE case_attributes SET status = 0 WHERE case_id = ? AND variable = ?",
    list(as.integer(case_id), as.character(variable))
  )
  invisible(TRUE)
}

#' Get all case attributes as a wide tibble
#'
#' Pivots the EAV `case_attributes` table into one column per variable name.
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble: `case_id`, `case_name`, then one column per
#'   attribute variable.
#' @export
qc_case_attributes_wide <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  raw <- .query(project$con, "
    SELECT ca.id AS case_id, ca.name AS case_name,
           a.variable, a.value
    FROM   cases ca
    LEFT   JOIN case_attributes a ON a.case_id = ca.id AND a.status = 1
    WHERE  ca.status = 1
    ORDER  BY ca.name, a.variable
  ")
  if (all(is.na(raw$variable))) {
    return(dplyr::select(raw, case_id, case_name) |> dplyr::distinct())
  }
  tidyr::pivot_wider(raw,
    id_cols     = c("case_id", "case_name"),
    names_from  = "variable",
    values_from = "value"
  )
}
