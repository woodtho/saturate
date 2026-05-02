# -- Internal DB helpers --------------------------------------------------------

.db_list_profiles <- function(project) {
  .query(project$con,
    "SELECT name, settings_json, created_at, last_used_at
     FROM profiles WHERE status = 1
     ORDER BY COALESCE(last_used_at, created_at) DESC"
  )
}

.db_upsert_profile <- function(project, name) {
  existing <- .query(project$con,
    "SELECT id FROM profiles WHERE lower(name) = lower(?)",
    list(name)
  )
  if (nrow(existing) > 0L) {
    .exec(project$con,
      "UPDATE profiles SET status = 1, last_used_at = now() WHERE id = ?",
      list(existing$id[[1L]])
    )
  } else {
    .exec(project$con,
      "INSERT INTO profiles (name, last_used_at) VALUES (?, now())",
      list(name)
    )
  }
  invisible(NULL)
}

.db_touch_profile <- function(project, name) {
  .exec(project$con,
    "UPDATE profiles SET last_used_at = now()
     WHERE lower(name) = lower(?) AND status = 1",
    list(name)
  )
  invisible(NULL)
}

.db_delete_profile <- function(project, name) {
  .soft_delete(project$con, "profiles",
    "lower(name) = lower(?)", list(name))
  invisible(NULL)
}

.db_save_profile_settings <- function(project, name, settings) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(NULL))
  json <- jsonlite::toJSON(settings, auto_unbox = TRUE)
  .exec(project$con,
    "UPDATE profiles SET settings_json = ?
     WHERE lower(name) = lower(?) AND status = 1",
    list(as.character(json), name)
  )
  invisible(NULL)
}

# -- Public API -----------------------------------------------------------------

#' List coder profiles stored in the project
#'
#' @param project A `qc_project` object.
#' @return A tibble with columns `name`, `created_at`, `last_used_at`.
#' @export
qc_list_profiles <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  df <- .db_list_profiles(project)
  df[, c("name", "created_at", "last_used_at"), drop = FALSE]
}
