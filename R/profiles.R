# -- Internal DB helpers --------------------------------------------------------

.db_list_profiles <- function(project) {
  .query(project$con,
    "SELECT name, settings_json, created_at, last_used_at
     FROM profiles WHERE status = 1
     ORDER BY COALESCE(last_used_at, created_at) DESC"
  )
}

.db_profile_settings_json <- function(settings) {
  if (is.null(settings)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return("{}")
  as.character(jsonlite::toJSON(settings, auto_unbox = TRUE))
}

.db_upsert_profile <- function(project, name, settings = NULL) {
  name <- trimws(as.character(name %||% ""))
  if (!nzchar(name)) return(invisible(NULL))
  settings_json <- .db_profile_settings_json(settings)
  existing <- .query(project$con,
    "SELECT id FROM profiles WHERE lower(name) = lower(?)",
    list(name)
  )
  if (nrow(existing) > 0L) {
    if (is.null(settings_json)) {
      .exec(project$con,
        "UPDATE profiles SET status = 1, last_used_at = now() WHERE id = ?",
        list(existing$id[[1L]])
      )
    } else {
      .exec(project$con,
        "UPDATE profiles
         SET status = 1, settings_json = ?, last_used_at = now()
         WHERE id = ?",
        list(settings_json, existing$id[[1L]])
      )
    }
  } else {
    if (is.null(settings_json)) {
      .exec(project$con,
        "INSERT INTO profiles (name, last_used_at) VALUES (?, now())",
        list(name)
      )
    } else {
      .exec(project$con,
        "INSERT INTO profiles (name, settings_json, last_used_at)
         VALUES (?, ?, now())",
        list(name, settings_json)
      )
    }
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
  .db_upsert_profile(project, name)
  json <- .db_profile_settings_json(settings)
  if (is.null(json)) return(invisible(NULL))
  .exec(project$con,
    "UPDATE profiles SET settings_json = ?, last_used_at = now()
     WHERE lower(name) = lower(?) AND status = 1",
    list(json, trimws(as.character(name %||% "")))
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
