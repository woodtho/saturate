#' Validate the codebook for structural and quality issues
#'
#' Runs a series of checks and returns a tibble of issues grouped by severity.
#' Uses a flat SQL fetch (not the recursive CTE) so it is safe to call even
#' when the hierarchy may be circular.
#'
#' **Checks performed:**
#' - `orphan_parent` (error): `parent_id` references a non-existent code.
#' - `circular_hierarchy` (error): parent chain loops back to itself.
#' - `missing_code_key` (warning): no stable key assigned; call
#'   [qc_set_code_key()].
#' - `missing_definition` (warning): code has no definition text.
#' - `missing_criteria` (info): code has no inclusion/exclusion criteria.
#' - `unused_code` (info): code has zero active codings.
#' - `deprecated_with_codings` (warning): deprecated code still has active
#'   codings.
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble: `code_id`, `code_name`, `issue_type`, `severity`
#'   (`"error"`, `"warning"`, `"info"`), `message`. Ordered error -> warning ->
#'   info, then alphabetically by code name. Returns an empty tibble (with a
#'   success message) when no issues are found.
#' @export
qc_validate_codebook <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  codes <- .query(project$con, "
    SELECT c.id, c.name, c.parent_id,
           c.definition, c.criteria, c.code_key,
           c.deprecated, c.deprecated_reason,
           COUNT(DISTINCT cod.id) AS n_codings
    FROM   codes c
    LEFT   JOIN codings cod ON cod.code_id = c.id AND cod.status = 1
    WHERE  c.status = 1
    GROUP  BY c.id, c.name, c.parent_id, c.definition, c.criteria,
              c.code_key, c.deprecated, c.deprecated_reason
  ")

  # Ensure deprecated is never NA (old rows before migration get 0)
  codes$deprecated[is.na(codes$deprecated)] <- 0L

  issues <- list()

  .issue <- function(cid, cname, type, sev, msg) {
    tibble::tibble(code_id    = as.integer(cid),
                   code_name  = as.character(cname),
                   issue_type = type,
                   severity   = sev,
                   message    = msg)
  }

  all_ids    <- codes$id
  parent_map <- stats::setNames(codes$parent_id, as.character(codes$id))

  # 1. Orphan parent_id
  for (i in which(!is.na(codes$parent_id) & !(codes$parent_id %in% all_ids))) {
    issues[[length(issues) + 1L]] <- .issue(
      codes$id[[i]], codes$name[[i]], "orphan_parent", "error",
      paste0("parent_id = ", codes$parent_id[[i]],
             " does not exist or has been deleted")
    )
  }

  # 2. Circular hierarchy (R-level walk -- avoids running a broken recursive CTE)
  for (i in seq_len(nrow(codes))) {
    visited <- codes$id[[i]]
    curr    <- codes$parent_id[[i]]
    while (!is.na(curr)) {
      if (curr %in% visited) {
        issues[[length(issues) + 1L]] <- .issue(
          codes$id[[i]], codes$name[[i]], "circular_hierarchy", "error",
          paste0("parent chain loops back through id = ", curr)
        )
        break
      }
      visited   <- c(visited, curr)
      next_par  <- parent_map[[as.character(curr)]]
      curr      <- if (is.null(next_par)) NA_integer_ else next_par
    }
  }

  # 3. Missing code_key
  for (i in which(is.na(codes$code_key) | codes$code_key == "")) {
    issues[[length(issues) + 1L]] <- .issue(
      codes$id[[i]], codes$name[[i]], "missing_code_key", "warning",
      "Code has no stable key; call qc_set_code_key() to assign one"
    )
  }

  # 4. Missing definition
  for (i in which(is.na(codes$definition) | codes$definition == "")) {
    issues[[length(issues) + 1L]] <- .issue(
      codes$id[[i]], codes$name[[i]], "missing_definition", "warning",
      "Code has no definition"
    )
  }

  # 5. Missing criteria
  for (i in which(is.na(codes$criteria) | codes$criteria == "")) {
    issues[[length(issues) + 1L]] <- .issue(
      codes$id[[i]], codes$name[[i]], "missing_criteria", "info",
      "Code has no inclusion/exclusion criteria"
    )
  }

  # 6. Unused codes
  for (i in which(codes$n_codings == 0L)) {
    issues[[length(issues) + 1L]] <- .issue(
      codes$id[[i]], codes$name[[i]], "unused_code", "info",
      "Code has no codings"
    )
  }

  # 7. Deprecated codes that still have active codings
  for (i in which(codes$deprecated == 1L & codes$n_codings > 0L)) {
    issues[[length(issues) + 1L]] <- .issue(
      codes$id[[i]], codes$name[[i]], "deprecated_with_codings", "warning",
      paste0("Deprecated code still has ", codes$n_codings[[i]],
             " active coding(s)")
    )
  }

  if (length(issues) == 0L) {
    cli::cli_alert_success("Codebook is valid - no issues found.")
    return(tibble::tibble(
      code_id    = integer(0),   code_name  = character(0),
      issue_type = character(0), severity   = character(0),
      message    = character(0)
    ))
  }

  result    <- do.call(rbind, issues)
  sev_rank  <- c(error = 1L, warning = 2L, info = 3L)
  result    <- result[order(sev_rank[result$severity], result$code_name), ]
  result
}
