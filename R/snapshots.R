#' Save a snapshot of the current codebook
#'
#' Serialises all active codes (including names, colours, memos, definitions,
#' criteria, parent relationships, and category links) to JSON and stores the
#' result in the `codebook_snapshots` table. This provides a reproducible,
#' point-in-time record of the codebook state.
#'
#' @param project A `qc_project` object.
#' @param label Character. Optional description for this snapshot
#'   (e.g. `"after initial coding round"`).
#'
#' @return A one-row tibble: `id`, `label`, `created_at`.
#' @export
qc_snapshot_codebook <- function(project, label = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    rlang::abort(
      'Codebook snapshots require jsonlite: install.packages("jsonlite")'
    )

  codes <- qc_list_codes(project)
  cats  <- qc_list_categories(project)

  snapshot <- lapply(seq_len(nrow(codes)), function(i) {
    cid       <- codes$id[[i]]
    code_cats <- cats$category_name[!is.na(cats$code_id) &
                                      cats$code_id == cid]
    list(
      id         = cid,
      name       = codes$name[[i]],
      color      = codes$color[[i]],
      memo       = codes$memo[[i]],
      definition = codes$definition[[i]],
      criteria   = codes$criteria[[i]],
      parent_id  = if (is.na(codes$parent_id[[i]])) NULL
                   else codes$parent_id[[i]],
      categories = as.list(unique(code_cats))
    )
  })

  json <- jsonlite::toJSON(snapshot, auto_unbox = TRUE, pretty = FALSE)

  .query(project$con,
    "INSERT INTO codebook_snapshots (label, snapshot_json)
     VALUES (?, ?)
     RETURNING id, label, created_at",
    list(label %||% "", json)
  )
}

#' List codebook snapshots
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble: `id`, `label`, `n_codes`, `created_at`.
#' @export
qc_list_snapshots <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    rlang::abort(
      'Snapshots require jsonlite: install.packages("jsonlite")'
    )

  raw <- .query(project$con,
    "SELECT id, label, snapshot_json, created_at
     FROM   codebook_snapshots
     ORDER  BY created_at DESC")

  n_codes <- vapply(raw$snapshot_json, function(j) {
    length(jsonlite::fromJSON(j, simplifyDataFrame = FALSE))
  }, integer(1L))

  tibble::tibble(
    id         = raw$id,
    label      = raw$label,
    n_codes    = n_codes,
    created_at = raw$created_at
  )
}

#' Retrieve a codebook snapshot as a tibble
#'
#' @param project A `qc_project` object.
#' @param snapshot_id Integer. The snapshot id from [qc_list_snapshots()].
#'
#' @return A tibble of codes as they existed at snapshot time.
#' @export
qc_get_snapshot <- function(project, snapshot_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    rlang::abort(
      'Snapshots require jsonlite: install.packages("jsonlite")'
    )

  row <- .query(project$con,
    "SELECT snapshot_json FROM codebook_snapshots WHERE id = ?",
    list(as.integer(snapshot_id)))
  if (nrow(row) == 0L)
    rlang::abort(paste0("No snapshot with id = ", snapshot_id))

  parsed <- jsonlite::fromJSON(row$snapshot_json[[1L]],
                                simplifyDataFrame = TRUE)
  tibble::as_tibble(parsed)
}
