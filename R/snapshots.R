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
    key <- codes$code_key[[i]]
    dep <- codes$deprecated[[i]]
    list(
      id                = cid,
      name              = codes$name[[i]],
      color             = codes$color[[i]],
      memo              = codes$memo[[i]],
      definition        = codes$definition[[i]],
      criteria          = codes$criteria[[i]],
      code_key          = if (is.na(key)) NULL else key,
      deprecated        = isTRUE(dep == 1L),
      deprecated_reason = {
        dr <- codes$deprecated_reason[[i]]
        if (is.null(dr) || is.na(dr)) "" else dr
      },
      parent_id         = if (is.na(codes$parent_id[[i]])) NULL
                          else codes$parent_id[[i]],
      categories        = as.list(unique(code_cats))
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

#' Compare two codebook snapshots
#'
#' Returns a row-per-change tibble describing what differed between two
#' point-in-time snapshots. Codes are matched by their stable numeric `id`.
#'
#' **Change types:**
#' - `"added"`: code present in snapshot 2 but not snapshot 1.
#' - `"removed"`: code present in snapshot 1 but not snapshot 2.
#' - `"changed"`: code present in both but one or more fields differ.
#'
#' @param project A `qc_project` object.
#' @param snapshot_id_1 Integer. The earlier snapshot (baseline).
#' @param snapshot_id_2 Integer. The later snapshot (comparison).
#'
#' @return A tibble: `code_id`, `code_name`, `change_type`, `field`,
#'   `old_value`, `new_value`. Returns an empty tibble with an info message
#'   when the snapshots are identical.
#' @export
qc_diff_snapshots <- function(project, snapshot_id_1, snapshot_id_2) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    rlang::abort('Snapshot diff requires jsonlite: install.packages("jsonlite")')

  .load_snap <- function(sid) {
    row <- .query(project$con,
      "SELECT snapshot_json FROM codebook_snapshots WHERE id = ?",
      list(as.integer(sid)))
    if (nrow(row) == 0L)
      rlang::abort(paste0("No snapshot with id = ", sid))
    parsed <- jsonlite::fromJSON(row$snapshot_json[[1L]],
                                  simplifyDataFrame = FALSE)
    stats::setNames(parsed,
      vapply(parsed, function(x) as.character(x$id), character(1L)))
  }

  s1   <- .load_snap(snapshot_id_1)
  s2   <- .load_snap(snapshot_id_2)
  ids1 <- names(s1)
  ids2 <- names(s2)

  diffs <- list()

  .row <- function(cid, cname, type,
                   field = NA_character_,
                   old   = NA_character_,
                   new   = NA_character_) {
    tibble::tibble(code_id     = as.integer(cid),
                   code_name   = as.character(cname),
                   change_type = type,
                   field       = field,
                   old_value   = old,
                   new_value   = new)
  }

  for (id in ids1[!ids1 %in% ids2])
    diffs[[length(diffs) + 1L]] <- .row(id, s1[[id]]$name, "removed")

  for (id in ids2[!ids2 %in% ids1])
    diffs[[length(diffs) + 1L]] <- .row(id, s2[[id]]$name, "added")

  scalar_fields <- c("name", "color", "memo", "definition", "criteria",
                     "code_key", "deprecated", "deprecated_reason")
  for (id in ids1[ids1 %in% ids2]) {
    c1   <- s1[[id]]
    c2   <- s2[[id]]
    cname <- c2$name %||% c1$name

    for (f in scalar_fields) {
      in1 <- !is.null(c1[[f]])
      in2 <- !is.null(c2[[f]])
      if (!in1 && !in2) next
      v1 <- if (in1) as.character(c1[[f]]) else NA_character_
      v2 <- if (in2) as.character(c2[[f]]) else NA_character_
      if (!identical(v1, v2))
        diffs[[length(diffs) + 1L]] <- .row(id, cname, "changed", f, v1, v2)
    }

    p1 <- if (is.null(c1$parent_id)) NA_character_
          else as.character(c1$parent_id)
    p2 <- if (is.null(c2$parent_id)) NA_character_
          else as.character(c2$parent_id)
    if (!identical(p1, p2))
      diffs[[length(diffs) + 1L]] <- .row(id, cname, "changed",
                                           "parent_id", p1, p2)

    cats1 <- paste(sort(unlist(c1$categories)), collapse = ", ")
    cats2 <- paste(sort(unlist(c2$categories)), collapse = ", ")
    if (!identical(cats1, cats2))
      diffs[[length(diffs) + 1L]] <- .row(id, cname, "changed",
                                           "categories", cats1, cats2)
  }

  if (length(diffs) == 0L) {
    cli::cli_alert_info(
      "Snapshots {snapshot_id_1} and {snapshot_id_2} are identical.")
    return(tibble::tibble(
      code_id     = integer(0),   code_name   = character(0),
      change_type = character(0), field       = character(0),
      old_value   = character(0), new_value   = character(0)
    ))
  }

  do.call(rbind, diffs)
}
