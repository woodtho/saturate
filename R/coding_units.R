#' Code a document by predefined text units
#'
#' Computes unit boundaries within a document and applies a code to each
#' selected unit as a single coding that spans that unit's character range.
#' Useful for systematic coding of paragraphs, sentences, or structured
#' response items without manual selection.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param code_id Integer. Code to apply.
#' @param unit One of `"paragraph"` (split on two or more blank lines) or
#'   `"sentence"` (linguistic boundaries via `stringi`).
#' @param unit_indices Integer vector or `NULL`. Which units to code (1-based).
#'   `NULL` codes every unit.
#' @param min_chars Integer. Skip units shorter than this (default `10L`).
#' @param coder Character. Coder identifier.
#' @param coding_status One of `"draft"` or `"validated"`.
#' @param memo Character. Memo applied to every coding created.
#'
#' @return A tibble with one row per coding created: `id`, `unit_n`,
#'   `selfirst`, `selast`, `seltext`.
#' @export
qc_code_by_unit <- function(project, source_id, code_id,
                             unit          = c("paragraph", "sentence"),
                             unit_indices  = NULL,
                             min_chars     = 10L,
                             coder         = "default",
                             coding_status = "validated",
                             memo          = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  unit      <- match.arg(unit)
  source_id <- as.integer(source_id)
  code_id   <- as.integer(code_id)

  doc   <- qc_get_document(project, source_id)
  units <- .locate_units(doc$content, unit, as.integer(min_chars))

  if (!is.null(unit_indices)) {
    idx   <- as.integer(unit_indices)
    bad   <- idx[idx < 1L | idx > nrow(units)]
    if (length(bad) > 0L)
      rlang::abort(paste0("unit_indices out of range: ",
                          paste(bad, collapse = ", ")))
    units <- units[idx, ]
  }

  if (nrow(units) == 0L) {
    cli::cli_warn("No units found matching the criteria.")
    return(tibble::tibble(id = integer(0), unit_n = integer(0),
                          selfirst = integer(0), selast = integer(0),
                          seltext = character(0)))
  }

  rows <- vector("list", nrow(units))
  for (i in seq_len(nrow(units))) {
    row <- qc_add_coding(project,
      source_id     = source_id,
      code_id       = code_id,
      selfirst      = units$selfirst[[i]],
      selast        = units$selast[[i]],
      memo          = memo,
      coder         = coder,
      coding_source = "manual",
      coding_status = coding_status
    )
    rows[[i]] <- tibble::tibble(
      id       = row$id,
      unit_n   = units$unit_n[[i]],
      selfirst = row$selfirst,
      selast   = row$selast,
      seltext  = row$seltext
    )
  }

  cli::cli_alert_success(
    "Created {nrow(units)} coding{?s} for {nrow(units)} unit{?s}.")
  do.call(rbind, rows)
}

# Returns a tibble with unit_n, selfirst, selast for each text unit.
.locate_units <- function(content, unit, min_chars) {
  n <- nchar(content)
  if (n == 0L) return(.empty_units())

  if (unit == "paragraph") {
    m    <- gregexpr("\n{2,}", content, perl = TRUE)[[1L]]
    lens <- attr(m, "match.length")
    if (m[[1L]] == -1L) {
      # Single paragraph: whole document
      bounds <- tibble::tibble(selfirst = 1L, selast = n)
    } else {
      starts <- c(1L, as.integer(m) + lens)
      ends   <- c(as.integer(m) - 1L, n)
      bounds <- tibble::tibble(selfirst = starts, selast = ends)
    }
  } else {
    if (!requireNamespace("stringi", quietly = TRUE))
      rlang::abort("Install `stringi` for sentence-level unit coding.")
    locs <- stringi::stri_locate_all_boundaries(
      content, type = "sentence")[[1L]]
    if (nrow(locs) == 0L) return(.empty_units())
    bounds <- tibble::tibble(selfirst = as.integer(locs[, "start"]),
                             selast   = as.integer(locs[, "end"]))
  }

  bounds <- bounds[bounds$selast >= bounds$selfirst, ]
  text   <- substr(content, bounds$selfirst, bounds$selast)
  keep   <- nchar(trimws(text)) >= min_chars
  bounds <- bounds[keep, ]

  if (nrow(bounds) == 0L) return(.empty_units())
  bounds$unit_n <- seq_len(nrow(bounds))
  bounds[, c("unit_n", "selfirst", "selast")]
}

.empty_units <- function() {
  tibble::tibble(unit_n = integer(0),
                 selfirst = integer(0), selast = integer(0))
}
