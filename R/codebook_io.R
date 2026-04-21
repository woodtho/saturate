#' Export the codebook to a file
#'
#' Writes all active codes (name, colour, memo, categories) to a CSV or JSON
#' file. JSON export requires the \pkg{jsonlite} package.
#'
#' @param project A `qc_project` object.
#' @param path Character. Destination file path.
#' @param format One of `"csv"` (default) or `"json"`.
#'
#' @return Invisibly, `path`.
#' @export
qc_export_codebook <- function(project, path, format = c("csv", "json")) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  format <- match.arg(format)

  codes <- qc_list_codes(project)
  out   <- codes[, c("name", "color", "memo", "categories")]
  out$categories[is.na(out$categories)] <- ""

  if (format == "csv") {
    utils::write.csv(out, file = path, row.names = FALSE)
  } else {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      rlang::abort(
        'JSON export requires jsonlite: install.packages("jsonlite")'
      )
    rows <- lapply(seq_len(nrow(out)), function(i) {
      cats_str <- out$categories[[i]]
      cats <- if (nchar(cats_str) > 0L)
        as.list(trimws(strsplit(cats_str, ",")[[1L]]))
      else
        list()
      list(name       = out$name[[i]],
           color      = out$color[[i]],
           memo       = out$memo[[i]],
           categories = cats)
    })
    writeLines(
      jsonlite::toJSON(rows, auto_unbox = TRUE, pretty = TRUE),
      path
    )
  }
  invisible(path)
}

#' Import a codebook from a file
#'
#' Reads codes from a CSV or JSON file and adds them to the project. Codes
#' whose names already exist are skipped by default. Category names in the
#' file are created if absent, then linked to the imported code.
#'
#' **CSV columns:** `name` (required), `color`, `memo`, `categories`
#' (comma-separated names in a single cell).
#'
#' **JSON format:** array of objects — `name`, `color`, `memo`, `categories`
#' (array of strings). JSON import requires \pkg{jsonlite}.
#'
#' @param project A `qc_project` object.
#' @param path Character. Path to the import file.
#' @param format One of `"csv"` (default) or `"json"`.
#' @param skip_existing Logical. When `TRUE` (default), codes whose names
#'   already exist in the project are silently skipped.
#'
#' @return Invisibly, a one-row tibble: `imported`, `skipped`.
#' @export
qc_import_codebook <- function(project, path,
                                format        = c("csv", "json"),
                                skip_existing = TRUE) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  format <- match.arg(format)
  if (!fs::file_exists(path))
    rlang::abort(paste0("File not found: ", path))

  if (format == "csv") {
    df <- utils::read.csv(path, stringsAsFactors = FALSE)
    if (!"name" %in% names(df))
      rlang::abort('CSV must contain a "name" column.')
    if (!"color"      %in% names(df)) df$color      <- "#4E79A7"
    if (!"memo"       %in% names(df)) df$memo        <- ""
    if (!"categories" %in% names(df)) df$categories  <- NA_character_
  } else {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      rlang::abort(
        'JSON import requires jsonlite: install.packages("jsonlite")'
      )
    rows <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
    df <- tibble::tibble(
      name = vapply(rows, `[[`, character(1L), "name"),
      color = vapply(rows, function(r) r$color %||% "#4E79A7", character(1L)),
      memo  = vapply(rows, function(r) r$memo  %||% "",        character(1L)),
      categories = vapply(rows, function(r) {
        cats <- r$categories
        if (length(cats) == 0L) NA_character_
        else paste(unlist(cats), collapse = ", ")
      }, character(1L))
    )
  }

  existing <- qc_list_codes(project)$name
  imported <- 0L
  skipped  <- 0L

  for (i in seq_len(nrow(df))) {
    nm <- df$name[[i]]
    if (nm %in% existing) {
      if (skip_existing) { skipped <- skipped + 1L; next }
    }

    color_val <- df$color[[i]]
    if (is.na(color_val) || !nchar(trimws(color_val))) color_val <- "#4E79A7"
    memo_val  <- df$memo[[i]]
    if (is.na(memo_val)) memo_val <- ""

    code <- qc_add_code(project, name = nm,
                        color = color_val, memo = memo_val)
    imported <- imported + 1L

    cats_str <- df$categories[[i]]
    if (!is.na(cats_str) && nchar(trimws(cats_str)) > 0L) {
      cat_names <- trimws(strsplit(cats_str, ",")[[1L]])
      cat_names <- cat_names[nchar(cat_names) > 0L]
      for (cn in cat_names) {
        existing_cat <- .query(project$con,
          "SELECT id FROM code_categories WHERE name = ? AND status = 1",
          list(cn))
        cat_id <- if (nrow(existing_cat) == 0L) {
          qc_add_category(project, cn)$id
        } else {
          existing_cat$id[[1L]]
        }
        qc_link_code_category(project, code$id, cat_id)
      }
    }
  }

  cli::cli_alert_success(
    "Imported {imported} code{?s}, skipped {skipped} duplicate{?s}."
  )
  invisible(tibble::tibble(imported = imported, skipped = skipped))
}
