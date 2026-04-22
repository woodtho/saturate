#' Export the codebook to a file
#'
#' Writes all active codes to a CSV, JSON, or Markdown file. CSV and JSON
#' include all fields: `code_key`, `name`, `color`, `memo`, `definition`,
#' `criteria`, `parent_name`, `depth`, `n_codings`, `deprecated`,
#' `deprecated_reason`, and `categories`. JSON requires \pkg{jsonlite}.
#' Markdown produces a human-readable reference document suitable for
#' supplementary materials.
#'
#' @param project A `qc_project` object.
#' @param path Character. Destination file path.
#' @param format One of `"csv"` (default), `"json"`, or `"md"`.
#'
#' @return Invisibly, `path`.
#' @export
qc_export_codebook <- function(project, path, format = c("csv", "json", "md")) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  format <- match.arg(format)

  codes <- qc_list_codes(project)

  # Normalise NA to empty / 0 for clean output
  codes$categories[is.na(codes$categories)]               <- ""
  codes$definition[is.na(codes$definition)]               <- ""
  codes$criteria[is.na(codes$criteria)]                   <- ""
  codes$code_key[is.na(codes$code_key)]                   <- ""
  codes$parent_name[is.na(codes$parent_name)]             <- ""
  codes$deprecated[is.na(codes$deprecated)]               <- 0L
  codes$deprecated_reason[is.na(codes$deprecated_reason)] <- ""

  if (format == "csv") {
    out <- codes[, c("code_key", "name", "color", "memo",
                     "definition", "criteria",
                     "parent_name", "depth", "n_codings",
                     "deprecated", "deprecated_reason", "categories")]
    utils::write.csv(out, file = path, row.names = FALSE)

  } else if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      rlang::abort('JSON export requires jsonlite: install.packages("jsonlite")')
    rows <- lapply(seq_len(nrow(codes)), function(i) {
      cats <- codes$categories[[i]]
      cats_list <- if (nchar(cats) > 0L)
        as.list(trimws(strsplit(cats, ",")[[1L]]))
      else
        list()
      list(
        code_key          = if (nchar(codes$code_key[[i]]) > 0L)
                              codes$code_key[[i]] else NULL,
        name              = codes$name[[i]],
        color             = codes$color[[i]],
        memo              = codes$memo[[i]],
        definition        = codes$definition[[i]],
        criteria          = codes$criteria[[i]],
        parent_name       = if (nchar(codes$parent_name[[i]]) > 0L)
                              codes$parent_name[[i]] else NULL,
        depth             = codes$depth[[i]],
        n_codings         = codes$n_codings[[i]],
        deprecated        = codes$deprecated[[i]] == 1L,
        deprecated_reason = codes$deprecated_reason[[i]],
        categories        = cats_list
      )
    })
    writeLines(
      jsonlite::toJSON(rows, auto_unbox = TRUE, pretty = TRUE),
      path
    )

  } else {
    info  <- qc_project_info(project)
    lines <- .build_codebook_md(info, codes)
    writeLines(lines, path, useBytes = FALSE)
  }
  invisible(path)
}

# Build lines for Markdown codebook export.
.build_codebook_md <- function(info, codes) {
  n_active <- sum(codes$deprecated == 0L)
  n_dep    <- sum(codes$deprecated == 1L)

  header <- c(
    paste0("# Codebook: ", info$name),
    "",
    paste0("**Owner:** ",     info$owner %||% ""),
    paste0("**Generated:** ", format(Sys.Date())),
    paste0("**Active codes:** ", n_active),
    if (n_dep > 0L) paste0("**Deprecated codes:** ", n_dep) else NULL,
    "",
    "---",
    ""
  )

  # Assign each code to one display category (first listed, or sentinel)
  UNCAT <- "__uncat__"
  cat_assign <- vapply(seq_len(nrow(codes)), function(i) {
    cs <- codes$categories[[i]]
    if (!is.na(cs) && nchar(trimws(cs)) > 0L)
      trimws(strsplit(cs, ",")[[1L]][[1L]])
    else
      UNCAT
  }, character(1L))

  all_cats   <- unique(cat_assign)
  named_cats <- sort(all_cats[all_cats != UNCAT])
  cat_order  <- c(named_cats, if (UNCAT %in% all_cats) UNCAT)

  body <- character(0)
  for (cat in cat_order) {
    heading <- if (cat == UNCAT) "Uncategorized" else cat
    body    <- c(body, paste0("## ", heading), "")

    for (i in which(cat_assign == cat)) {
      key_tag  <- codes$code_key[[i]]
      key_str  <- if (nchar(key_tag) > 0L) paste0(" `[", key_tag, "]`") else ""
      dep_flag <- if (codes$deprecated[[i]] == 1L) " **[DEPRECATED]**" else ""

      body <- c(body, paste0("### ", codes$name[[i]], key_str, dep_flag), "")

      reason <- codes$deprecated_reason[[i]]
      if (codes$deprecated[[i]] == 1L && nchar(reason) > 0L)
        body <- c(body, paste0("> *Deprecated:* ", reason), "")

      def  <- codes$definition[[i]]
      crit <- codes$criteria[[i]]
      body <- c(body,
        paste0("**Definition:** ",
               if (nchar(def) > 0L) def else "*Not defined.*"),
        "")
      if (nchar(crit) > 0L)
        body <- c(body, paste0("**Criteria:** ", crit), "")

      meta <- paste0("*Codings: ", codes$n_codings[[i]], "*")
      par  <- codes$parent_name[[i]]
      if (nchar(par) > 0L)
        meta <- paste0(meta, " | *Parent: ", par, "*")
      body <- c(body, meta, "", "---", "")
    }
  }

  c(header, body, "", "*Exported by the saturate package.*")
}

#' Import a codebook from a file
#'
#' Reads codes from a CSV or JSON file and adds them to the project. Codes
#' whose names already exist are skipped by default. Category names in the
#' file are created if absent, then linked to the imported code.
#'
#' **CSV columns:** `name` (required), `color`, `memo`, `definition`,
#' `criteria`, `code_key`, `categories` (comma-separated names in a single
#' cell).
#'
#' **JSON format:** array of objects — `name`, `color`, `memo`, `definition`,
#' `criteria`, `code_key`, `categories` (array of strings). JSON import
#' requires \pkg{jsonlite}.
#'
#' @param project A `qc_project` object.
#' @param path Character. Path to the import file.
#' @param format One of `"csv"` (default) or `"json"`.
#' @param skip_existing Logical. When `TRUE` (default), codes whose names
#'   already exist are silently skipped.
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
    if (!"definition" %in% names(df)) df$definition  <- ""
    if (!"criteria"   %in% names(df)) df$criteria    <- ""
    if (!"code_key"   %in% names(df)) df$code_key    <- NA_character_
    if (!"categories" %in% names(df)) df$categories  <- NA_character_
  } else {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      rlang::abort('JSON import requires jsonlite: install.packages("jsonlite")')
    rows <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
    df <- tibble::tibble(
      name       = vapply(rows, `[[`, character(1L), "name"),
      color      = vapply(rows, function(r) r$color      %||% "#4E79A7", character(1L)),
      memo       = vapply(rows, function(r) r$memo       %||% "",        character(1L)),
      definition = vapply(rows, function(r) r$definition %||% "",        character(1L)),
      criteria   = vapply(rows, function(r) r$criteria   %||% "",        character(1L)),
      code_key   = vapply(rows, function(r) r$code_key   %||% NA_character_, character(1L)),
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
    memo_val  <- df$memo[[i]];       if (is.na(memo_val))  memo_val  <- ""
    def_val   <- df$definition[[i]]; if (is.na(def_val))   def_val   <- ""
    crit_val  <- df$criteria[[i]];   if (is.na(crit_val))  crit_val  <- ""
    key_val   <- df$code_key[[i]]
    if (is.na(key_val) || nchar(trimws(key_val)) == 0L) key_val <- NULL

    code <- qc_add_code(project, name = nm, color = color_val, memo = memo_val,
                        definition = def_val, criteria = crit_val,
                        code_key = key_val)
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
