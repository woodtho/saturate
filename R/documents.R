#' Import a text document into the project
#'
#' Supports `.txt`, `.csv`, `.docx`, `.pdf`, and spreadsheet files. Requires
#' `readtext` (any format), `officer` (`.docx`), `pdftools` (`.pdf`), or
#' `readxl` (`.xlsx`/`.xls`) to be installed for the corresponding format.
#'
#' Unicode text is normalised to NFC on import when `stringi` is available.
#' An MD5 hash of the content is stored; if an identical document already
#' exists, a warning is emitted.
#'
#' @param project A `qc_project` object.
#' @param path Character. Path to a file. When `NULL`, `content` must be given.
#' @param content Character scalar. Raw document text (used when `path = NULL`).
#' @param name Character. Display name. Defaults to filename without extension.
#' @param memo Character. Initial memo text.
#' @param language Character. BCP-47 language tag, e.g. `"en"`, `"fr-CA"`.
#' @param parent_id Integer or `NULL`. Parent document id for segments.
#' @param source_type Character. Data-collection method label, e.g.
#'   `"interview"`, `"focus_group"`, `"survey"`, `"observation"`. Used by
#'   [qc_triangulate()] and [qc_saturation_curve()].
#'
#' @return A one-row tibble: `id`, `name`, `created_at`.
#' @export
qc_import_document <- function(project,
                               path        = NULL,
                               content     = NULL,
                               name        = NULL,
                               memo        = "",
                               language    = "",
                               parent_id   = NULL,
                               source_type = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)

  if (!is.null(path) && !is.null(content))
    rlang::abort("Supply `path` or `content`, not both.")
  if (is.null(path) && is.null(content))
    rlang::abort("Supply either `path` or `content`.")

  filename      <- ""
  source_system <- "manual"

  if (!is.null(path)) {
    path    <- fs::path_abs(path)
    if (!fs::file_exists(path)) rlang::abort(paste0("File not found: ", path))
    filename      <- fs::path_file(path)
    source_system <- tolower(fs::path_ext(path))
    if (is.null(name))
      name <- fs::path_ext_remove(fs::path_file(path))
    content <- .read_file_content(path, source_system)
  }

  if (!is_string(content))  rlang::abort("`content` must be a single string.")
  if (is.null(name))        rlang::abort("`name` required when `path = NULL`.")
  if (!is_string(name))     rlang::abort("`name` must be a single string.")

  content    <- .normalize_content(content)
  word_count <- .count_words(content)

  source_type <- source_type %||% ""

  if (is.null(parent_id)) {
    row <- .query(project$con,
      "INSERT INTO sources
         (name, content, memo, filename, source_system, language,
          word_count, source_type)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)
       RETURNING id, name, created_at",
      list(name, content, memo %||% "", filename, source_system,
           language %||% "", word_count, source_type)
    )
  } else {
    row <- .query(project$con,
      "INSERT INTO sources
         (name, content, memo, filename, source_system, language,
          word_count, parent_id, source_type)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
       RETURNING id, name, created_at",
      list(name, content, memo %||% "", filename, source_system,
           language %||% "", word_count, as.integer(parent_id), source_type)
    )
  }

  .exec(project$con,
    "UPDATE sources SET content_hash = md5(content) WHERE id = ?",
    list(row$id)
  )

  dups <- .query(project$con,
    "SELECT name FROM sources
     WHERE content_hash = md5(?) AND status = 1 AND id != ?",
    list(content, row$id)
  )
  if (nrow(dups) > 0L)
    cli::cli_warn(c(
      "!" = "Possible duplicate of: {paste(dups$name, collapse = ', ')}"
    ))

  row
}

# Read raw text from a file, dispatching on extension.
.read_file_content <- function(path, ext) {
  switch(ext,
    pdf = {
      if (!requireNamespace("pdftools", quietly = TRUE))
        rlang::abort("Install `pdftools` to import PDF files.")
      paste(pdftools::pdf_text(path), collapse = "\n\n")
    },
    docx = {
      if (requireNamespace("officer", quietly = TRUE)) {
        doc  <- officer::read_docx(path)
        summ <- officer::docx_summary(doc)
        paste(summ$text[!is.na(summ$text)], collapse = "\n")
      } else if (requireNamespace("readtext", quietly = TRUE)) {
        readtext::readtext(path)$text[[1L]]
      } else {
        rlang::abort("Install `officer` or `readtext` to import .docx files.")
      }
    },
    {   # xlsx / xls / csv / tsv / txt and everything else
      if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE))
          rlang::abort("Install `readxl` to import spreadsheet files.")
        df   <- readxl::read_excel(path)
        cols <- vapply(df, function(x) is.character(x) || is.numeric(x),
                       logical(1L))
        rows <- apply(df[, cols, drop = FALSE], 1L, function(r) {
          r <- r[!is.na(r)]
          paste(r, collapse = " ")
        })
        paste(rows, collapse = "\n")
      } else if (requireNamespace("readtext", quietly = TRUE)) {
        readtext::readtext(path)$text[[1L]]
      } else {
        paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
              collapse = "\n")
      }
    }
  )
}

# Write text to a .docx file using officer, falling back to plain UTF-8 text.
.export_as_docx <- function(text, file) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    writeLines(text, file, useBytes = FALSE)
    return(invisible(NULL))
  }
  doc   <- officer::read_docx()
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  for (ln in lines) doc <- officer::body_add_par(doc, ln)
  print(doc, target = file)
}

# NFC-normalise and clean up common encoding artefacts.
.normalize_content <- function(text) {
  if (requireNamespace("stringi", quietly = TRUE))
    text <- stringi::stri_trans_nfc(text)
  text
}

.count_words <- function(text) {
  text <- trimws(text)
  if (nchar(text) == 0L) return(0L)
  length(strsplit(text, "\\s+")[[1L]])
}

# Returns distinct non-empty source_type values from the project, merged with
# a fixed set of common defaults. Safe to call even on a fresh project.
.source_type_options <- function(project) {
  defaults <- c("interview", "focus_group", "survey", "observation", "document")
  tryCatch({
    df <- .query(project$con,
      "SELECT DISTINCT source_type FROM sources
       WHERE status = 1 AND source_type IS NOT NULL AND source_type != ''
       ORDER BY source_type")
    unique(c(defaults, df$source_type))
  }, error = function(e) defaults)
}

#' List all documents in the project
#'
#' @param project A `qc_project` object.
#' @param include_content Logical. Include the full `content` column.
#' @param segments Logical. When `FALSE`, only root documents
#'   (`parent_id IS NULL`) are returned, hiding segments created by
#'   [qc_segment_document()].
#'
#' @return A tibble: `id`, `name`, `memo`, `filename`, `source_system`,
#'   `language`, `source_type`, `doc_version`, `word_count`, `char_count`,
#'   `parent_id`, `n_codings`, `n_coders`,
#'   `created_at` (and `content` when `include_content = TRUE`).
#' @export
qc_list_documents <- function(project, include_content = FALSE,
                               segments = TRUE) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  content_col  <- if (include_content) "s.content," else ""
  content_grp  <- if (include_content) "s.content," else ""
  w_parent     <- if (!segments) "AND s.parent_id IS NULL" else ""
  .query(project$con, paste0("
    SELECT s.id, s.name, ", content_col, " s.memo,
           s.filename, s.source_system, s.language, s.source_type,
           s.doc_version, s.word_count, LENGTH(s.content) AS char_count,
           s.parent_id,
           COUNT(c.id)            AS n_codings,
           COUNT(DISTINCT c.coder) AS n_coders,
           s.created_at
    FROM   sources s
    LEFT   JOIN codings c ON c.source_id = s.id AND c.status = 1
    WHERE  s.status = 1 ", w_parent, "
    GROUP  BY s.id, s.name, ", content_grp,
    "s.memo, s.filename, s.source_system, s.language, s.source_type,
     s.doc_version, s.word_count, LENGTH(s.content), s.parent_id, s.created_at
    ORDER  BY s.created_at
  "))
}

#' Retrieve a single document's full text
#'
#' @param project A `qc_project` object.
#' @param id Integer. The document id.
#'
#' @return A one-row tibble: `id`, `name`, `content`, `memo`.
#' @export
qc_get_document <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  out <- .query(project$con,
    "SELECT id, name, content, memo, filename, source_system, language,
            source_type, doc_version, word_count, content_hash, parent_id,
            created_at
     FROM   sources WHERE id = ? AND status = 1",
    list(as.integer(id))
  )
  if (nrow(out) == 0L) rlang::abort(paste0("No document with id = ", id))
  out
}

#' Update the memo on a document
#'
#' @param project A `qc_project` object.
#' @param id Integer. Document id.
#' @param memo Character. New memo text.
#'
#' @return The updated one-row tibble (same shape as `qc_get_document()`).
#' @export
qc_update_document_memo <- function(project, id, memo) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE sources SET memo = ? WHERE id = ? AND status = 1",
    list(memo, as.integer(id))
  )
  qc_get_document(project, id)
}

#' Remove a document (soft delete)
#'
#' Also soft-deletes all codings attached to this document.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Document id.
#'
#' @return Invisibly, the number of codings also soft-deleted.
#' @export
qc_delete_document <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id <- as.integer(id)
  n  <- .exec(project$con,
    "UPDATE codings SET status = 0 WHERE source_id = ? AND status = 1",
    list(id)
  )
  .soft_delete(project$con, "sources", "id", id)
  invisible(n)
}
