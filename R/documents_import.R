#' Import multiple documents from a directory or tabular file
#'
#' Handles three source types:
#' - **Directory**: imports every file whose extension matches `file_pattern`.
#' - **CSV / TSV**: each row becomes one document; `text_col` names the text
#'   column, `name_col` the display-name column (optional).
#' - **Excel (`.xlsx` / `.xls`)**: same row-per-document model.
#'
#' Columns listed in `metadata_cols` are stored as `source_attributes` on the
#' imported document.
#'
#' @param project A `qc_project` object.
#' @param path Character. Path to a directory, CSV/TSV, or Excel file.
#' @param text_col Character. Column name containing document text (tabular
#'   sources only).
#' @param name_col Character or `NULL`. Column to use as document name
#'   (tabular). When `NULL`, names are generated from the source file and
#'   row number.
#' @param metadata_cols Character vector or `NULL`. Column names whose values
#'   are stored as source attributes.
#' @param format One of `"dir"`, `"csv"`, `"tsv"`, `"xlsx"`, `"xls"`, or
#'   `NULL` (auto-detect from extension / path type).
#' @param language Character. BCP-47 language tag applied to all imported docs.
#' @param file_pattern Glob passed to [fs::dir_ls()] for directory import
#'   (default `"*"`).
#' @param sheet Integer. Sheet index for Excel files (default `1`).
#' @param skip Integer. Rows to skip before the header in tabular files.
#'
#' @return A tibble with one row per imported document: `id`, `name`,
#'   `created_at`, `row` (source row or filename).
#' @export
qc_import_batch <- function(project, path,
                             text_col      = NULL,
                             name_col      = NULL,
                             metadata_cols = NULL,
                             format        = NULL,
                             language      = "",
                             file_pattern  = "*",
                             sheet         = 1L,
                             skip          = 0L) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)

  path <- fs::path_abs(path)

  if (is.null(format)) {
    format <- if (fs::is_dir(path)) "dir"
               else tolower(fs::path_ext(path))
  }

  if (format == "dir") {
    files <- fs::dir_ls(path, glob = file_pattern, recurse = FALSE)
    if (length(files) == 0L)
      rlang::abort(paste0("No files matching '", file_pattern,
                          "' in ", path))
    rows <- vector("list", length(files))
    for (i in seq_along(files)) {
      tryCatch({
        row      <- qc_import_document(project, path = files[[i]],
                                       language = language)
        rows[[i]] <- tibble::tibble(id = row$id, name = row$name,
                                    created_at = row$created_at,
                                    row = fs::path_file(files[[i]]))
      }, error = function(e) {
        cli::cli_warn("Skipping {.file {files[[i]]}}: {conditionMessage(e)}")
      })
    }
    rows <- rows[!vapply(rows, is.null, logical(1L))]
    cli::cli_alert_success(
      "Imported {length(rows)} document{?s} from {.file {path}}")
    return(do.call(rbind, rows))
  }

  # Tabular import --------------------------------------------------------
  if (is.null(text_col))
    rlang::abort("`text_col` required for tabular import.")

  df <- if (format %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      rlang::abort("Install `readxl` to import spreadsheet files.")
    readxl::read_excel(path, sheet = sheet, skip = skip)
  } else {
    sep <- if (format == "tsv") "\t" else ","
    utils::read.csv(path, sep = sep, skip = skip,
                    stringsAsFactors = FALSE, encoding = "UTF-8",
                    check.names = FALSE)
  }

  if (!text_col %in% names(df))
    rlang::abort(paste0("`text_col` '", text_col,
                        "' not found in file. Columns: ",
                        paste(names(df), collapse = ", ")))

  base_name <- fs::path_ext_remove(fs::path_file(path))
  rows <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    raw_text <- as.character(df[[text_col]][[i]])
    if (is.na(raw_text) || nchar(trimws(raw_text)) == 0L) next

    doc_name <- if (!is.null(name_col) && name_col %in% names(df)) {
      as.character(df[[name_col]][[i]])
    } else {
      paste0(base_name, "_", i)
    }

    tryCatch({
      row <- qc_import_document(project, content = raw_text,
                                name = doc_name, language = language)

      if (!is.null(metadata_cols)) {
        for (col in intersect(metadata_cols, names(df))) {
          val <- df[[col]][[i]]
          if (!is.na(val))
            .exec(project$con,
              "INSERT INTO source_attributes (source_id, variable, value)
               VALUES (?, ?, ?)
               ON CONFLICT (source_id, variable)
               DO UPDATE SET value = excluded.value, status = 1",
              list(row$id, col, as.character(val))
            )
        }
      }

      rows[[i]] <- tibble::tibble(id = row$id, name = row$name,
                                  created_at = row$created_at,
                                  row = i)
    }, error = function(e) {
      cli::cli_warn("Row {i}: {conditionMessage(e)}")
    })
  }

  rows <- rows[!vapply(rows, is.null, logical(1L))]
  cli::cli_alert_success(
    "Imported {length(rows)} document{?s} from {.file {path}}")
  do.call(rbind, rows)
}


#' Segment a document into sub-units
#'
#' Splits the content of an existing document into child documents, each
#' stored with `parent_id` pointing to the original. Useful for splitting
#' long interview transcripts into paragraphs, sentences, or speaker turns
#' before coding.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document to segment.
#' @param method Segmentation method: `"paragraph"` (split on two or more
#'   consecutive newlines), `"sentence"` (linguistic sentence boundaries via
#'   `stringi`), `"speaker_turn"` (lines beginning with `SPEAKER:` or
#'   `Speaker:`), or `"response_id"` (lines beginning with a numeric or
#'   letter-prefixed identifier such as `Q1:` or `1.`).
#' @param min_chars Integer. Segments shorter than this (after trimming) are
#'   dropped (default `20L`).
#' @param pattern Character or `NULL`. Custom regex overriding the default
#'   turn/ID detection pattern (for `"speaker_turn"` and `"response_id"`).
#' @param keep_parent Logical. When `TRUE` (default) the original document is
#'   kept. When `FALSE` it is soft-deleted after its segments are created.
#'
#' @return A tibble with one row per segment: `id`, `name`, `created_at`,
#'   `segment_n`.
#' @export
qc_segment_document <- function(project, source_id,
                                 method      = c("paragraph", "sentence",
                                                 "speaker_turn",
                                                 "response_id"),
                                 min_chars   = 20L,
                                 pattern     = NULL,
                                 keep_parent = TRUE) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  method    <- match.arg(method)
  source_id <- as.integer(source_id)
  min_chars <- as.integer(min_chars)

  doc <- qc_get_document(project, source_id)

  segs <- switch(method,
    paragraph = .seg_paragraph(doc$content, min_chars),
    sentence  = .seg_sentence(doc$content, min_chars),
    speaker_turn = .seg_speaker_turn(doc$content, min_chars, pattern),
    response_id  = .seg_response_id(doc$content, min_chars, pattern)
  )

  if (length(segs) == 0L)
    rlang::abort("No segments found with these settings.")

  rows <- vector("list", length(segs))
  for (i in seq_along(segs)) {
    seg_name <- paste0(doc$name, " [", i, "/", length(segs), "]")
    row <- qc_import_document(project,
                              content   = segs[[i]],
                              name      = seg_name,
                              language  = doc$language %||% "",
                              parent_id = source_id)
    rows[[i]] <- tibble::tibble(id = row$id, name = row$name,
                                created_at = row$created_at,
                                segment_n  = i)
  }

  if (!keep_parent) qc_delete_document(project, source_id)

  cli::cli_alert_success(
    "Created {length(rows)} segment{?s} from '{doc$name}'.")
  do.call(rbind, rows)
}

.seg_paragraph <- function(content, min_chars) {
  segs <- strsplit(content, "\n{2,}", perl = TRUE)[[1L]]
  segs <- trimws(segs)
  segs[nchar(segs) >= min_chars]
}

.seg_sentence <- function(content, min_chars) {
  if (!requireNamespace("stringi", quietly = TRUE))
    rlang::abort("Install `stringi` for sentence segmentation.")
  segs <- stringi::stri_split_boundaries(content, type = "sentence")[[1L]]
  segs <- trimws(segs)
  segs[nchar(segs) >= min_chars]
}

.seg_speaker_turn <- function(content, min_chars, pattern) {
  p     <- pattern %||% "^([A-Z][A-Za-z0-9 _-]+):\\s*"
  lines <- strsplit(content, "\n")[[1L]]
  turns <- list()
  cur_speaker <- NULL
  cur_lines   <- character(0L)

  for (ln in lines) {
    if (grepl(p, ln, perl = TRUE)) {
      if (!is.null(cur_speaker) && length(cur_lines) > 0L) {
        turns <- c(turns,
          list(paste0(cur_speaker, ": ",
                      paste(trimws(cur_lines), collapse = " "))))
      }
      cur_speaker <- sub(p, "\\1", ln, perl = TRUE)
      rest        <- sub(p, "", ln, perl = TRUE)
      cur_lines   <- if (nchar(trimws(rest)) > 0L) rest else character(0L)
    } else if (!is.null(cur_speaker)) {
      cur_lines <- c(cur_lines, ln)
    }
  }
  if (!is.null(cur_speaker) && length(cur_lines) > 0L)
    turns <- c(turns,
      list(paste0(cur_speaker, ": ",
                  paste(trimws(cur_lines), collapse = " "))))

  segs <- vapply(turns, trimws, character(1L))
  segs[nchar(segs) >= min_chars]
}

.seg_response_id <- function(content, min_chars, pattern) {
  p     <- pattern %||% "^(Q?[0-9]+[.):]|[A-Z][0-9]+[.):])\\s*"
  lines <- strsplit(content, "\n")[[1L]]
  blocks <- list()
  cur    <- character(0L)

  for (ln in lines) {
    if (grepl(p, ln, perl = TRUE)) {
      if (length(cur) > 0L)
        blocks <- c(blocks, list(paste(cur, collapse = " ")))
      cur <- ln
    } else {
      cur <- c(cur, ln)
    }
  }
  if (length(cur) > 0L) blocks <- c(blocks, list(paste(cur, collapse = " ")))

  segs <- vapply(blocks, trimws, character(1L))
  segs[nchar(segs) >= min_chars]
}


#' Detect exact and near-duplicate documents
#'
#' Compares documents pairwise using MD5 hashes (exact duplicates) and
#' Jaccard similarity on word sets (near-duplicates). No additional packages
#' are required.
#'
#' @param project A `qc_project` object.
#' @param threshold Numeric in `[0, 1]`. Minimum Jaccard similarity to report
#'   as a near-duplicate (default `0.85`). Exact duplicates always appear
#'   regardless of threshold.
#' @param method One of `"both"`, `"exact"`, or `"near"`.
#' @param source_ids Integer vector or `NULL`. Restrict comparison to these
#'   documents.
#'
#' @return A tibble: `source_id_1`, `name_1`, `source_id_2`, `name_2`,
#'   `similarity`, `type` (`"exact"` or `"near"`). An empty tibble if no
#'   duplicates are found.
#' @export
qc_detect_duplicates <- function(project, threshold = 0.85,
                                  method     = c("both", "exact", "near"),
                                  source_ids = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  method <- match.arg(method)

  docs <- qc_list_documents(project, include_content = (method != "exact"))
  if (!is.null(source_ids))
    docs <- docs[docs$id %in% as.integer(source_ids), ]
  if (nrow(docs) < 2L)
    return(.empty_dupe_tibble())

  rows <- list()

  if (method %in% c("both", "exact")) {
    hashes <- .query(project$con, paste0(
      "SELECT id, content_hash FROM sources WHERE status = 1",
      if (!is.null(source_ids))
        paste0(" AND id IN (", paste(as.integer(source_ids), collapse=","), ")")
      else ""
    ))
    dups <- hashes[duplicated(hashes$content_hash) |
                   duplicated(hashes$content_hash, fromLast = TRUE), ]
    if (nrow(dups) > 0L) {
      pairs <- utils::combn(dups$id, 2L, simplify = FALSE)
      for (p in pairs) {
        h1 <- hashes$content_hash[hashes$id == p[[1L]]]
        h2 <- hashes$content_hash[hashes$id == p[[2L]]]
        if (!identical(h1, h2)) next
        rows <- c(rows, list(tibble::tibble(
          source_id_1 = p[[1L]],
          name_1      = docs$name[docs$id == p[[1L]]],
          source_id_2 = p[[2L]],
          name_2      = docs$name[docs$id == p[[2L]]],
          similarity  = 1,
          type        = "exact"
        )))
      }
    }
  }

  if (method %in% c("both", "near")) {
    if (!"content" %in% names(docs))
      docs <- qc_list_documents(project, include_content = TRUE)

    n <- nrow(docs)
    for (i in seq_len(n - 1L)) {
      for (j in seq(i + 1L, n)) {
        sim <- .jaccard_words(docs$content[[i]], docs$content[[j]])
        if (sim >= threshold && sim < 1) {
          rows <- c(rows, list(tibble::tibble(
            source_id_1 = docs$id[[i]],
            name_1      = docs$name[[i]],
            source_id_2 = docs$id[[j]],
            name_2      = docs$name[[j]],
            similarity  = round(sim, 3),
            type        = "near"
          )))
        }
      }
    }
  }

  if (length(rows) == 0L) return(.empty_dupe_tibble())
  do.call(rbind, rows)
}

.jaccard_words <- function(a, b) {
  wa <- unique(strsplit(tolower(a), "\\W+", perl = TRUE)[[1L]])
  wb <- unique(strsplit(tolower(b), "\\W+", perl = TRUE)[[1L]])
  wa <- wa[nchar(wa) > 0L]
  wb <- wb[nchar(wb) > 0L]
  if (length(wa) == 0L && length(wb) == 0L) return(1)
  length(intersect(wa, wb)) / length(union(wa, wb))
}

.empty_dupe_tibble <- function() {
  tibble::tibble(
    source_id_1 = integer(0), name_1      = character(0),
    source_id_2 = integer(0), name_2      = character(0),
    similarity  = numeric(0), type        = character(0)
  )
}
