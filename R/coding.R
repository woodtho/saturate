# Append one row to coding_audit. All arguments after `operation` are optional.
.log_coding_audit <- function(con, coding_id, source_id, code_id, operation,
                               field      = NULL, old_value  = NULL,
                               new_value  = NULL, selfirst   = NULL,
                               selast     = NULL, seltext    = NULL,
                               coder      = NULL, changed_by = NULL) {
  .exec(con,
    "INSERT INTO coding_audit
       (coding_id, source_id, code_id, operation, field,
        old_value, new_value, selfirst, selast, seltext, coder, changed_by)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    list(as.integer(coding_id), as.integer(source_id), as.integer(code_id),
         operation,
         field      %||% NA_character_,
         old_value  %||% NA_character_,
         new_value  %||% NA_character_,
         selfirst   %||% NA_integer_,
         selast     %||% NA_integer_,
         seltext    %||% NA_character_,
         coder      %||% NA_character_,
         changed_by %||% NA_character_)
  )
}

#' Add a coded segment
#'
#' Records that the passage from character offset `selfirst` to `selast`
#' (1-based, both inclusive, matching `substr(content, selfirst, selast)`) in
#' document `source_id` is tagged with `code_id`. The passage text is
#' snapshotted into `seltext` at write time.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param code_id Integer. Code id.
#' @param selfirst Integer. 1-based start character position.
#' @param selast Integer. 1-based end character position (inclusive).
#' @param memo Character. Optional per-segment memo.
#' @param coder Character. Coder identifier (username or label).
#' @param coding_source One of `"manual"` or `"auto"`.
#' @param coding_status One of `"draft"` or `"validated"`.
#' @param confidence Integer 0-100 or `NULL`. Coder's confidence that this
#'   passage belongs under this code. `NULL` means unrated.
#'
#' @return A one-row tibble: `id`, `source_id`, `code_id`, `selfirst`,
#'   `selast`, `seltext`, `memo`, `coder`, `coding_source`,
#'   `coding_status`, `confidence`, `created_at`.
#' @export
qc_add_coding <- function(project, source_id, code_id,
                          selfirst, selast, memo = "",
                          coder         = "default",
                          coding_source = "manual",
                          coding_status = "validated",
                          confidence    = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  source_id <- as.integer(source_id)
  code_id   <- as.integer(code_id)
  selfirst  <- as.integer(selfirst)
  selast    <- as.integer(selast)
  if (selfirst < 1L) rlang::abort("`selfirst` must be >= 1.")
  if (selast < selfirst) rlang::abort("`selast` must be >= `selfirst`.")
  if (!is.null(confidence)) {
    confidence <- as.integer(confidence)
    if (confidence < 0L || confidence > 100L)
      rlang::abort("`confidence` must be between 0 and 100.")
  }

  dep_row <- .query(project$con,
    "SELECT deprecated FROM codes WHERE id = ? AND status = 1",
    list(code_id))
  if (nrow(dep_row) > 0L && isTRUE(dep_row$deprecated[[1L]] == 1L))
    rlang::abort(paste0("Code id = ", code_id, " is deprecated and cannot accept new codings."))

  doc     <- qc_get_document(project, source_id)
  seltext <- substr(doc$content, selfirst, selast)

  row <- if (is.null(confidence)) {
    .query(project$con,
      "INSERT INTO codings
         (source_id, code_id, selfirst, selast, seltext, memo,
          coder, coding_source, coding_status)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
       RETURNING id, source_id, code_id, selfirst, selast, seltext, memo,
                 coder, coding_source, coding_status, confidence, created_at",
      list(source_id, code_id, selfirst, selast, seltext, memo %||% "",
           coder %||% "default",
           coding_source %||% "manual",
           coding_status %||% "validated")
    )
  } else {
    .query(project$con,
      "INSERT INTO codings
         (source_id, code_id, selfirst, selast, seltext, memo,
          coder, coding_source, coding_status, confidence)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
       RETURNING id, source_id, code_id, selfirst, selast, seltext, memo,
                 coder, coding_source, coding_status, confidence, created_at",
      list(source_id, code_id, selfirst, selast, seltext, memo %||% "",
           coder %||% "default",
           coding_source %||% "manual",
           coding_status %||% "validated",
           confidence)
    )
  }
  .log_coding_audit(project$con,
    coding_id  = row$id,
    source_id  = row$source_id,
    code_id    = row$code_id,
    operation  = "create",
    selfirst   = row$selfirst,
    selast     = row$selast,
    seltext    = row$seltext,
    coder      = row$coder,
    changed_by = row$coder
  )
  row
}

#' List codings, optionally filtered
#'
#' @param project A `qc_project` object.
#' @param source_id Integer or `NULL`. Restrict to a single document.
#' @param code_id Integer or `NULL`. Restrict to a single code.
#' @param coder Character or `NULL`. Restrict to a single coder. Used to
#'   implement blind coding -- pass the current coder's name to hide all
#'   other coders' annotations.
#'
#' @return A tibble: `id`, `source_id`, `code_id`, `code_name`,
#'   `code_color`, `selfirst`, `selast`, `seltext`, `memo`, `coder`,
#'   `confidence`, `created_at`. Ordered by `selfirst`.
#' @export
qc_list_codings <- function(project, source_id = NULL, code_id = NULL,
                             coder = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  conds  <- "cod.status = 1"
  params <- list()
  if (!is.null(source_id)) {
    conds  <- paste0(conds, " AND cod.source_id = ?")
    params <- c(params, list(as.integer(source_id)))
  }
  if (!is.null(code_id)) {
    conds  <- paste0(conds, " AND cod.code_id = ?")
    params <- c(params, list(as.integer(code_id)))
  }
  if (!is.null(coder)) {
    conds  <- paste0(conds, " AND cod.coder = ?")
    params <- c(params, list(as.character(coder)))
  }
  .query(project$con, paste0("
    SELECT cod.id, cod.source_id, cod.code_id,
           c.name  AS code_name,
           c.color AS code_color,
           cod.selfirst, cod.selast, cod.seltext, cod.memo,
           cod.coder, cod.coding_status, cod.confidence, cod.created_at
    FROM   codings cod
    JOIN   codes c ON c.id = cod.code_id
    WHERE  ", conds, "
    ORDER  BY cod.selfirst
  "), params)
}

#' Delete a single coding (soft delete)
#'
#' @param project A `qc_project` object.
#' @param id Integer. The coding id.
#'
#' @return Invisibly, `1L`.
#' @export
qc_delete_coding <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id  <- as.integer(id)
  row <- .query(project$con,
    "SELECT id, source_id, code_id, selfirst, selast, seltext, coder
     FROM   codings WHERE id = ? AND status = 1", list(id))
  .soft_delete(project$con, "codings", "id", id)
  if (nrow(row) > 0L)
    .log_coding_audit(project$con,
      coding_id  = row$id[[1L]],
      source_id  = row$source_id[[1L]],
      code_id    = row$code_id[[1L]],
      operation  = "delete",
      selfirst   = row$selfirst[[1L]],
      selast     = row$selast[[1L]],
      seltext    = row$seltext[[1L]],
      coder      = row$coder[[1L]],
      changed_by = row$coder[[1L]]
    )
  invisible(1L)
}

#' Reassign a coding to a different code
#'
#' Moves a single coding from its current code to `new_code_id`. Useful after
#' splitting a code to redistribute existing passages.
#'
#' @param project A `qc_project` object.
#' @param coding_id Integer. The coding to reassign.
#' @param new_code_id Integer. The target code.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_reassign_coding <- function(project, coding_id, new_code_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  coding_id   <- as.integer(coding_id)
  new_code_id <- as.integer(new_code_id)
  row <- .query(project$con,
    "SELECT source_id, code_id, selfirst, selast, seltext, coder
     FROM   codings WHERE id = ? AND status = 1", list(coding_id))
  .exec(project$con,
    "UPDATE codings SET code_id = ? WHERE id = ? AND status = 1",
    list(new_code_id, coding_id)
  )
  if (nrow(row) > 0L)
    .log_coding_audit(project$con,
      coding_id  = coding_id,
      source_id  = row$source_id[[1L]],
      code_id    = new_code_id,
      operation  = "reassign",
      field      = "code_id",
      old_value  = as.character(row$code_id[[1L]]),
      new_value  = as.character(new_code_id),
      selfirst   = row$selfirst[[1L]],
      selast     = row$selast[[1L]],
      seltext    = row$seltext[[1L]],
      coder      = row$coder[[1L]],
      changed_by = row$coder[[1L]]
    )
  invisible(TRUE)
}

#' Update the memo on a single coding
#'
#' @param project A `qc_project` object.
#' @param id Integer. Coding id.
#' @param memo Character. New memo text.
#'
#' @return Invisibly `TRUE`.
#' @export
qc_update_coding_memo <- function(project, id, memo) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id  <- as.integer(id)
  row <- .query(project$con,
    "SELECT source_id, code_id, memo, coder FROM codings
     WHERE  id = ? AND status = 1", list(id))
  .exec(project$con,
    "UPDATE codings SET memo = ? WHERE id = ? AND status = 1",
    list(as.character(memo), id)
  )
  if (nrow(row) > 0L)
    .log_coding_audit(project$con,
      coding_id  = id,
      source_id  = row$source_id[[1L]],
      code_id    = row$code_id[[1L]],
      operation  = "update",
      field      = "memo",
      old_value  = row$memo[[1L]] %||% "",
      new_value  = as.character(memo),
      coder      = row$coder[[1L]],
      changed_by = row$coder[[1L]]
    )
  invisible(TRUE)
}

#' Update the confidence score on a single coding
#'
#' @param project A `qc_project` object.
#' @param id Integer. Coding id.
#' @param confidence Integer 0-100 or `NULL` (unrated).
#'
#' @return Invisibly `TRUE`.
#' @export
qc_update_coding_confidence <- function(project, id, confidence) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id  <- as.integer(id)
  row <- .query(project$con,
    "SELECT source_id, code_id, confidence, coder FROM codings
     WHERE  id = ? AND status = 1", list(id))
  if (is.null(confidence)) {
    .exec(project$con,
      "UPDATE codings SET confidence = NULL WHERE id = ? AND status = 1",
      list(id)
    )
  } else {
    conf <- as.integer(confidence)
    if (conf < 0L || conf > 100L)
      rlang::abort("`confidence` must be between 0 and 100.")
    .exec(project$con,
      "UPDATE codings SET confidence = ? WHERE id = ? AND status = 1",
      list(conf, id)
    )
  }
  if (nrow(row) > 0L)
    .log_coding_audit(project$con,
      coding_id  = id,
      source_id  = row$source_id[[1L]],
      code_id    = row$code_id[[1L]],
      operation  = "update",
      field      = "confidence",
      old_value  = if (is.na(row$confidence[[1L]])) NA_character_
                   else as.character(row$confidence[[1L]]),
      new_value  = if (is.null(confidence)) NA_character_
                   else as.character(as.integer(confidence)),
      coder      = row$coder[[1L]],
      changed_by = row$coder[[1L]]
    )
  invisible(TRUE)
}

#' Split a coding into two at a character position
#'
#' Soft-deletes the original coding and creates two replacements:
#' `[selfirst, split_at]` and `[split_at + 1, selast]`. Both children
#' inherit the original's code, coder, source, and coding metadata.
#'
#' @param project A `qc_project` object.
#' @param coding_id Integer. The coding to split.
#' @param split_at Integer. Absolute character position (same coordinate
#'   system as `selfirst`/`selast`). Must be in
#'   `[selfirst, selast - 1]`.
#' @param memo1,memo2 Character. Memos for the two new codings.
#'
#' @return A two-row tibble of the created codings.
#' @export
qc_split_coding <- function(project, coding_id,
                             split_at, memo1 = "", memo2 = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  coding_id <- as.integer(coding_id)
  split_at  <- as.integer(split_at)

  orig <- .query(project$con,
    "SELECT * FROM codings WHERE id = ? AND status = 1",
    list(coding_id)
  )
  if (nrow(orig) == 0L)
    rlang::abort(paste0("No active coding with id = ", coding_id))
  if (split_at < orig$selfirst || split_at >= orig$selast)
    rlang::abort(paste0(
      "`split_at` must be in [selfirst, selast - 1]: [",
      orig$selfirst, ", ", orig$selast - 1L, "]"
    ))

  .soft_delete(project$con, "codings", "id", coding_id)

  r1 <- qc_add_coding(project,
    source_id     = orig$source_id,
    code_id       = orig$code_id,
    selfirst      = orig$selfirst,
    selast        = split_at,
    memo          = if (nchar(memo1) > 0L) memo1 else orig$memo %||% "",
    coder         = orig$coder %||% "default",
    coding_source = orig$coding_source %||% "manual",
    coding_status = orig$coding_status %||% "validated"
  )
  r2 <- qc_add_coding(project,
    source_id     = orig$source_id,
    code_id       = orig$code_id,
    selfirst      = split_at + 1L,
    selast        = orig$selast,
    memo          = if (nchar(memo2) > 0L) memo2 else orig$memo %||% "",
    coder         = orig$coder %||% "default",
    coding_source = orig$coding_source %||% "manual",
    coding_status = orig$coding_status %||% "validated"
  )
  rbind(r1, r2)
}

#' Merge two or more codings into one
#'
#' All codings must belong to the same document. The merged coding spans
#' `min(selfirst)` to `max(selast)` of the group. Input codings are
#' soft-deleted.
#'
#' @param project A `qc_project` object.
#' @param coding_ids Integer vector. At least two coding ids.
#' @param code_id Integer or `NULL`. Code for the merged coding. Defaults
#'   to the code of the first coding in `coding_ids`.
#' @param memo Character. Memo for the merged coding.
#'
#' @return A one-row tibble of the created coding.
#' @export
qc_merge_codings <- function(project, coding_ids, code_id = NULL,
                              memo = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  coding_ids <- as.integer(coding_ids)
  if (length(coding_ids) < 2L)
    rlang::abort("Supply at least two coding ids.")

  ids_sql <- paste(coding_ids, collapse = ",")
  rows <- .query(project$con, paste0(
    "SELECT * FROM codings WHERE id IN (", ids_sql, ") AND status = 1"
  ))
  if (nrow(rows) != length(coding_ids))
    rlang::abort("One or more coding ids not found or already deleted.")
  if (length(unique(rows$source_id)) > 1L)
    rlang::abort("All codings must belong to the same document.")

  merged_code  <- if (!is.null(code_id)) as.integer(code_id)
                  else rows$code_id[[1L]]
  merged_coder <- rows$coder[[1L]] %||% "default"

  for (id in coding_ids)
    .soft_delete(project$con, "codings", "id", id)

  qc_add_coding(project,
    source_id     = rows$source_id[[1L]],
    code_id       = merged_code,
    selfirst      = min(rows$selfirst),
    selast        = max(rows$selast),
    memo          = memo,
    coder         = merged_coder,
    coding_source = "manual",
    coding_status = "validated"
  )
}

#' Find uncoded text segments in a document
#'
#' Splits a document into paragraphs or sentences and returns those that have
#' no overlapping active coding. Useful for navigating to unreviewed text.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param unit One of `"paragraph"` (default) or `"sentence"`.
#' @param min_chars Integer. Minimum segment length to report (default 20).
#'
#' @return A tibble: `start`, `end`, `text`.
#' @export
qc_uncoded_segments <- function(project, source_id,
                                 unit      = c("paragraph", "sentence"),
                                 min_chars = 20L) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  unit      <- match.arg(unit)
  source_id <- as.integer(source_id)
  min_chars <- as.integer(min_chars)

  doc     <- qc_get_document(project, source_id)
  content <- doc$content
  n       <- nchar(content)

  EMPTY <- tibble::tibble(start = integer(), end = integer(),
                          text  = character())
  if (n == 0L) return(EMPTY)

  units <- if (unit == "paragraph") .split_paragraphs(content)
           else                     .split_sentences(content)
  if (nrow(units) == 0L) return(EMPTY)

  codings <- qc_list_codings(project, source_id)

  keep <- vapply(seq_len(nrow(units)), function(i) {
    s   <- units$start[[i]]
    e   <- units$end[[i]]
    if ((e - s + 1L) < min_chars) return(FALSE)
    if (nrow(codings) == 0L) return(TRUE)
    !any(codings$selfirst <= e & codings$selast >= s)
  }, logical(1L))

  units[keep, ]
}

#' Find disputed or draft-status segments in a document
#'
#' Returns codings that are either in `"draft"` status or that overlap with
#' another coder's coding on a different code (a coder conflict).
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#'
#' @return A tibble: `coding_id`, `code_name`, `coder`, `reason`,
#'   `selfirst`, `selast`, `seltext`. Ordered by `selfirst`.
#' @export
qc_disputed_segments <- function(project, source_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  source_id <- as.integer(source_id)

  EMPTY <- tibble::tibble(
    coding_id = integer(), code_name = character(),
    coder     = character(), reason    = character(),
    selfirst  = integer(),  selast    = integer(),
    seltext   = character()
  )

  drafts <- .query(project$con,
    "SELECT cod.id        AS coding_id,
            c.name        AS code_name,
            cod.coder     AS coder,
            'draft'       AS reason,
            cod.selfirst, cod.selast, cod.seltext
     FROM   codings cod
     JOIN   codes c ON c.id = cod.code_id
     WHERE  cod.source_id = ? AND cod.status = 1
       AND  cod.coding_status = 'draft'",
    list(source_id))

  conflicts <- .query(project$con,
    "SELECT DISTINCT
            c1.id         AS coding_id,
            cod1.name     AS code_name,
            c1.coder      AS coder,
            'coder_conflict' AS reason,
            c1.selfirst, c1.selast, c1.seltext
     FROM   codings c1
     JOIN   codes  cod1 ON cod1.id = c1.code_id
     JOIN   codings c2  ON c2.source_id = c1.source_id
                       AND c2.id       != c1.id
                       AND c2.status    = 1
                       AND c2.coder    != c1.coder
                       AND c2.code_id  != c1.code_id
                       AND c2.selfirst <= c1.selast
                       AND c2.selast   >= c1.selfirst
     WHERE  c1.source_id = ? AND c1.status = 1",
    list(source_id))

  result <- rbind(drafts, conflicts)
  if (nrow(result) == 0L) return(EMPTY)

  # Deduplicate: keep one row per coding_id
  result <- result[!duplicated(result$coding_id), ]
  result[order(result$selfirst), ]
}

# -- Internal document splitting helpers ----------------------------------------

.split_paragraphs <- function(content) {
  m <- gregexpr("\\n{2,}", content, perl = TRUE)[[1L]]
  if (m[[1L]] == -1L)
    return(tibble::tibble(start = 1L, end = nchar(content), text = content))

  sep_starts  <- as.integer(m)
  sep_lengths <- attr(m, "match.length")
  para_starts <- c(1L, sep_starts + sep_lengths)
  para_ends   <- c(sep_starts - 1L, nchar(content))

  keep <- nchar(trimws(substr(content, para_starts, para_ends))) > 0L
  tibble::tibble(
    start = para_starts[keep],
    end   = para_ends[keep],
    text  = substr(content, para_starts[keep], para_ends[keep])
  )
}

.split_sentences <- function(content) {
  m <- gregexpr("[.!?]+\\s+", content, perl = TRUE)[[1L]]
  if (m[[1L]] == -1L)
    return(tibble::tibble(start = 1L, end = nchar(content), text = content))

  sep_starts  <- as.integer(m)
  sep_lengths <- attr(m, "match.length")
  sent_starts <- c(1L, sep_starts + sep_lengths)
  sent_ends   <- c(sep_starts + sep_lengths - 1L, nchar(content))

  keep <- sent_starts <= sent_ends &
          nchar(trimws(substr(content, sent_starts, sent_ends))) > 0L
  tibble::tibble(
    start = sent_starts[keep],
    end   = sent_ends[keep],
    text  = substr(content, sent_starts[keep], sent_ends[keep])
  )
}

# Build highlighted HTML from document content and a codings tibble.
# Used by the Shiny coding panel.
#
# Builds the document HTML with coding highlights, excerpt underlines,
# memo icons, optional bold markdown, and optional line numbers.
#
# codings:          tibble from qc_list_codings -- must have id, code_id,
#                   code_color, code_name, selfirst, selast, memo.
# excerpts:         tibble from qc_list_excerpts or NULL -- selfirst, selast, memo.
# opacity:          numeric 0-1, highlight background alpha.
# cb_mode:          TRUE -> border-bottom instead of fill (colorblind-safe).
# highlight_codes:  integer vector; when not NULL only these code_ids are shown.
# show_line_numbers: TRUE -> prepend line numbers to each line.
build_highlighted_html <- function(content, codings,
                                    opacity           = 0.33,
                                    cb_mode           = FALSE,
                                    highlight_codes   = NULL,
                                    excerpts          = NULL,
                                    show_line_numbers = FALSE,
                                    show_timestamps   = TRUE,
                                    search_ranges     = NULL) {
  n <- nchar(content)

  make_div <- function(inner_html, with_ln = FALSE) {
    cls <- paste(
      if (with_ln) "qc-text-display qc-line-numbers-on" else "qc-text-display",
      if (isTRUE(show_timestamps)) "qc-timestamps-on" else ""
    )
    htmltools::div(
      class        = trimws(cls),
      role         = "region",
      tabindex     = "0",
      `aria-label` = "Document text \u2014 select a passage then choose a code to apply",
      style        = .text_display_style(),
      htmltools::HTML(inner_html)
    )
  }

  if (!is.null(highlight_codes))
    codings <- codings[codings$code_id %in% as.integer(highlight_codes), ]

  if (!is.null(excerpts) && !is.data.frame(excerpts))    excerpts <- NULL
  if (!is.null(excerpts) && nrow(excerpts) == 0L)        excerpts <- NULL
  if (!is.null(search_ranges) && !is.data.frame(search_ranges)) search_ranges <- NULL
  if (!is.null(search_ranges) && nrow(search_ranges) == 0L)     search_ranges <- NULL

  no_codings  <- nrow(codings) == 0L
  no_excerpts <- is.null(excerpts)
  no_search   <- is.null(search_ranges)

  if ((no_codings && no_excerpts && no_search) || n == 0L) {
    raw <- .apply_bold(htmltools::htmlEscape(content))
    if (isTRUE(show_timestamps)) raw <- .wrap_timestamps(raw)
    if (show_line_numbers) raw <- .add_line_numbers(raw, merge_timestamps = isTRUE(show_timestamps))
    return(make_div(raw, with_ln = show_line_numbers))
  }

  # Alpha hex suffix (clamped to [0.05, 1.0])
  alpha_hex <- sprintf("%02X", as.integer(round(pmin(pmax(opacity, 0.05), 1.0) * 255)))

  # Build break points from codings, excerpts, and search matches
  c_breaks <- if (!no_codings)  c(codings$selfirst,      codings$selast      + 1L) else integer(0)
  e_breaks <- if (!no_excerpts) c(excerpts$selfirst,     excerpts$selast     + 1L) else integer(0)
  s_breaks <- if (!no_search)   c(search_ranges$selfirst, search_ranges$selast + 1L) else integer(0)
  breaks   <- sort(unique(c(1L, c_breaks, e_breaks, s_breaks, n + 1L)))
  breaks   <- breaks[breaks >= 1L & breaks <= n + 1L]

  if (show_line_numbers) {
    nl_pos <- .find_newlines(content)
    if (length(nl_pos) > 0L) {
      breaks <- sort(unique(c(breaks, nl_pos, nl_pos + 1L)))
      breaks <- breaks[breaks >= 1L & breaks <= n + 1L]
    }
  }

  html_parts <- character(length(breaks) - 1L)
  for (i in seq_along(html_parts)) {
    seg_start <- breaks[i]
    seg_end   <- breaks[i + 1L] - 1L
    seg_raw   <- substr(content, seg_start, seg_end)
    seg_html  <- .apply_bold(htmltools::htmlEscape(seg_raw))

    active_c <- if (!no_codings)
      codings[codings$selfirst <= seg_start & codings$selast >= seg_start, ]
    else
      codings[integer(0), ]

    active_e <- if (!no_excerpts)
      excerpts[excerpts$selfirst <= seg_start & excerpts$selast >= seg_start, ]
    else
      NULL

    in_search <- if (!no_search)
      any(search_ranges$selfirst <= seg_start & search_ranges$selast >= seg_start)
    else
      FALSE

    has_coding  <- nrow(active_c) > 0L
    has_excerpt <- !is.null(active_e) && nrow(active_e) > 0L

    # Search highlight prefix/suffix -- layered on top of coding/excerpt marks
    s_open  <- if (in_search)
      '<mark class="qc-search-match">'
    else ""
    s_close <- if (in_search) "</mark>" else ""

    if (!has_coding && !has_excerpt && !in_search) {
      html_parts[[i]] <- seg_html
      next
    }

    if (!has_coding && !has_excerpt) {
      html_parts[[i]] <- paste0(s_open, seg_html, s_close)
      next
    }

    if (has_coding) {
      col  <- active_c$code_color[[1L]]
      tip  <- htmltools::htmlEscape(paste(active_c$code_name, collapse = ", "))
      aria <- htmltools::htmlEscape(paste0("Coded as: ", tip))
      hex  <- if (grepl("^#[0-9A-Fa-f]{6}$", col)) col else "#4E79A7"

      style <- if (cb_mode) {
        paste0("background-color:rgba(0,0,0,0.06);",
               "border-bottom:3px solid ", hex, ";",
               "border-radius:0;cursor:pointer;")
      } else {
        paste0("background-color:", hex, alpha_hex, ";cursor:pointer;")
      }

      ids_str   <- paste(active_c$id, collapse = ",")
      memos_txt <- paste(active_c$memo[nzchar(active_c$memo %||% "")], collapse = "; ")
      memo_icon <- if (nzchar(memos_txt))
        paste0('<sup class="qc-memo-icon" title="', htmltools::htmlEscape(memos_txt),
               '" aria-label="Memo">&#10148;</sup>')
      else ""

      html_parts[[i]] <- paste0(
        s_open,
        '<mark role="mark"',
        ' aria-label="', aria, '"',
        ' title="',      tip,  '"',
        ' style="',      style, '"',
        ' data-selfirst="',   seg_start, '"',
        ' data-selast="',     seg_end,   '"',
        ' data-coding-ids="', ids_str,   '">',
        seg_html, memo_icon,
        '</mark>',
        s_close
      )
    } else {
      # Excerpt-only segment
      exc_memo <- paste(
        active_e$memo[nzchar(active_e$memo %||% "")], collapse = "; ")
      exc_tip  <- htmltools::htmlEscape(
        if (nzchar(exc_memo)) paste0("Excerpt: ", exc_memo) else "Excerpt"
      )
      memo_icon <- if (nzchar(exc_memo))
        paste0('<sup class="qc-memo-icon" title="',
               htmltools::htmlEscape(exc_memo),
               '" aria-label="Excerpt memo">&#10148;</sup>')
      else ""
      html_parts[[i]] <- paste0(
        s_open,
        '<span class="qc-excerpt" title="', exc_tip, '"',
        ' style="border-bottom:2px dashed var(--sat-text-muted);cursor:default;">',
        seg_html, memo_icon,
        '</span>',
        s_close
      )
    }
  }

  html_out <- paste(html_parts, collapse = "")
  if (isTRUE(show_timestamps)) html_out <- .wrap_timestamps(html_out)
  if (show_line_numbers) html_out <- .add_line_numbers(html_out, merge_timestamps = isTRUE(show_timestamps))
  make_div(html_out, with_ln = show_line_numbers)
}

# Convert **bold** markdown syntax to <strong> within already-HTML-escaped text.
# Double-asterisks are not HTML-special so no entity mangling occurs.
.apply_bold <- function(html) {
  gsub("\\*\\*([^*\n]+)\\*\\*", "<strong>\\1</strong>", html, perl = TRUE)
}

# Wrap [HH:MM:SS] timestamp markers with a styled span so CSS can render them
# like line-number gutters. The container must carry class qc-timestamps-on.
.wrap_timestamps <- function(html) {
  gsub(
    "(\\[)(\\d{2}:\\d{2}:\\d{2})(\\])",
    '<span class="qc-ts-marker" aria-hidden="true">\\1\\2\\3</span>',
    html,
    perl = TRUE
  )
}

# Wrap each newline-delimited line in a flex row with a gutter number column.
# merge_timestamps: when TRUE, any qc-ts-marker at the very start of a line is
# pulled into the gutter (below the line number) instead of staying inline.
.add_line_numbers <- function(html, merge_timestamps = FALSE) {
  lines    <- strsplit(html, "\n", fixed = TRUE)[[1L]]
  n_digits <- nchar(as.character(length(lines)))

  # Pattern that matches a leading timestamp marker span (produced by .wrap_timestamps)
  ts_pattern <- '^<span class="qc-ts-marker" aria-hidden="true">(\\[\\d{2}:\\d{2}:\\d{2}\\])</span>'

  rows <- vapply(seq_along(lines), function(i) {
    line_html <- lines[[i]]
    num       <- formatC(i, width = n_digits, flag = " ")

    gutter <- num
    if (merge_timestamps) {
      m <- regmatches(line_html, regexpr(ts_pattern, line_html, perl = TRUE))
      if (length(m) == 1L && nchar(m) > 0L) {
        ts_text <- regmatches(m, regexpr("\\[\\d{2}:\\d{2}:\\d{2}\\]", m))
        gutter  <- paste0(
          num,
          '<span class="qc-ln-ts" aria-hidden="true">', ts_text, '</span>'
        )
        line_html <- sub(ts_pattern, "", line_html, perl = TRUE)
      }
    }

    paste0(
      '<div class="qc-line">',
      '<span class="qc-line-num" aria-hidden="true">', gutter, '</span>',
      '<span class="qc-line-text">', line_html, '</span>',
      '</div>'
    )
  }, character(1L))
  paste(rows, collapse = "")
}

.find_newlines <- function(content) {
  m <- gregexpr("\n", content, fixed = TRUE)[[1L]]
  if (length(m) == 1L && m[[1L]] == -1L) return(integer(0))
  as.integer(m)
}

.text_display_style <- function() "user-select: text;"

#' Retrieve the coding audit log
#'
#' Returns an append-only record of every coding operation (create, delete,
#' update, reassign) across the project. Combined with [qc_code_history()] this
#' gives a complete audit trail of all analytical decisions.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer or `NULL`. Filter to a single document.
#' @param operation Character or `NULL`. One of `"create"`, `"delete"`,
#'   `"update"`, `"reassign"`.
#' @param from_date Date/POSIXct or `NULL`. Earliest `changed_at` to include.
#' @param to_date   Date/POSIXct or `NULL`. Latest `changed_at` to include.
#'
#' @return A tibble ordered by `changed_at` descending: `id`, `coding_id`,
#'   `operation`, `field`, `old_value`, `new_value`, `source_name`,
#'   `code_name`, `selfirst`, `selast`, `seltext`, `coder`, `changed_by`,
#'   `changed_at`.
#' @export
qc_coding_audit <- function(project, source_id = NULL, operation = NULL,
                             from_date = NULL, to_date = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  w_src  <- if (!is.null(source_id))
    paste0("AND ca.source_id = ", as.integer(source_id)) else ""
  w_op   <- if (!is.null(operation))
    paste0("AND ca.operation = '", operation, "'") else ""
  w_from <- if (!is.null(from_date))
    paste0("AND ca.changed_at >= TIMESTAMPTZ '",
           format(as.POSIXct(from_date), "%Y-%m-%d %H:%M:%S"), "'") else ""
  w_to   <- if (!is.null(to_date))
    paste0("AND ca.changed_at <= TIMESTAMPTZ '",
           format(as.POSIXct(to_date),   "%Y-%m-%d %H:%M:%S"), "'") else ""

  .query(project$con, paste0("
    SELECT ca.id, ca.coding_id, ca.operation, ca.field,
           ca.old_value, ca.new_value,
           s.name  AS source_name,
           c.name  AS code_name,
           ca.selfirst, ca.selast, ca.seltext,
           ca.coder, ca.changed_by, ca.changed_at
    FROM   coding_audit ca
    LEFT   JOIN sources s ON s.id = ca.source_id
    LEFT   JOIN codes   c ON c.id = ca.code_id
    WHERE  1 = 1
    ", w_src, w_op, w_from, w_to, "
    ORDER  BY ca.changed_at DESC
  "))
}
