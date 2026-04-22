#' Full-text search across documents
#'
#' Finds all occurrences of a pattern within document content, returning each
#' match with surrounding context.
#'
#' @param project A `qc_project` object.
#' @param pattern Character. The search term or regular expression.
#' @param regex Logical. When `TRUE`, `pattern` is treated as a Perl-compatible
#'   regex. When `FALSE` (default), it is matched literally.
#' @param ignore_case Logical. Case-insensitive search (default `TRUE`).
#' @param source_ids Integer vector or `NULL`. Restrict to these documents.
#' @param context_chars Integer. Characters of surrounding text to include on
#'   each side of the match (default 80).
#' @param accent_fold Logical. When `TRUE` (requires `stringi`), both the
#'   document text and `pattern` are converted to ASCII-equivalent characters
#'   before matching, enabling accent-insensitive search (e.g. `"cafe"` matches
#'   `"café"`).
#'
#' @return A tibble: `source_id`, `source_name`, `match_n`, `match_start`,
#'   `match_end`, `match_text`, `context`.
#' @export
qc_search_documents <- function(project, pattern,
                                 regex         = FALSE,
                                 ignore_case   = TRUE,
                                 accent_fold   = FALSE,
                                 source_ids    = NULL,
                                 context_chars = 80L) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!is_string(pattern)) rlang::abort("`pattern` must be a single string.")

  docs <- qc_list_documents(project, include_content = TRUE)
  if (!is.null(source_ids))
    docs <- docs[docs$id %in% as.integer(source_ids), ]

  if (accent_fold) {
    if (!requireNamespace("stringi", quietly = TRUE))
      rlang::abort("Install `stringi` for accent-insensitive search.")
    fold <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")
  } else {
    fold <- identity
  }
  search_pattern <- fold(pattern)

  rows <- vector("list", nrow(docs))
  for (i in seq_len(nrow(docs))) {
    content        <- docs$content[[i]]
    search_content <- fold(content)
    m <- gregexpr(search_pattern, search_content,
                  perl        = regex,
                  fixed       = !regex,
                  ignore.case = ignore_case)[[1L]]
    if (m[[1L]] == -1L) next
    lens    <- attr(m, "match.length")
    ctx     <- as.integer(context_chars)
    n       <- length(m)
    rows[[i]] <- tibble::tibble(
      source_id   = docs$id[[i]],
      source_name = docs$name[[i]],
      match_n     = seq_len(n),
      match_start = as.integer(m),
      match_end   = as.integer(m) + lens - 1L,
      match_text  = substr(content, m, m + lens - 1L),
      context     = vapply(seq_len(n), function(j) {
        lo  <- max(1L, m[[j]] - ctx)
        hi  <- min(nchar(content), m[[j]] + lens[[j]] - 1L + ctx)
        substr(content, lo, hi)
      }, character(1L))
    )
  }

  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L)
    return(tibble::tibble(
      source_id = integer(0), source_name = character(0),
      match_n = integer(0), match_start = integer(0),
      match_end = integer(0), match_text = character(0),
      context = character(0)
    ))
  do.call(rbind, rows)
}

#' Apply a code automatically using a regular expression
#'
#' Scans document content for all matches of `pattern` and creates a
#' `'auto'`-sourced coding for each match. Useful for dictionary-based
#' or rule-based initial tagging.
#'
#' @param project A `qc_project` object.
#' @param code_id Integer. Code to apply.
#' @param pattern Character. Perl-compatible regular expression.
#' @param source_ids Integer vector or `NULL`. Restrict to these documents.
#' @param coder Character. Coder identifier (default `"auto"`).
#' @param ignore_case Logical. (default `TRUE`).
#'
#' @return Invisibly, the number of codings created.
#' @export
qc_auto_code <- function(project, code_id, pattern,
                          source_ids  = NULL,
                          coder       = "auto",
                          ignore_case = TRUE) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  code_id <- as.integer(code_id)
  if (!is_string(pattern)) rlang::abort("`pattern` must be a single string.")

  docs <- qc_list_documents(project, include_content = TRUE)
  if (!is.null(source_ids))
    docs <- docs[docs$id %in% as.integer(source_ids), ]

  n_total <- 0L
  for (i in seq_len(nrow(docs))) {
    content <- docs$content[[i]]
    m       <- gregexpr(pattern, content,
                        perl = TRUE, ignore.case = ignore_case)[[1L]]
    if (m[[1L]] == -1L) next
    lens <- attr(m, "match.length")
    for (j in seq_along(m)) {
      tryCatch(
        qc_add_coding(project,
                      source_id     = docs$id[[i]],
                      code_id       = code_id,
                      selfirst      = as.integer(m[[j]]),
                      selast        = as.integer(m[[j]]) + lens[[j]] - 1L,
                      coder         = coder,
                      coding_source = "auto",
                      coding_status = "draft"),
        error = function(e) NULL
      )
      n_total <- n_total + 1L
    }
  }

  cli::cli_alert_success("Created {n_total} auto-coding{?s}.")
  invisible(n_total)
}
