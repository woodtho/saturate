#' List coders and their coding activity
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble: `coder`, `n_codings`, `n_documents`, `n_codes`.
#' @export
qc_list_coders <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .query(project$con, "
    SELECT cod.coder,
           COUNT(*)                      AS n_codings,
           COUNT(DISTINCT cod.source_id) AS n_documents,
           COUNT(DISTINCT cod.code_id)   AS n_codes
    FROM   codings cod
    WHERE  cod.status = 1
    GROUP  BY cod.coder
    ORDER  BY n_codings DESC
  ")
}

#' Pairwise inter-coder agreement (Cohen's kappa) for one code
#'
#' Computes Cohen's kappa at the document level: for each document coded by
#' at least one of the two coders, was the code applied (1) or not (0)?
#' Kappa measures agreement beyond chance on this binary decision.
#'
#' @param project A `qc_project` object.
#' @param code_id Integer. The code to evaluate.
#' @param coder1 Character. First coder identifier.
#' @param coder2 Character. Second coder identifier.
#'
#' @return A one-row tibble: `code_id`, `code_name`, `coder1`, `coder2`,
#'   `n_docs`, `n_agree`, `pct_agree`, `kappa`, `n11`, `n10`, `n01`, `n00`.
#'   - `n11` = both coded, `n10` = coder1 only, `n01` = coder2 only,
#'   - `n00` = neither (among docs seen by at least one coder).
#' @export
qc_agreement <- function(project, code_id, coder1, coder2) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  code_id <- as.integer(code_id)

  code_name <- .query(project$con,
    "SELECT name FROM codes WHERE id = ? AND status = 1", list(code_id))
  if (nrow(code_name) == 0L)
    rlang::abort(paste0("No active code with id = ", code_id))

  # All documents coded by either coder with ANY code
  all_docs <- .query(project$con, "
    SELECT DISTINCT source_id FROM codings
    WHERE  status = 1 AND coder IN (?, ?)
  ", list(coder1, coder2))

  if (nrow(all_docs) == 0L)
    rlang::abort("No codings found for either coder.")

  # Per-document binary indicator for each coder
  c1_docs <- .query(project$con, "
    SELECT DISTINCT source_id FROM codings
    WHERE  status = 1 AND code_id = ? AND coder = ?
  ", list(code_id, coder1))$source_id
  c2_docs <- .query(project$con, "
    SELECT DISTINCT source_id FROM codings
    WHERE  status = 1 AND code_id = ? AND coder = ?
  ", list(code_id, coder2))$source_id

  docs <- all_docs$source_id
  v1   <- docs %in% c1_docs
  v2   <- docs %in% c2_docs

  n11 <- sum( v1 &  v2)
  n10 <- sum( v1 & !v2)
  n01 <- sum(!v1 &  v2)
  n00 <- sum(!v1 & !v2)
  n   <- length(docs)

  p_o <- (n11 + n00) / n
  p_e <- ((n11 + n10) / n) * ((n11 + n01) / n) +
         ((n01 + n00) / n) * ((n10 + n00) / n)
  kap <- if (abs(1 - p_e) < 1e-10) NA_real_ else (p_o - p_e) / (1 - p_e)

  tibble::tibble(
    code_id   = code_id,
    code_name = code_name$name[[1L]],
    coder1    = coder1,
    coder2    = coder2,
    n_docs    = n,
    n_agree   = n11 + n00,
    pct_agree = round(p_o * 100, 1),
    kappa     = round(kap, 3),
    n11 = n11, n10 = n10, n01 = n01, n00 = n00
  )
}

#' Krippendorff's alpha for one code across multiple coders
#'
#' Computes Krippendorff's alpha (nominal metric) for a single code across any
#' number of coders. The unit of analysis is the document: each coder either
#' applied the code to a document (1) or did not (0). Only documents coded by
#' at least two of the specified coders contribute to the calculation.
#'
#' Alpha interpretation: > 0.8 = strong, 0.67-0.8 = tentative, < 0.67 =
#' unreliable (Krippendorff 2004 thresholds for content analysis).
#'
#' @param project A `qc_project` object.
#' @param code_id Integer. The code to evaluate.
#' @param coders Character vector or `NULL`. Restrict to these coders.
#'   Defaults to all coders in the project.
#'
#' @return A one-row tibble: `code_id`, `code_name`, `n_coders`, `n_units`,
#'   `observed_disagreement`, `expected_disagreement`, `alpha`.
#' @export
qc_krippendorff <- function(project, code_id, coders = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  code_id <- as.integer(code_id)

  code_nm <- .query(project$con,
    "SELECT name FROM codes WHERE id = ? AND status = 1", list(code_id))
  if (nrow(code_nm) == 0L)
    rlang::abort(paste0("No active code with id = ", code_id))

  if (is.null(coders)) coders <- qc_list_coders(project)$coder
  if (length(coders) < 2L) rlang::abort("At least two coders are needed.")

  coder_in <- paste(
    paste0("'", gsub("'", "''", coders), "'"), collapse = ","
  )

  # All (source_id, coder) pairs where any coding was made by these coders
  all_pairs <- .query(project$con, paste0(
    "SELECT DISTINCT source_id, coder FROM codings
     WHERE status = 1 AND coder IN (", coder_in, ")"
  ))
  if (nrow(all_pairs) == 0L)
    rlang::abort("No codings found for the specified coders.")

  # (source_id, coder) pairs where code_id was specifically applied
  target_pairs <- .query(project$con, paste0(
    "SELECT DISTINCT source_id, coder FROM codings
     WHERE status = 1 AND code_id = ", code_id,
    " AND coder IN (", coder_in, ")"
  ))

  # Per-document coder counts from R (avoids per-doc SQL round-trips)
  doc_m <- table(all_pairs$source_id)    # m_u: coders who touched each doc
  doc_k <- table(target_pairs$source_id) # k_u: coders who applied code_id

  valid_docs <- names(doc_m)[doc_m >= 2L]
  if (length(valid_docs) < 2L)
    rlang::abort("Fewer than 2 documents have been coded by \u22652 coders.")

  do_num  <- 0
  do_den  <- 0
  n_total <- 0L
  n_ones  <- 0L

  for (doc_id in valid_docs) {
    m_u <- as.integer(doc_m[[doc_id]])
    k_u <- if (doc_id %in% names(doc_k)) as.integer(doc_k[[doc_id]]) else 0L
    do_num  <- do_num + k_u * (m_u - k_u)
    do_den  <- do_den + m_u * (m_u - 1L) / 2
    n_total <- n_total + m_u
    n_ones  <- n_ones  + k_u
  }

  D_o   <- if (do_den > 0) do_num / do_den else 0
  n_zeros <- n_total - n_ones
  D_e   <- if (n_total > 1L)
    2 * n_ones * n_zeros / (n_total * (n_total - 1L)) else NA_real_
  alpha <- if (!is.na(D_e) && abs(D_e) > 1e-10) 1 - D_o / D_e else NA_real_

  tibble::tibble(
    code_id                = code_id,
    code_name              = code_nm$name[[1L]],
    n_coders               = length(coders),
    n_units                = length(valid_docs),
    observed_disagreement  = round(D_o, 4),
    expected_disagreement  = round(D_e, 4),
    alpha                  = round(alpha, 3)
  )
}

#' Agreement matrix across all coder pairs and codes
#'
#' Calls [qc_agreement()] for every combination of coders and codes
#' that appear in the project.
#'
#' @param project A `qc_project` object.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#'
#' @return A tibble with one row per (code, coder1, coder2) triple.
#' @export
qc_agreement_matrix <- function(project, code_ids = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  coders <- qc_list_coders(project)$coder
  if (length(coders) < 2L)
    rlang::abort("At least two distinct coders are needed.")

  codes <- qc_list_codes(project)
  if (!is.null(code_ids))
    codes <- codes[codes$id %in% as.integer(code_ids), ]

  pairs  <- utils::combn(coders, 2L, simplify = FALSE)
  rows   <- vector("list", nrow(codes) * length(pairs))
  k      <- 0L
  for (cid in codes$id) {
    for (p in pairs) {
      k       <- k + 1L
      rows[[k]] <- tryCatch(
        qc_agreement(project, cid, p[[1L]], p[[2L]]),
        error = function(e) NULL
      )
    }
  }
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L)
    return(tibble::tibble())
  do.call(rbind, rows)
}
