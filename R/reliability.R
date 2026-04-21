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
