#' Retrieve all coded segments, optionally filtered
#'
#' The primary analysis function. Returns a flat tibble of every coded passage.
#'
#' @param project A `qc_project` object.
#' @param code_ids Integer vector or `NULL`. OR filter — return segments that
#'   carry any of these codes.
#' @param must_have Integer vector or `NULL`. AND filter — restrict to
#'   documents that contain *all* of these codes somewhere (any segment).
#' @param must_not Integer vector or `NULL`. NOT filter — exclude documents
#'   that carry any of these codes.
#' @param source_ids Integer vector or `NULL`. Restrict to these documents.
#' @param case_ids Integer vector or `NULL`. Restrict to documents linked to
#'   these cases.
#' @param category_ids Integer vector or `NULL`. Restrict to codes in these
#'   categories.
#' @param coder Character or `NULL`. Restrict to codings by this coder.
#' @param coding_source One of `"manual"`, `"auto"`, or `NULL` for all.
#' @param coding_status One of `"draft"`, `"validated"`, or `NULL` for all.
#'
#' @return A tibble: `coding_id`, `source_id`, `source_name`, `code_id`,
#'   `code_name`, `code_color`, `category_names`, `selfirst`, `selast`,
#'   `seltext`, `memo`, `coder`, `coding_source`, `coding_status`,
#'   `created_at`.
#' @export
qc_get_coded_segments <- function(project,
                                  code_ids      = NULL,
                                  must_have     = NULL,
                                  must_not      = NULL,
                                  source_ids    = NULL,
                                  case_ids      = NULL,
                                  category_ids  = NULL,
                                  coder         = NULL,
                                  coding_source = NULL,
                                  coding_status = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  w_codes   <- .in_clause("cod.code_id",   code_ids)
  w_sources <- .in_clause("cod.source_id", source_ids)
  w_must    <- .must_have_clause(must_have)
  w_not     <- if (!is.null(must_not)) {
    ids <- paste(as.integer(must_not), collapse = ",")
    paste0("AND cod.source_id NOT IN (
      SELECT source_id FROM codings
      WHERE  code_id IN (", ids, ") AND status = 1
    )")
  } else ""
  w_cats    <- if (!is.null(category_ids)) {
    ids <- paste(as.integer(category_ids), collapse = ",")
    paste0("AND cod.code_id IN (
      SELECT code_id FROM code_category_links
      WHERE  category_id IN (", ids, ") AND status = 1
    )")
  } else ""
  w_cases   <- if (!is.null(case_ids)) {
    ids <- paste(as.integer(case_ids), collapse = ",")
    paste0("AND cod.source_id IN (
      SELECT source_id FROM case_source_links
      WHERE  case_id IN (", ids, ") AND status = 1
    )")
  } else ""
  w_coder   <- if (!is.null(coder))
    paste0("AND cod.coder = '", coder, "'") else ""
  w_csrc    <- if (!is.null(coding_source))
    paste0("AND cod.coding_source = '", coding_source, "'") else ""
  w_cstat   <- if (!is.null(coding_status))
    paste0("AND cod.coding_status = '", coding_status, "'") else ""

  sql <- paste0("
    SELECT cod.id                                             AS coding_id,
           cod.source_id,
           s.name                                            AS source_name,
           cod.code_id,
           c.name                                            AS code_name,
           c.color                                           AS code_color,
           STRING_AGG(DISTINCT cat.name, ', ' ORDER BY cat.name)
                                                            AS category_names,
           cod.selfirst, cod.selast, cod.seltext, cod.memo,
           cod.coder, cod.coding_source, cod.coding_status,
           cod.created_at
    FROM   codings cod
    JOIN   sources s  ON s.id  = cod.source_id
    JOIN   codes   c  ON c.id  = cod.code_id
    LEFT   JOIN code_category_links l   ON l.code_id    = c.id  AND l.status = 1
    LEFT   JOIN code_categories     cat ON cat.id = l.category_id AND cat.status = 1
    WHERE  cod.status = 1
    ", w_codes, w_sources, w_must, w_not, w_cats, w_cases,
       w_coder, w_csrc, w_cstat, "
    GROUP  BY cod.id, cod.source_id, s.name, cod.code_id, c.name, c.color,
              cod.selfirst, cod.selast, cod.seltext, cod.memo,
              cod.coder, cod.coding_source, cod.coding_status, cod.created_at
    ORDER  BY s.name, cod.selfirst
  ")

  .query(project$con, sql)
}

#' Code co-occurrence matrix
#'
#' Returns a long-format table of code pairs that co-occur within the same
#' unit (document or overlapping segment).
#'
#' @param project A `qc_project` object.
#' @param unit One of `"document"` (default) or `"segment"`. `"document"`
#'   counts pairs that appear anywhere in the same document; `"segment"`
#'   counts pairs whose spans overlap.
#'
#' @return A tibble: `code1_id`, `code1_name`, `code2_id`, `code2_name`,
#'   `n` (co-occurrence count). Ordered by `n` descending.
#' @export
qc_code_cooccurrence <- function(project, unit = c("document", "segment")) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  unit <- match.arg(unit)

  if (unit == "document") {
    sql <- "
      SELECT a.code_id                      AS code1_id,
             c1.name                        AS code1_name,
             b.code_id                      AS code2_id,
             c2.name                        AS code2_name,
             COUNT(DISTINCT a.source_id)    AS n
      FROM   codings a
      JOIN   codings b
             ON  b.source_id = a.source_id
             AND b.code_id   > a.code_id
             AND b.status    = 1
      JOIN   codes c1 ON c1.id = a.code_id AND c1.status = 1
      JOIN   codes c2 ON c2.id = b.code_id AND c2.status = 1
      WHERE  a.status = 1
      GROUP  BY a.code_id, c1.name, b.code_id, c2.name
      ORDER  BY n DESC
    "
  } else {
    sql <- "
      SELECT a.code_id                   AS code1_id,
             c1.name                     AS code1_name,
             b.code_id                   AS code2_id,
             c2.name                     AS code2_name,
             COUNT(*)                    AS n
      FROM   codings a
      JOIN   codings b
             ON  b.source_id = a.source_id
             AND b.code_id   > a.code_id
             AND b.status    = 1
             AND b.selfirst  <= a.selast
             AND b.selast    >= a.selfirst
      JOIN   codes c1 ON c1.id = a.code_id AND c1.status = 1
      JOIN   codes c2 ON c2.id = b.code_id AND c2.status = 1
      WHERE  a.status = 1
      GROUP  BY a.code_id, c1.name, b.code_id, c2.name
      ORDER  BY n DESC
    "
  }
  .query(project$con, sql)
}

#' Cross-tabulate code frequency by a case attribute
#'
#' Returns a wide table: rows = attribute values, columns = code names,
#' cells = number of documents with that code–attribute combination.
#'
#' @param project A `qc_project` object.
#' @param attribute Character. The case attribute variable to cross-tabulate
#'   by (must match a `variable` value in `case_attributes`).
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#'
#' @return A tibble: `attribute_value`, then one column per code.
#' @export
qc_cross_tabulate <- function(project, attribute, code_ids = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!is_string(attribute))
    rlang::abort("`attribute` must be a single string.")

  w_codes <- .in_clause("cod.code_id", code_ids)

  long <- .query(project$con, paste0("
    SELECT ca.value  AS attribute_value,
           c.name    AS code_name,
           COUNT(DISTINCT cod.source_id) AS n_documents
    FROM   codings cod
    JOIN   codes c   ON c.id = cod.code_id AND c.status = 1
    JOIN   case_source_links csl
           ON  csl.source_id = cod.source_id AND csl.status = 1
    JOIN   case_attributes ca
           ON  ca.case_id  = csl.case_id
           AND ca.variable = ?
           AND ca.status   = 1
    WHERE  cod.status = 1 ", w_codes, "
    GROUP  BY ca.value, c.name
    ORDER  BY ca.value, c.name
  "), list(attribute))

  tidyr::pivot_wider(long,
    names_from  = "code_name",
    values_from = "n_documents",
    values_fill = 0L
  )
}

#' Find segments of one code near segments of another code
#'
#' Returns all pairs of coded spans (one per code) within the same document
#' where the gap between them is at most `max_chars` characters.
#'
#' @param project A `qc_project` object.
#' @param code_id1 Integer. First code.
#' @param code_id2 Integer. Second code.
#' @param max_chars Integer. Maximum character gap between spans (0 = overlapping).
#'
#' @return A tibble: `source_name`, `coding1_id`, `c1_start`, `c1_end`,
#'   `c1_text`, `coding2_id`, `c2_start`, `c2_end`, `c2_text`, `gap`.
#' @export
qc_proximity_query <- function(project, code_id1, code_id2, max_chars = 200L) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  code_id1  <- as.integer(code_id1)
  code_id2  <- as.integer(code_id2)
  max_chars <- as.integer(max_chars)

  .query(project$con, "
    SELECT s.name                                            AS source_name,
           a.id                                              AS coding1_id,
           a.selfirst                                        AS c1_start,
           a.selast                                          AS c1_end,
           a.seltext                                         AS c1_text,
           b.id                                              AS coding2_id,
           b.selfirst                                        AS c2_start,
           b.selast                                          AS c2_end,
           b.seltext                                         AS c2_text,
           CASE WHEN a.selast  < b.selfirst THEN b.selfirst - a.selast  - 1
                WHEN b.selast  < a.selfirst THEN a.selfirst - b.selast  - 1
                ELSE 0
           END                                               AS gap
    FROM   codings a
    JOIN   codings b
           ON  b.source_id = a.source_id
           AND b.code_id   = ?
           AND b.status    = 1
           AND b.id        != a.id
    JOIN   sources s ON s.id = a.source_id AND s.status = 1
    WHERE  a.status  = 1
      AND  a.code_id = ?
      AND  CASE WHEN a.selast  < b.selfirst THEN b.selfirst - a.selast  - 1
                WHEN b.selast  < a.selfirst THEN a.selfirst - b.selast  - 1
                ELSE 0
           END <= ?
    ORDER  BY s.name, a.selfirst
  ", list(code_id2, code_id1, max_chars))
}

#' Summarise coded segment counts per code
#'
#' @param project A `qc_project` object.
#' @param ... Passed to [qc_get_coded_segments()] for filtering.
#'
#' @return A tibble: `code_id`, `code_name`, `n_segments`, `n_documents`.
#' @export
qc_code_summary <- function(project, ...) {
  segs <- qc_get_coded_segments(project, ...)
  dplyr::summarise(
    dplyr::group_by(segs, code_id, code_name),
    n_segments  = dplyr::n(),
    n_documents = dplyr::n_distinct(source_id),
    .groups = "drop"
  ) |> dplyr::arrange(dplyr::desc(n_segments))
}

#' Export coded segments to CSV or xlsx
#'
#' @param project A `qc_project` object.
#' @param path Character. Output file path.
#' @param format One of `"csv"`, `"xlsx"`.
#' @param ... Passed to [qc_get_coded_segments()] for filtering.
#'
#' @return `path`, invisibly.
#' @export
qc_export <- function(project, path, format = c("csv", "xlsx"), ...) {
  format <- match.arg(format)
  segs   <- qc_get_coded_segments(project, ...)

  if (format == "xlsx") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      cli::cli_warn(
        "`writexl` not installed; falling back to CSV."
      )
      format <- "csv"
    } else {
      writexl::write_xlsx(segs, path)
      cli::cli_alert_success("Exported {nrow(segs)} rows to {.file {path}}")
      return(invisible(path))
    }
  }

  utils::write.csv(segs, path, row.names = FALSE)
  cli::cli_alert_success("Exported {nrow(segs)} rows to {.file {path}}")
  invisible(path)
}
