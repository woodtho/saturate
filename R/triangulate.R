#' Set the source type of a document
#'
#' Source type categorises documents by data-collection method, e.g.
#' `"interview"`, `"focus_group"`, `"survey"`, `"observation"`. This label
#' is used by [qc_triangulate()] to compare code coverage across methods.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Document id.
#' @param source_type Character. Type label.
#'
#' @return Invisibly, the updated document row.
#' @export
qc_set_source_type <- function(project, id, source_type) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  if (!is_string(source_type)) rlang::abort("`source_type` must be a single string.")
  .exec(project$con,
    "UPDATE sources SET source_type = ? WHERE id = ? AND status = 1",
    list(source_type, as.integer(id))
  )
  invisible(qc_get_document(project, id))
}

#' Triangulate codes across source types
#'
#' Compares how codes (or themes) are distributed across different
#' data-collection methods. A code with strong presence across multiple source
#' types provides better-triangulated evidence than one appearing in only one.
#'
#' Documents must have a `source_type` set via [qc_set_source_type()] or the
#' `source_type` argument in [qc_import_document()]. Documents without a type
#' are grouped under `"unspecified"`.
#'
#' @param project A `qc_project` object.
#' @param code_ids Integer vector or `NULL`. Restrict to specific codes.
#' @param category_ids Integer vector or `NULL`. Restrict to codes in these
#'   categories.
#' @param metric One of `"segments"` (count of coded segments, default) or
#'   `"documents"` (count of distinct documents containing the code).
#'
#' @return A wide tibble: one row per code, one column per source type, values
#'   are segment or document counts. Includes a `total` column. Rows ordered
#'   by `total` descending.
#' @export
qc_triangulate <- function(project, code_ids = NULL, category_ids = NULL,
                            metric = c("segments", "documents")) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  metric <- match.arg(metric)

  w_code <- if (!is.null(code_ids))
    paste0("AND cod.code_id IN (", paste(as.integer(code_ids), collapse = ","), ")")
  else ""

  w_cat <- if (!is.null(category_ids)) {
    ids <- paste(as.integer(category_ids), collapse = ",")
    paste0("AND c.id IN (
       SELECT code_id FROM code_category_links
       WHERE  category_id IN (", ids, ") AND status = 1)")
  } else ""

  agg_expr <- if (metric == "documents")
    "COUNT(DISTINCT cod.source_id)" else "COUNT(cod.id)"

  long <- .query(project$con, paste0("
    SELECT c.name AS code_name,
           COALESCE(NULLIF(s.source_type, ''), 'unspecified') AS source_type,
           ", agg_expr, " AS n
    FROM   codings cod
    JOIN   codes   c ON c.id  = cod.code_id ", w_cat, "
    JOIN   sources s ON s.id  = cod.source_id
    WHERE  cod.status = 1 AND c.status = 1 AND s.status = 1 ", w_code, "
    GROUP  BY c.name, s.source_type
    ORDER  BY c.name, source_type
  "))

  if (nrow(long) == 0L) return(tibble::tibble())

  wide <- tidyr::pivot_wider(long,
    names_from  = "source_type",
    values_from = "n",
    values_fill = 0L
  )

  type_cols  <- setdiff(names(wide), "code_name")
  wide$total <- rowSums(wide[, type_cols, drop = FALSE])
  wide[order(wide$total, decreasing = TRUE), ]
}
