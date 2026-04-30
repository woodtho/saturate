# NOTE: globalVariables suppresses R CMD CHECK NOTEs for ggplot2/dplyr bare
# names used in aes() and across() calls within this file.
utils::globalVariables(c(
  "row_id", "row_name", "code_name", "val",
  "code_id", "n_codings", "n_documents", "source_id",
  "period_lbl", "code_x", "code_y", "n",
  "weight", "color", "name", "total"
))

# ---- qc_code_matrix ----------------------------------------------------------

#' Code-by-entity matrix
#'
#' Returns a wide tibble of codes (columns) crossed with entities (rows).
#' Entities can be documents, cases linked via `case_source_links`, or
#' distinct values of a named source attribute.  Cell values are coding
#' counts, binary presence/absence flags, or total characters coded.
#'
#' @param project A `qc_project` object.
#' @param by One of `"document"` (default), `"case"`, or `"attribute"`.
#' @param attribute Character. Required when `by = "attribute"`. Name of the
#'   `source_attributes.variable` to use as the row dimension.
#' @param code_ids Integer vector or `NULL`. Restrict columns to these codes.
#' @param source_ids Integer vector or `NULL`. Restrict to these documents.
#' @param values One of `"count"` (default), `"binary"`, or `"chars"`.
#' @param coder Character or `NULL`. Restrict to codings by this coder.
#'
#' @return A tibble: identifier columns then one column per code, filled with 0
#'   where the code was not applied.
#' @export
qc_code_matrix <- function(project,
                            by        = c("document", "case", "attribute"),
                            attribute = NULL,
                            code_ids  = NULL,
                            source_ids = NULL,
                            values    = c("count", "binary", "chars"),
                            coder     = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  by     <- match.arg(by)
  values <- match.arg(values)

  if (by == "attribute" && !is_string(attribute))
    rlang::abort('`attribute` must be a single string when `by = "attribute"`.')

  w_codes   <- .in_clause("cod.code_id",   code_ids)
  w_sources <- .in_clause("cod.source_id", source_ids)
  w_coder   <- if (!is.null(coder) && is_string(coder))
    paste0("AND cod.coder = '", gsub("'", "''", coder), "'")
  else ""

  val_sql <- if (values == "chars")
    "SUM(cod.selast - cod.selfirst + 1)"
  else
    "COUNT(*)"

  long <- if (by == "document") {
    .query(project$con, paste0(
      "SELECT s.id AS row_id, s.name AS row_name,
              c.name AS code_name, ", val_sql, " AS val
       FROM   codings cod
       JOIN   sources s ON s.id = cod.source_id AND s.status = 1
       JOIN   codes   c ON c.id = cod.code_id   AND c.status = 1
       WHERE  cod.status = 1
       ", w_codes, " ", w_sources, " ", w_coder, "
       GROUP  BY s.id, s.name, c.name"
    ))
  } else if (by == "case") {
    .query(project$con, paste0(
      "SELECT cas.id AS row_id, cas.name AS row_name,
              c.name AS code_name, ", val_sql, " AS val
       FROM   codings cod
       JOIN   sources s   ON s.id  = cod.source_id AND s.status = 1
       JOIN   codes   c   ON c.id  = cod.code_id   AND c.status = 1
       JOIN   case_source_links csl
              ON  csl.source_id = cod.source_id AND csl.status = 1
       JOIN   cases cas ON cas.id = csl.case_id    AND cas.status = 1
       WHERE  cod.status = 1
       ", w_codes, " ", w_sources, " ", w_coder, "
       GROUP  BY cas.id, cas.name, c.name"
    ))
  } else {
    .query(project$con, paste0(
      "SELECT sa.value AS row_name,
              c.name   AS code_name, ", val_sql, " AS val
       FROM   codings cod
       JOIN   sources s  ON s.id = cod.source_id AND s.status = 1
       JOIN   codes   c  ON c.id = cod.code_id   AND c.status = 1
       JOIN   source_attributes sa
              ON  sa.source_id = cod.source_id
              AND sa.variable  = ?
              AND sa.status    = 1
       WHERE  cod.status = 1
       ", w_codes, " ", w_sources, " ", w_coder, "
       GROUP  BY sa.value, c.name"
    ), list(attribute))
  }

  if (nrow(long) == 0L) {
    cli::cli_warn("No codings found \u2014 returning empty tibble.")
    return(tibble::tibble())
  }

  if (by == "attribute") {
    wide <- tidyr::pivot_wider(long,
      id_cols     = "row_name",
      names_from  = "code_name",
      values_from = "val",
      values_fill = 0L
    )
    names(wide)[names(wide) == "row_name"] <- paste0(attribute, "_value")
  } else {
    wide <- tidyr::pivot_wider(long,
      id_cols     = c("row_id", "row_name"),
      names_from  = "code_name",
      values_from = "val",
      values_fill = 0L
    )
    id_col   <- if (by == "document") "document_id"   else "case_id"
    name_col <- if (by == "document") "document_name" else "case_name"
    names(wide)[names(wide) == "row_id"]   <- id_col
    names(wide)[names(wide) == "row_name"] <- name_col
  }

  if (values == "binary") {
    id_cols <- if (by == "attribute") 1L else 2L
    code_cols <- seq(id_cols + 1L, ncol(wide))
    wide[code_cols] <- lapply(wide[code_cols], function(x) pmin(x, 1L))
  }

  wide
}


# ---- qc_code_metrics ---------------------------------------------------------

#' Per-code prevalence, density, and Gries' DP dispersion
#'
#' For each code, computes how broadly it is used (prevalence across
#' documents), how densely it covers the corpus (density as a percentage of
#' total characters), and how evenly it is spread (Gries' DP dispersion, where
#' 0 = perfectly uniform, 1 = concentrated in a single document).
#'
#' @param project A `qc_project` object.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#' @param source_ids Integer vector or `NULL`. Restrict to these documents.
#'
#' @return A tibble: `code_id`, `code_name`, `n_codings`, `n_documents`,
#'   `total_documents`, `prevalence`, `mean_chars`, `total_chars_coded`,
#'   `density`, `dispersion`. Ordered by `n_codings` descending.
#' @export
qc_code_metrics <- function(project, code_ids = NULL, source_ids = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  w_docs  <- .in_clause("id",          source_ids)
  w_codes <- .in_clause("cod.code_id", code_ids)
  w_src   <- .in_clause("cod.source_id", source_ids)

  doc_info <- .query(project$con, paste0(
    "SELECT id, LENGTH(content) AS char_count
     FROM   sources
     WHERE  status = 1 ", w_docs
  ))

  codings <- .query(project$con, paste0(
    "SELECT cod.code_id,
            c.name                              AS code_name,
            cod.source_id,
            (cod.selast - cod.selfirst + 1)     AS span_chars
     FROM   codings cod
     JOIN   codes c ON c.id = cod.code_id AND c.status = 1
     WHERE  cod.status = 1 ", w_codes, " ", w_src
  ))

  EMPTY_COLS <- c("code_id", "code_name", "n_codings", "n_documents",
                  "total_documents", "prevalence", "mean_chars",
                  "total_chars_coded", "density", "dispersion")

  if (nrow(doc_info) == 0L || nrow(codings) == 0L) {
    cli::cli_warn("No codings or documents found \u2014 returning empty tibble.")
    empty <- tibble::tibble(
      code_id          = integer(),
      code_name        = character(),
      n_codings        = integer(),
      n_documents      = integer(),
      total_documents  = integer(),
      prevalence       = numeric(),
      mean_chars       = numeric(),
      total_chars_coded = integer(),
      density          = numeric(),
      dispersion       = numeric()
    )
    return(empty)
  }

  total_docs  <- nrow(doc_info)
  total_chars <- sum(doc_info$char_count, na.rm = TRUE)

  unique_codes <- unique(codings[, c("code_id", "code_name")])

  rows <- lapply(seq_len(nrow(unique_codes)), function(i) {
    cid  <- unique_codes$code_id[[i]]
    cnm  <- unique_codes$code_name[[i]]
    sub  <- codings[codings$code_id == cid, ]

    n_codings         <- nrow(sub)
    n_documents       <- length(unique(sub$source_id))
    prevalence        <- round(n_documents / total_docs * 100, 1)
    mean_chars        <- round(mean(sub$span_chars), 0)
    total_chars_coded <- sum(sub$span_chars)
    density           <- if (total_chars > 0)
      round(total_chars_coded / total_chars * 100, 4)
    else NA_real_

    # Gries' DP
    v_i <- tabulate(
      match(sub$source_id, doc_info$id),
      nbins = nrow(doc_info)
    ) / n_codings
    s_i         <- doc_info$char_count / total_chars
    dispersion  <- round(0.5 * sum(abs(v_i - s_i)), 4)

    tibble::tibble(
      code_id           = cid,
      code_name         = cnm,
      n_codings         = n_codings,
      n_documents       = n_documents,
      total_documents   = total_docs,
      prevalence        = prevalence,
      mean_chars        = mean_chars,
      total_chars_coded = total_chars_coded,
      density           = density,
      dispersion        = dispersion
    )
  })

  result <- do.call(rbind, rows)
  result[order(-result$n_codings), ]
}


# ---- qc_temporal_analysis ----------------------------------------------------

#' Time-series of code usage driven by a document-level date attribute
#'
#' Groups codings by a calendar period (year, month, ISO week, or day) using a
#' named `source_attributes` variable as the date axis.
#'
#' @param project A `qc_project` object.
#' @param date_attr Character. The `source_attributes.variable` that stores
#'   ISO-8601 date strings for each document.
#' @param period One of `"year"`, `"month"` (default), `"week"`, or `"day"`.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#' @param source_ids Integer vector or `NULL`. Restrict to these documents.
#' @param coder Character or `NULL`. Restrict to codings by this coder.
#'
#' @return A tibble: `period`, `code_id`, `code_name`, `n_codings`,
#'   `n_documents`. Sorted by period then code name.
#' @export
qc_temporal_analysis <- function(project,
                                  date_attr  = "doc_date",
                                  period     = c("year", "month", "week", "day"),
                                  code_ids   = NULL,
                                  source_ids = NULL,
                                  coder      = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  period <- match.arg(period)

  if (!is_string(date_attr))
    rlang::abort("`date_attr` must be a single string.")

  w_codes   <- .in_clause("cod.code_id",   code_ids)
  w_sources <- .in_clause("cod.source_id", source_ids)
  w_coder   <- if (!is.null(coder) && is_string(coder))
    paste0("AND cod.coder = '", gsub("'", "''", coder), "'")
  else ""

  EMPTY <- tibble::tibble(
    period      = character(),
    code_id     = integer(),
    code_name   = character(),
    n_codings   = integer(),
    n_documents = integer()
  )

  raw <- .query(project$con, paste0(
    "SELECT cod.code_id,
            c.name  AS code_name,
            cod.source_id,
            sa.value AS raw_date
     FROM   codings cod
     JOIN   codes c ON c.id = cod.code_id AND c.status = 1
     JOIN   source_attributes sa
            ON  sa.source_id = cod.source_id
            AND sa.variable  = ?
            AND sa.status    = 1
     WHERE  cod.status = 1 ", w_codes, " ", w_sources, " ", w_coder
  ), list(date_attr))

  if (nrow(raw) == 0L) {
    cli::cli_warn("No codings with date attribute {.val {date_attr}} found.")
    return(EMPTY)
  }

  dates <- suppressWarnings(as.Date(raw$raw_date))
  if (all(is.na(dates)))
    rlang::abort(paste0(
      "All values of '", date_attr,
      "' failed to parse as dates. Expected ISO-8601 format (YYYY-MM-DD)."
    ))

  raw <- raw[!is.na(dates), ]
  dates <- dates[!is.na(dates)]

  fmt <- switch(period,
    year  = "%Y",
    month = "%Y-%m",
    week  = "%G-W%V",
    day   = "%Y-%m-%d"
  )
  raw$period_lbl <- format(dates, fmt)

  result <- dplyr::summarise(
    dplyr::group_by(raw, .data$period_lbl, .data$code_id, .data$code_name),
    n_codings   = dplyr::n(),
    n_documents = dplyr::n_distinct(.data$source_id),
    .groups     = "drop"
  )
  names(result)[names(result) == "period_lbl"] <- "period"

  result[order(result$period, result$code_name), ]
}


# ---- qc_get_memos ------------------------------------------------------------

#' Retrieve memos across entity types
#'
#' Returns a unified tibble of non-empty memos from codings, documents, codes,
#' and/or cases. Each row carries a `memo_type` flag so results can be
#' filtered or grouped after the fact.
#'
#' @param project A `qc_project` object.
#' @param types Character vector. Subset of `c("coding","document","code","case")`.
#' @param code_ids Integer vector or `NULL`. Filter coding/code memos to these
#'   codes.
#' @param source_ids Integer vector or `NULL`. Filter coding/document memos to
#'   these documents.
#'
#' @return A tibble: `memo_type`, `entity_id`, `source_name`, `code_name`,
#'   `coder`, `memo`, `created_at`.
#' @export
qc_get_memos <- function(project,
                          types      = c("coding", "document", "code", "case"),
                          code_ids   = NULL,
                          source_ids = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  types <- match.arg(types, several.ok = TRUE)

  w_codes   <- .in_clause("cod.code_id",   code_ids)
  w_sources <- .in_clause("cod.source_id", source_ids)
  w_src_doc <- .in_clause("s.id",          source_ids)
  w_code_c  <- .in_clause("c.id",          code_ids)

  EMPTY <- tibble::tibble(
    memo_type  = character(),
    entity_id  = integer(),
    source_name = character(),
    code_name  = character(),
    coder      = character(),
    memo       = character(),
    created_at = as.POSIXct(character())
  )

  parts <- list()

  if ("coding" %in% types) {
    parts$coding <- .query(project$con, paste0(
      "SELECT 'coding'                  AS memo_type,
              cod.id                    AS entity_id,
              s.name                    AS source_name,
              c.name                    AS code_name,
              cod.coder                 AS coder,
              cod.memo                  AS memo,
              cod.created_at            AS created_at
       FROM   codings cod
       JOIN   sources s ON s.id = cod.source_id AND s.status = 1
       JOIN   codes   c ON c.id = cod.code_id   AND c.status = 1
       WHERE  cod.status = 1
         AND  cod.memo != ''
       ", w_codes, " ", w_sources
    ))
  }

  if ("document" %in% types) {
    parts$document <- .query(project$con, paste0(
      "SELECT 'document'                     AS memo_type,
              s.id                            AS entity_id,
              s.name                          AS source_name,
              CAST(NULL AS VARCHAR)           AS code_name,
              CAST(NULL AS VARCHAR)           AS coder,
              s.memo                          AS memo,
              s.created_at                    AS created_at
       FROM   sources s
       WHERE  s.status = 1
         AND  s.memo != ''
       ", w_src_doc
    ))
  }

  if ("code" %in% types) {
    parts$code <- .query(project$con, paste0(
      "SELECT 'code'                         AS memo_type,
              c.id                            AS entity_id,
              CAST(NULL AS VARCHAR)           AS source_name,
              c.name                          AS code_name,
              CAST(NULL AS VARCHAR)           AS coder,
              c.memo                          AS memo,
              c.created_at                    AS created_at
       FROM   codes c
       WHERE  c.status = 1
         AND  c.memo != ''
       ", w_code_c
    ))
  }

  if ("case" %in% types) {
    parts$case <- .query(project$con,
      "SELECT 'case'                         AS memo_type,
              cas.id                          AS entity_id,
              CAST(NULL AS VARCHAR)           AS source_name,
              CAST(NULL AS VARCHAR)           AS code_name,
              CAST(NULL AS VARCHAR)           AS coder,
              cas.memo                        AS memo,
              cas.created_at                  AS created_at
       FROM   cases cas
       WHERE  cas.status = 1
         AND  cas.memo != ''"
    )
  }

  non_empty <- Filter(function(x) nrow(x) > 0L, parts)
  if (length(non_empty) == 0L) return(EMPTY)
  do.call(rbind, non_empty)
}


# ---- qc_summary_report -------------------------------------------------------

#' Project summary report
#'
#' Compiles key project statistics into a named list of tibbles suitable for
#' rendering with `knitr::kable()` or `flextable`. The return value has S3
#' class `"qc_report"` with a `print` method that produces a formatted
#' console summary.
#'
#' @param project A `qc_project` object.
#' @param include_metrics Logical. When `TRUE` (default), calls
#'   [qc_code_metrics()] and includes prevalence, density, and dispersion.
#' @param top_n Integer. Number of top codes / co-occurrences to include.
#'
#' @return An S3 object of class `"qc_report"`: a named list with elements
#'   `project`, `corpus`, `codes`, `cooccurrence`, `coders`, and `metrics`.
#' @export
qc_summary_report <- function(project, include_metrics = TRUE, top_n = 15L) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  top_n <- as.integer(top_n)

  proj_info <- tryCatch(qc_project_info(project), error = function(e) NULL)

  corpus <- tryCatch({
    n_docs    <- .query(project$con,
      "SELECT COUNT(*) AS n FROM sources WHERE status = 1")$n
    n_codes   <- .query(project$con,
      "SELECT COUNT(*) AS n FROM codes WHERE status = 1")$n
    n_codings <- .query(project$con,
      "SELECT COUNT(*) AS n FROM codings WHERE status = 1")$n
    n_coders  <- .query(project$con,
      "SELECT COUNT(DISTINCT coder) AS n FROM codings WHERE status = 1")$n
    tibble::tibble(
      metric = c("Documents", "Active codes", "Total codings", "Total coders"),
      value  = as.character(c(n_docs, n_codes, n_codings, n_coders))
    )
  }, error = function(e) NULL)

  code_list <- tryCatch(qc_list_codes(project), error = function(e) NULL)

  codes_tbl <- tryCatch({
    if (include_metrics) {
      m <- qc_code_metrics(project)
      if (!is.null(code_list) && nrow(m) > 0L) {
        defs <- code_list[, c("name", "definition")]
        m    <- merge(m, defs, by.x = "code_name", by.y = "name", all.x = TRUE)
      }
      head(m[order(-m$n_codings), ], top_n)
    } else {
      if (is.null(code_list)) NULL
      else head(
        code_list[order(-code_list$n_codings),
                  c("name", "n_codings")],
        top_n
      )
    }
  }, error = function(e) NULL)

  cooccurrence <- tryCatch({
    co <- qc_code_cooccurrence(project)
    if (nrow(co) == 0L) NULL else head(co, top_n)
  }, error = function(e) NULL)

  coders <- tryCatch({
    cd <- qc_list_coders(project)
    if (nrow(cd) == 0L) NULL else cd
  }, error = function(e) NULL)

  metrics <- if (include_metrics)
    tryCatch(qc_code_metrics(project), error = function(e) NULL)
  else NULL

  structure(
    list(
      project     = proj_info,
      corpus      = corpus,
      codes       = codes_tbl,
      cooccurrence = cooccurrence,
      coders      = coders,
      metrics     = metrics
    ),
    class = "qc_report"
  )
}

#' @export
print.qc_report <- function(x, ...) {
  cli::cli_h1("saturate Project Report")

  if (!is.null(x$project)) {
    cli::cli_h2("Project")
    print(x$project)
  }

  if (!is.null(x$corpus)) {
    cli::cli_h2("Corpus")
    print(x$corpus)
  }

  if (!is.null(x$codes)) {
    cli::cli_h2("Top Codes")
    print(x$codes)
  }

  if (!is.null(x$cooccurrence)) {
    cli::cli_h2("Top Co-occurrences")
    print(x$cooccurrence)
  }

  if (!is.null(x$coders)) {
    cli::cli_h2("Coders")
    print(x$coders)
  }

  invisible(x)
}


# ---- qc_as_igraph ------------------------------------------------------------

#' Convert project network data to an igraph object
#'
#' Builds an igraph object from code co-occurrence counts or from explicit
#' code relations stored in `code_relations`.
#'
#' @param project A `qc_project` object.
#' @param type One of `"cooccurrence"` (default) or `"relations"`.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#' @param source_ids Integer vector or `NULL`. Restrict to these documents
#'   (applies to `"cooccurrence"` only).
#' @param min_shared Integer. Minimum co-occurrence count to include an edge
#'   (applies to `"cooccurrence"` only).
#'
#' @return An igraph object. For `"cooccurrence"`, edges carry a `weight`
#'   attribute and vertices carry `color` and `n_codings`. For `"relations"`,
#'   edges carry a `type` attribute (directed graph).
#' @export
qc_as_igraph <- function(project,
                          type       = c("cooccurrence", "relations"),
                          code_ids   = NULL,
                          source_ids = NULL,
                          min_shared = 1L) {
  assert_class(project, "qc_project")
  assert_con(project$con)

  if (!requireNamespace("igraph", quietly = TRUE))
    rlang::abort(
      "Package 'igraph' is required. Install with: install.packages('igraph')"
    )

  type <- match.arg(type)

  if (type == "cooccurrence") {
    co <- qc_code_cooccurrence(project)
    if (!is.null(code_ids)) {
      ids <- as.integer(code_ids)
      co  <- co[co$code1_id %in% ids | co$code2_id %in% ids, ]
    }
    co <- co[co$n >= as.integer(min_shared), ]

    if (nrow(co) == 0L)
      rlang::abort("No co-occurrence edges meet the min_shared threshold.")

    code_meta <- qc_list_codes(project)
    all_names <- unique(c(co$code1_name, co$code2_name))
    vertices_df <- merge(
      data.frame(name = all_names, stringsAsFactors = FALSE),
      code_meta[, c("name", "color", "n_codings")],
      by   = "name",
      all.x = TRUE
    )
    vertices_df$color[is.na(vertices_df$color)] <- "#4E79A7"
    vertices_df$n_codings[is.na(vertices_df$n_codings)] <- 0L

    edges_df <- data.frame(
      from   = co$code1_name,
      to     = co$code2_name,
      weight = co$n,
      stringsAsFactors = FALSE
    )

    igraph::graph_from_data_frame(
      d        = edges_df,
      directed = FALSE,
      vertices = vertices_df
    )

  } else {
    rels <- qc_list_code_relations(project)
    if (nrow(rels) == 0L)
      rlang::abort("No code relations found in the project.")

    edges_df <- data.frame(
      from = rels$name_1,
      to   = rels$name_2,
      type = rels$relation_type,
      stringsAsFactors = FALSE
    )

    igraph::graph_from_data_frame(edges_df, directed = TRUE)
  }
}
