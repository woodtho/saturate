#' Compute the code saturation curve
#'
#' Returns one row per document (in import order, or first-coded order) showing
#' how many new codes were introduced and the running cumulative total. A curve
#' that flattens signals theoretical saturation — new data is no longer
#' producing new codes.
#'
#' @param project A `qc_project` object.
#' @param order_by One of `"import_order"` (document creation date, default) or
#'   `"first_coded"` (earliest coding timestamp in each document).
#' @param code_ids Integer vector or `NULL`. Restrict to a subset of codes.
#'
#' @return A tibble: `doc_index`, `doc_name`, `source_type`, `n_codings`,
#'   `new_codes`, `cumulative_codes`.
#' @export
qc_saturation_curve <- function(project,
                                 order_by = c("import_order", "first_coded"),
                                 code_ids = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  order_by <- match.arg(order_by)

  w_code <- if (!is.null(code_ids))
    paste0("AND cod.code_id IN (", paste(as.integer(code_ids), collapse = ","), ")")
  else ""

  all_codings <- .query(project$con, paste0("
    SELECT cod.code_id,
           cod.source_id,
           s.name  AS doc_name,
           COALESCE(NULLIF(s.source_type, ''), 'unspecified') AS source_type,
           s.created_at  AS source_created,
           cod.created_at AS coded_at
    FROM   codings cod
    JOIN   sources s ON s.id = cod.source_id AND s.status = 1
    WHERE  cod.status = 1 ", w_code, "
  "))

  EMPTY <- tibble::tibble(
    doc_index        = integer(),
    doc_name         = character(),
    source_type      = character(),
    n_codings        = integer(),
    new_codes        = integer(),
    cumulative_codes = integer()
  )
  if (nrow(all_codings) == 0L) return(EMPTY)

  # Determine document ordering
  if (order_by == "first_coded") {
    doc_order <- tapply(all_codings$coded_at, all_codings$source_id, min)
  } else {
    doc_order <- tapply(all_codings$source_created, all_codings$source_id, min)
  }

  # Unique docs in order
  doc_ids  <- as.integer(names(sort(doc_order)))
  doc_meta <- unique(all_codings[, c("source_id", "doc_name", "source_type")])

  seen_codes <- integer(0)
  rows <- lapply(seq_along(doc_ids), function(i) {
    sid        <- doc_ids[[i]]
    mask       <- all_codings$source_id == sid
    codes_here <- unique(as.integer(all_codings$code_id[mask]))
    new_here   <- setdiff(codes_here, seen_codes)
    seen_codes <<- union(seen_codes, codes_here)
    meta       <- doc_meta[doc_meta$source_id == sid, ][1L, ]
    tibble::tibble(
      doc_index        = i,
      doc_name         = meta$doc_name,
      source_type      = meta$source_type,
      n_codings        = sum(mask),
      new_codes        = length(new_here),
      cumulative_codes = length(seen_codes)
    )
  })

  do.call(rbind, rows)
}

#' Plot the code saturation curve
#'
#' Requires `ggplot2`.
#'
#' @param project A `qc_project` object.
#' @param dark Logical. Apply dark-mode plot colours.
#' @param ... Additional arguments passed to [qc_saturation_curve()].
#'
#' @return A `ggplot` object.
#' @export
qc_plot_saturation <- function(project, ..., dark = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    rlang::abort("Install `ggplot2` to use `qc_plot_saturation()`.")
  df <- qc_saturation_curve(project, ...)
  if (nrow(df) == 0L) rlang::abort("No coded documents to plot.")

  line_colour   <- if (isTRUE(dark)) "#8cc4ee" else "#4E79A7"
  smooth_colour <- if (isTRUE(dark)) "#fbbf24" else "#F28E2B"

  ggplot2::ggplot(df,
    ggplot2::aes(x = doc_index, y = cumulative_codes)) +
    ggplot2::geom_line(colour = line_colour, linewidth = 1) +
    ggplot2::geom_point(
      ggplot2::aes(size = new_codes, colour = source_type),
      alpha = 0.85) +
    ggplot2::geom_smooth(
      method  = "loess", formula = y ~ x,
      se      = FALSE,
      colour  = smooth_colour,
      linetype = "dashed",
      linewidth = 0.8) +
    ggplot2::scale_size_continuous(name = "New codes", range = c(2, 8)) +
    ggplot2::scale_colour_brewer(name = "Source type", palette = "Set2") +
    ggplot2::labs(
      title    = "Code Saturation Curve",
      subtitle = "Cumulative distinct codes per document (flattening = saturation)",
      x        = "Document (in order)",
      y        = "Cumulative distinct codes"
    ) +
    .qc_plot_theme(dark, base_size = 13) +
    ggplot2::theme(legend.position = "right")
}
