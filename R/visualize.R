# NOTE: globalVariables suppresses R CMD CHECK NOTEs for bare names used in
# ggplot2 aes() calls within this file.
utils::globalVariables(c(
  "code_name", "code_color", "plot_value",
  "code_x", "code_y", "n",
  "coder1", "coder2", "mean_kappa",
  "weight", "color", "n_codings", "name",
  "period", "n_codings"
))

# ---- qc_plot_codes -----------------------------------------------------------

#' Horizontal bar chart of code frequency
#'
#' Visualises how often codes were applied, measured either by total coding
#' count or by the number of distinct documents that carry each code.
#'
#' @param project A `qc_project` object.
#' @param top_n Integer. Maximum number of codes to show.
#' @param by One of `"codings"` (default) or `"documents"`.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#' @param ... Unused. Reserved for future arguments.
#'
#' @return A ggplot2 object.
#' @export
qc_plot_codes <- function(project, top_n = 20L,
                           by       = c("codings", "documents"),
                           code_ids = NULL,
                           ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    rlang::abort(
      "Package 'ggplot2' is required. Install with: install.packages('ggplot2')"
    )
  assert_class(project, "qc_project")
  assert_con(project$con)
  by    <- match.arg(by)
  top_n <- as.integer(top_n)

  segs <- qc_get_coded_segments(project, code_ids = code_ids)

  if (nrow(segs) == 0L) {
    cli::cli_warn("No coded segments found — returning NULL.")
    return(NULL)
  }

  df <- dplyr::summarise(
    dplyr::group_by(segs, .data$code_id, .data$code_name, .data$code_color),
    n_codings   = dplyr::n(),
    n_documents = dplyr::n_distinct(.data$source_id),
    .groups     = "drop"
  )

  sort_col <- if (by == "codings") "n_codings" else "n_documents"
  df <- head(df[order(-df[[sort_col]]), ], top_n)

  df$plot_value <- df[[sort_col]]
  df$code_name  <- factor(df$code_name,
                           levels = rev(df$code_name[order(df$plot_value)]))

  y_label <- if (by == "codings") "Number of codings" else "Number of documents"

  ggplot2::ggplot(df, ggplot2::aes(x = plot_value, y = code_name,
                                    fill = code_name)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = setNames(df$code_color, as.character(df$code_name))
    ) +
    ggplot2::labs(x = y_label, y = NULL, title = "Code Frequency") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}


# ---- qc_plot_cooccurrence ----------------------------------------------------

#' Symmetric tile heatmap of code co-occurrence
#'
#' Displays how frequently pairs of codes appear together within the same unit
#' (document or overlapping segment). The heatmap is symmetric — each pair
#' appears once in each triangle.
#'
#' @param project A `qc_project` object.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#' @param unit One of `"document"` (default) or `"segment"`.
#' @param ... Unused. Reserved for future arguments.
#'
#' @return A ggplot2 object, or NULL (with a warning) when no co-occurrences
#'   are found.
#' @export
qc_plot_cooccurrence <- function(project,
                                  code_ids = NULL,
                                  unit     = c("document", "segment"),
                                  ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    rlang::abort(
      "Package 'ggplot2' is required. Install with: install.packages('ggplot2')"
    )
  assert_class(project, "qc_project")
  assert_con(project$con)
  unit <- match.arg(unit)

  co <- qc_code_cooccurrence(project, unit = unit)

  if (!is.null(code_ids)) {
    ids <- as.integer(code_ids)
    co  <- co[co$code1_id %in% ids | co$code2_id %in% ids, ]
  }

  if (nrow(co) == 0L) {
    cli::cli_warn("No co-occurrences found — returning NULL.")
    return(NULL)
  }

  # Make symmetric by mirroring
  swapped <- data.frame(
    code_x = co$code2_name,
    code_y = co$code1_name,
    n      = co$n,
    stringsAsFactors = FALSE
  )
  orig <- data.frame(
    code_x = co$code1_name,
    code_y = co$code2_name,
    n      = co$n,
    stringsAsFactors = FALSE
  )
  df <- rbind(orig, swapped)

  lvls       <- sort(unique(c(df$code_x, df$code_y)))
  df$code_x  <- factor(df$code_x, levels = lvls)
  df$code_y  <- factor(df$code_y, levels = lvls)

  ggplot2::ggplot(df, ggplot2::aes(x = code_x, y = code_y, fill = n)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = n), colour = "grey30", size = 3) +
    ggplot2::scale_fill_gradient(
      low  = "#e8f4f8",
      high = "#1565c0",
      name = "Co-occurrences"
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Code Co-occurrence") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


# ---- qc_plot_overlap ---------------------------------------------------------

#' Inter-coder agreement heatmap (mean Cohen's kappa)
#'
#' Displays mean Cohen's kappa across all code–document combinations for each
#' pair of coders. The diagonal (self-agreement) is fixed at 1.0.
#'
#' @param project A `qc_project` object.
#' @param code_ids Integer vector or `NULL`. Restrict the agreement calculation
#'   to these codes.
#' @param ... Unused. Reserved for future arguments.
#'
#' @return A ggplot2 object, or NULL (with a warning) when fewer than two
#'   coders are found.
#' @export
qc_plot_overlap <- function(project, code_ids = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    rlang::abort(
      "Package 'ggplot2' is required. Install with: install.packages('ggplot2')"
    )
  assert_class(project, "qc_project")
  assert_con(project$con)

  mat <- qc_agreement_matrix(project, code_ids = code_ids)

  if (nrow(mat) == 0L) {
    cli::cli_warn("Agreement matrix is empty — returning NULL.")
    return(NULL)
  }

  pair_df <- dplyr::summarise(
    dplyr::group_by(mat, .data$coder1, .data$coder2),
    mean_kappa = mean(.data$kappa, na.rm = TRUE),
    .groups    = "drop"
  )

  # Mirror + diagonal
  mirrored <- data.frame(
    coder1     = pair_df$coder2,
    coder2     = pair_df$coder1,
    mean_kappa = pair_df$mean_kappa,
    stringsAsFactors = FALSE
  )
  all_coders <- unique(c(pair_df$coder1, pair_df$coder2))
  diag_df <- data.frame(
    coder1     = all_coders,
    coder2     = all_coders,
    mean_kappa = 1.0,
    stringsAsFactors = FALSE
  )
  df <- rbind(pair_df, mirrored, diag_df)

  ggplot2::ggplot(df, ggplot2::aes(x = coder1, y = coder2,
                                    fill = mean_kappa)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = round(mean_kappa, 2)), size = 3.5) +
    ggplot2::scale_fill_gradient2(
      low      = "#c62828",
      mid      = "#fff9c4",
      high     = "#2e7d32",
      midpoint = 0.67,
      limits   = c(-1, 1),
      name     = "Kappa"
    ) +
    ggplot2::labs(
      x     = NULL,
      y     = NULL,
      title = "Inter-coder Agreement (Mean Kappa)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


# ---- qc_plot_network ---------------------------------------------------------

#' Network diagram of code relationships
#'
#' Renders a force-directed network graph of code co-occurrences or explicit
#' code relations. Requires the `ggraph` package; if absent, returns the igraph
#' object invisibly with an informative message.
#'
#' @param project A `qc_project` object.
#' @param type One of `"cooccurrence"` (default) or `"relations"`.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#' @param source_ids Integer vector or `NULL`. Restrict to these documents
#'   (applies to `"cooccurrence"` only).
#' @param min_shared Integer. Minimum co-occurrence count to draw an edge.
#' @param layout Character. igraph/ggraph layout algorithm (e.g. `"fr"`,
#'   `"kk"`, `"stress"`).
#' @param ... Unused. Reserved for future arguments.
#'
#' @return A ggplot2/ggraph object, or the igraph object invisibly when ggraph
#'   is not installed.
#' @export
qc_plot_network <- function(project,
                             type       = c("cooccurrence", "relations"),
                             code_ids   = NULL,
                             source_ids = NULL,
                             min_shared = 2L,
                             layout     = "fr",
                             ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    rlang::abort(
      "Package 'ggplot2' is required. Install with: install.packages('ggplot2')"
    )
  assert_class(project, "qc_project")
  assert_con(project$con)
  type <- match.arg(type)

  g <- qc_as_igraph(
    project,
    type       = type,
    code_ids   = code_ids,
    source_ids = source_ids,
    min_shared = min_shared
  )

  if (!requireNamespace("ggraph", quietly = TRUE)) {
    cli::cli_inform(
      "Install ggraph for a ggplot2-based network: {.run install.packages('ggraph')}"
    )
    return(invisible(g))
  }

  ggraph::ggraph(g, layout = layout) +
    ggraph::geom_edge_link(
      ggplot2::aes(width = weight, alpha = weight),
      show.legend = FALSE,
      colour      = "grey70"
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(colour = color, size = n_codings)
    ) +
    ggraph::geom_node_label(
      ggplot2::aes(label = name),
      repel   = TRUE,
      size    = 3.2,
      nudge_y = 0.2
    ) +
    ggplot2::scale_colour_identity(guide = "none") +
    ggplot2::scale_size(range = c(3, 10), guide = "none") +
    ggplot2::labs(
      title = paste0("Code ", tools::toTitleCase(type), " Network")
    ) +
    ggraph::theme_graph(base_size = 12)
}


# ---- qc_plot_timeline --------------------------------------------------------

#' Line chart of code usage over time
#'
#' Shows how coding activity for the top codes changes across calendar periods,
#' using a document-level date attribute to anchor each coding on the time axis.
#'
#' @param project A `qc_project` object.
#' @param date_attr Character. The `source_attributes.variable` storing dates.
#' @param period One of `"month"` (default), `"year"`, `"week"`, or `"day"`.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#' @param top_n Integer. Number of most-used codes to plot.
#' @param ... Unused. Reserved for future arguments.
#'
#' @return A ggplot2 object, or NULL (with a warning) when no temporal data is
#'   found.
#' @export
qc_plot_timeline <- function(project,
                              date_attr = "doc_date",
                              period    = c("month", "year", "week", "day"),
                              code_ids  = NULL,
                              top_n     = 8L,
                              ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    rlang::abort(
      "Package 'ggplot2' is required. Install with: install.packages('ggplot2')"
    )
  assert_class(project, "qc_project")
  assert_con(project$con)
  period <- match.arg(period)
  top_n  <- as.integer(top_n)

  df <- qc_temporal_analysis(
    project,
    date_attr  = date_attr,
    period     = period,
    code_ids   = code_ids
  )

  if (nrow(df) == 0L) {
    cli::cli_warn("No temporal data found — returning NULL.")
    return(NULL)
  }

  totals <- dplyr::arrange(
    dplyr::summarise(
      dplyr::group_by(df, .data$code_name),
      total   = sum(.data$n_codings),
      .groups = "drop"
    ),
    dplyr::desc(.data$total)
  )
  top_codes <- totals$code_name[seq_len(min(top_n, nrow(totals)))]
  df <- df[df$code_name %in% top_codes, ]

  period_label <- switch(period,
    year  = "Year",
    month = "Month",
    week  = "ISO Week",
    day   = "Date"
  )

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = period, y = n_codings,
                 colour = code_name, group = code_name)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      x      = period_label,
      y      = "Number of codings",
      colour = "Code",
      title  = "Code Usage Over Time"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
