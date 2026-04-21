#' Build graph data for the document–code network
#'
#' Returns node and edge tibbles suitable for `visNetwork` or `igraph`.
#' Three graph types are supported:
#'
#' * `"similarity"` — documents are nodes; edges connect documents that share
#'   at least `min_shared` codes, weighted by the number of shared codes.
#' * `"bipartite"` — documents *and* codes are nodes; edges represent
#'   individual coding relationships.
#' * `"cooccurrence"` — codes are nodes; edges connect codes that co-occur in
#'   at least one document, weighted by the document count.
#'
#' @param project A `qc_project` object.
#' @param type One of `"similarity"` (default), `"bipartite"`,
#'   `"cooccurrence"`.
#' @param code_ids Integer vector or `NULL`. Restrict to these codes.
#' @param source_ids Integer vector or `NULL`. Restrict to these documents.
#' @param min_shared Integer. For `"similarity"`: minimum shared codes for an
#'   edge. For `"cooccurrence"`: minimum co-document count.
#'
#' @return A named list: `nodes` and `edges`, each a tibble formatted for
#'   `visNetwork::visNetwork()`.
#' @export
qc_document_graph <- function(project,
                               type       = c("similarity", "bipartite",
                                              "cooccurrence"),
                               code_ids   = NULL,
                               source_ids = NULL,
                               min_shared = 1L) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  type       <- match.arg(type)
  min_shared <- as.integer(min_shared)

  w_codes   <- .in_clause("cod.code_id",   code_ids)
  w_sources <- .in_clause("cod.source_id", source_ids)

  if (type == "similarity") {
    .graph_similarity(project$con, w_codes, w_sources, min_shared)
  } else if (type == "bipartite") {
    .graph_bipartite(project$con, w_codes, w_sources)
  } else {
    .graph_cooccurrence(project$con, w_codes, w_sources, min_shared)
  }
}

# ── Internal graph builders ───────────────────────────────────────────────────

.graph_similarity <- function(con, w_codes, w_sources, min_shared) {
  edges_raw <- .query(con, paste0("
    SELECT a.source_id                             AS from_id,
           b.source_id                             AS to_id,
           COUNT(DISTINCT a.code_id)               AS shared,
           STRING_AGG(DISTINCT c.name, ', ' ORDER BY c.name) AS shared_codes
    FROM   codings a
    JOIN   codings b
           ON  b.code_id   = a.code_id
           AND b.source_id > a.source_id
           AND b.status    = 1
    JOIN   codes c ON c.id = a.code_id AND c.status = 1
    WHERE  a.status = 1 ", w_codes, w_sources, "
    GROUP  BY a.source_id, b.source_id
    HAVING COUNT(DISTINCT a.code_id) >= ", min_shared, "
    ORDER  BY shared DESC
  "))

  if (nrow(edges_raw) == 0L)
    return(list(nodes = tibble::tibble(id = integer(0), label = character(0)),
                edges = tibble::tibble(from = integer(0), to = integer(0))))

  node_ids <- unique(c(edges_raw$from_id, edges_raw$to_id))
  src_info <- .query(con, paste0(
    "SELECT id, name FROM sources WHERE status = 1",
    .in_clause("id", node_ids)
  ))

  n_cod <- .query(con, paste0(
    "SELECT source_id, COUNT(*) AS n FROM codings
     WHERE  status = 1 ", w_sources, "
     GROUP  BY source_id"
  ))

  nodes <- tibble::tibble(
    id    = src_info$id,
    label = src_info$name,
    group = "document",
    value = n_cod$n[match(src_info$id, n_cod$source_id)],
    title = paste0("<b>", src_info$name, "</b>")
  )

  edges <- tibble::tibble(
    from  = edges_raw$from_id,
    to    = edges_raw$to_id,
    value = edges_raw$shared,
    title = paste0(edges_raw$shared, " shared code(s): ",
                   edges_raw$shared_codes),
    label = as.character(edges_raw$shared)
  )

  list(nodes = nodes, edges = edges)
}

.graph_bipartite <- function(con, w_codes, w_sources) {
  codings <- .query(con, paste0("
    SELECT DISTINCT cod.source_id, cod.code_id,
                    s.name AS source_name, c.name AS code_name,
                    c.color AS code_color,
                    COUNT(*) OVER (PARTITION BY cod.source_id, cod.code_id)
                      AS n_codings
    FROM   codings cod
    JOIN   sources s ON s.id = cod.source_id AND s.status = 1
    JOIN   codes   c ON c.id = cod.code_id   AND c.status = 1
    WHERE  cod.status = 1 ", w_codes, w_sources
  ))

  if (nrow(codings) == 0L)
    return(list(nodes = tibble::tibble(), edges = tibble::tibble()))

  doc_nodes <- unique(codings[, c("source_id", "source_name")])
  code_nodes <- unique(codings[, c("code_id", "code_name", "code_color")])

  nodes <- rbind(
    tibble::tibble(
      id    = paste0("d", doc_nodes$source_id),
      label = doc_nodes$source_name,
      group = "document",
      color = "#AEC6CF",
      shape = "box",
      title = paste0("<b>", doc_nodes$source_name, "</b>")
    ),
    tibble::tibble(
      id    = paste0("c", code_nodes$code_id),
      label = code_nodes$code_name,
      group = "code",
      color = code_nodes$code_color,
      shape = "ellipse",
      title = paste0("<b>Code:</b> ", code_nodes$code_name)
    )
  )

  edges <- tibble::tibble(
    from  = paste0("d", codings$source_id),
    to    = paste0("c", codings$code_id),
    value = codings$n_codings,
    title = paste0(codings$n_codings, " coding(s)")
  )

  list(nodes = nodes, edges = edges)
}

.graph_cooccurrence <- function(con, w_codes, w_sources, min_shared) {
  cooc <- .query(con, paste0("
    SELECT a.code_id                          AS from_id,
           b.code_id                          AS to_id,
           COUNT(DISTINCT a.source_id)        AS n,
           c1.name AS from_name, c2.name AS to_name,
           c1.color AS from_color, c2.color AS to_color
    FROM   codings a
    JOIN   codings b
           ON  b.source_id = a.source_id
           AND b.code_id   > a.code_id
           AND b.status    = 1
    JOIN   codes c1 ON c1.id = a.code_id AND c1.status = 1
    JOIN   codes c2 ON c2.id = b.code_id AND c2.status = 1
    WHERE  a.status = 1 ", w_codes, w_sources, "
    GROUP  BY a.code_id, b.code_id, c1.name, c2.name, c1.color, c2.color
    HAVING COUNT(DISTINCT a.source_id) >= ", min_shared, "
  "))

  if (nrow(cooc) == 0L)
    return(list(nodes = tibble::tibble(), edges = tibble::tibble()))

  n_cod <- .query(con, paste0(
    "SELECT code_id, COUNT(*) AS n FROM codings
     WHERE  status = 1 ", w_codes, " GROUP BY code_id"
  ))

  node_ids <- unique(c(cooc$from_id, cooc$to_id))
  from_rows <- cooc[!duplicated(cooc$from_id), c("from_id", "from_name", "from_color")]
  to_rows   <- cooc[!duplicated(cooc$to_id),   c("to_id",   "to_name",   "to_color")]
  names(from_rows) <- c("id", "name", "color")
  names(to_rows)   <- c("id", "name", "color")
  code_df <- unique(rbind(from_rows, to_rows))
  code_df  <- code_df[code_df$id %in% node_ids, ]

  nodes <- tibble::tibble(
    id    = code_df$id,
    label = code_df$name,
    group = "code",
    color = code_df$color,
    value = n_cod$n[match(code_df$id, n_cod$code_id)],
    title = paste0("<b>", code_df$name, "</b>")
  )

  edges <- tibble::tibble(
    from  = cooc$from_id,
    to    = cooc$to_id,
    value = cooc$n,
    title = paste0(cooc$n, " document(s)"),
    label = as.character(cooc$n)
  )

  list(nodes = nodes, edges = edges)
}
