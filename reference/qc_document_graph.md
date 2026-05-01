# Build graph data for the document-code network

Returns node and edge tibbles suitable for `visNetwork` or `igraph`.
Three graph types are supported:

## Usage

``` r
qc_document_graph(
  project,
  type = c("similarity", "bipartite", "cooccurrence"),
  code_ids = NULL,
  source_ids = NULL,
  min_shared = 1L
)
```

## Arguments

- project:

  A `qc_project` object.

- type:

  One of `"similarity"` (default), `"bipartite"`, `"cooccurrence"`.

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

- source_ids:

  Integer vector or `NULL`. Restrict to these documents.

- min_shared:

  Integer. For `"similarity"`: minimum shared codes for an edge. For
  `"cooccurrence"`: minimum co-document count.

## Value

A named list: `nodes` and `edges`, each a tibble formatted for
[`visNetwork::visNetwork()`](https://rdrr.io/pkg/visNetwork/man/visNetwork.html).

## Details

- `"similarity"` – documents are nodes; edges connect documents that
  share at least `min_shared` codes, weighted by the number of shared
  codes.

- `"bipartite"` – documents *and* codes are nodes; edges represent
  individual coding relationships.

- `"cooccurrence"` – codes are nodes; edges connect codes that co-occur
  in at least one document, weighted by the document count.
