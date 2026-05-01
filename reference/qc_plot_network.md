# Network diagram of code relationships

Renders a force-directed network graph of code co-occurrences or
explicit code relations. Requires the `ggraph` package; if absent,
returns the igraph object invisibly with an informative message.

## Usage

``` r
qc_plot_network(
  project,
  type = c("cooccurrence", "relations"),
  code_ids = NULL,
  source_ids = NULL,
  min_shared = 2L,
  layout = "fr",
  ...
)
```

## Arguments

- project:

  A `qc_project` object.

- type:

  One of `"cooccurrence"` (default) or `"relations"`.

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

- source_ids:

  Integer vector or `NULL`. Restrict to these documents (applies to
  `"cooccurrence"` only).

- min_shared:

  Integer. Minimum co-occurrence count to draw an edge.

- layout:

  Character. igraph/ggraph layout algorithm (e.g. `"fr"`, `"kk"`,
  `"stress"`).

- ...:

  Unused. Reserved for future arguments.

## Value

A ggplot2/ggraph object, or the igraph object invisibly when ggraph is
not installed.
