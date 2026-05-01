# Convert project network data to an igraph object

Builds an igraph object from code co-occurrence counts or from explicit
code relations stored in `code_relations`.

## Usage

``` r
qc_as_igraph(
  project,
  type = c("cooccurrence", "relations"),
  code_ids = NULL,
  source_ids = NULL,
  min_shared = 1L
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

  Integer. Minimum co-occurrence count to include an edge (applies to
  `"cooccurrence"` only).

## Value

An igraph object. For `"cooccurrence"`, edges carry a `weight` attribute
and vertices carry `color` and `n_codings`. For `"relations"`, edges
carry a `type` attribute (directed graph).
