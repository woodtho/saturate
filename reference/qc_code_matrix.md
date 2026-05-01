# Code-by-entity matrix

Returns a wide tibble of codes (columns) crossed with entities (rows).
Entities can be documents, cases linked via `case_source_links`, or
distinct values of a named source attribute. Cell values are coding
counts, binary presence/absence flags, or total characters coded.

## Usage

``` r
qc_code_matrix(
  project,
  by = c("document", "case", "attribute"),
  attribute = NULL,
  code_ids = NULL,
  source_ids = NULL,
  values = c("count", "binary", "chars"),
  coder = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- by:

  One of `"document"` (default), `"case"`, or `"attribute"`.

- attribute:

  Character. Required when `by = "attribute"`. Name of the
  `source_attributes.variable` to use as the row dimension.

- code_ids:

  Integer vector or `NULL`. Restrict columns to these codes.

- source_ids:

  Integer vector or `NULL`. Restrict to these documents.

- values:

  One of `"count"` (default), `"binary"`, or `"chars"`.

- coder:

  Character or `NULL`. Restrict to codings by this coder.

## Value

A tibble: identifier columns then one column per code, filled with 0
where the code was not applied.
