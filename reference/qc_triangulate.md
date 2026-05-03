# Triangulate codes across source types

Compares how codes (or themes) are distributed across different
data-collection methods. A code with strong presence across multiple
source types provides better-triangulated evidence than one appearing in
only one.

## Usage

``` r
qc_triangulate(
  project,
  code_ids = NULL,
  category_ids = NULL,
  metric = c("segments", "documents")
)
```

## Arguments

- project:

  A `qc_project` object.

- code_ids:

  Integer vector or `NULL`. Restrict to specific codes.

- category_ids:

  Integer vector or `NULL`. Restrict to codes in these categories.

- metric:

  One of `"segments"` (count of coded segments, default) or
  `"documents"` (count of distinct documents containing the code).

## Value

A wide tibble: one row per code, one column per source type, values are
segment or document counts. Includes a `total` column. Rows ordered by
`total` descending.

## Details

Documents must have a `source_type` set via
[`qc_set_source_type()`](https://woodtho.github.io/saturate/reference/qc_set_source_type.md)
or the `source_type` argument in
[`qc_import_document()`](https://woodtho.github.io/saturate/reference/qc_import_document.md).
Documents without a type are grouped under `"unspecified"`.
