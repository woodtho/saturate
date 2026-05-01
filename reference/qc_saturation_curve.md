# Compute the code saturation curve

Returns one row per document (in import order, or first-coded order)
showing how many new codes were introduced and the running cumulative
total. A curve that flattens signals theoretical saturation – new data
is no longer producing new codes.

## Usage

``` r
qc_saturation_curve(
  project,
  order_by = c("import_order", "first_coded"),
  code_ids = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- order_by:

  One of `"import_order"` (document creation date, default) or
  `"first_coded"` (earliest coding timestamp in each document).

- code_ids:

  Integer vector or `NULL`. Restrict to a subset of codes.

## Value

A tibble: `doc_index`, `doc_name`, `source_type`, `n_codings`,
`new_codes`, `cumulative_codes`.
