# Time-series of code usage driven by a document-level date attribute

Groups codings by a calendar period (year, month, ISO week, or day)
using a named `source_attributes` variable as the date axis.

## Usage

``` r
qc_temporal_analysis(
  project,
  date_attr = "doc_date",
  period = c("year", "month", "week", "day"),
  code_ids = NULL,
  source_ids = NULL,
  coder = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- date_attr:

  Character. The `source_attributes.variable` that stores ISO-8601 date
  strings for each document.

- period:

  One of `"year"`, `"month"` (default), `"week"`, or `"day"`.

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

- source_ids:

  Integer vector or `NULL`. Restrict to these documents.

- coder:

  Character or `NULL`. Restrict to codings by this coder.

## Value

A tibble: `period`, `code_id`, `code_name`, `n_codings`, `n_documents`.
Sorted by period then code name.
