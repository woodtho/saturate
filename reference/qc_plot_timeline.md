# Line chart of code usage over time

Shows how coding activity for the top codes changes across calendar
periods, using a document-level date attribute to anchor each coding on
the time axis.

## Usage

``` r
qc_plot_timeline(
  project,
  date_attr = "doc_date",
  period = c("month", "year", "week", "day"),
  code_ids = NULL,
  top_n = 8L,
  ...
)
```

## Arguments

- project:

  A `qc_project` object.

- date_attr:

  Character. The `source_attributes.variable` storing dates.

- period:

  One of `"month"` (default), `"year"`, `"week"`, or `"day"`.

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

- top_n:

  Integer. Number of most-used codes to plot.

- ...:

  Unused. Reserved for future arguments.

## Value

A ggplot2 object, or NULL (with a warning) when no temporal data is
found.
