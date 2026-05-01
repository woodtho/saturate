# Horizontal bar chart of code frequency

Visualises how often codes were applied, measured either by total coding
count or by the number of distinct documents that carry each code.

## Usage

``` r
qc_plot_codes(
  project,
  top_n = 20L,
  by = c("codings", "documents"),
  code_ids = NULL,
  ...
)
```

## Arguments

- project:

  A `qc_project` object.

- top_n:

  Integer. Maximum number of codes to show.

- by:

  One of `"codings"` (default) or `"documents"`.

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

- ...:

  Unused. Reserved for future arguments.

## Value

A ggplot2 object.
