# Symmetric tile heatmap of code co-occurrence

Displays how frequently pairs of codes appear together within the same
unit (document or overlapping segment). The heatmap is symmetric – each
pair appears once in each triangle.

## Usage

``` r
qc_plot_cooccurrence(
  project,
  code_ids = NULL,
  unit = c("document", "segment"),
  dark = FALSE,
  ...
)
```

## Arguments

- project:

  A `qc_project` object.

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

- unit:

  One of `"document"` (default) or `"segment"`.

- dark:

  Logical. Apply dark-mode plot colours.

- ...:

  Unused. Reserved for future arguments.

## Value

A ggplot2 object, or NULL (with a warning) when no co-occurrences are
found.
