# Inter-coder agreement heatmap (mean Cohen's kappa)

Displays mean Cohen's kappa across all code-document combinations for
each pair of coders. The diagonal (self-agreement) is fixed at 1.0.

## Usage

``` r
qc_plot_overlap(project, code_ids = NULL, ...)
```

## Arguments

- project:

  A `qc_project` object.

- code_ids:

  Integer vector or `NULL`. Restrict the agreement calculation to these
  codes.

- ...:

  Unused. Reserved for future arguments.

## Value

A ggplot2 object, or NULL (with a warning) when fewer than two coders
are found.
