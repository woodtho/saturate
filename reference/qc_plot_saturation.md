# Plot the code saturation curve

Requires `ggplot2`.

## Usage

``` r
qc_plot_saturation(project, ..., dark = FALSE)
```

## Arguments

- project:

  A `qc_project` object.

- ...:

  Additional arguments passed to
  [`qc_saturation_curve()`](https://woodtho.github.io/saturate/reference/qc_saturation_curve.md).

- dark:

  Logical. Apply dark-mode plot colours.

## Value

A `ggplot` object.
