# Project summary report

Compiles key project statistics into a named list of tibbles suitable
for rendering with
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) or
`flextable`. The return value has S3 class `"qc_report"` with a `print`
method that produces a formatted console summary.

## Usage

``` r
qc_summary_report(project, include_metrics = TRUE, top_n = 15L)
```

## Arguments

- project:

  A `qc_project` object.

- include_metrics:

  Logical. When `TRUE` (default), calls
  [`qc_code_metrics()`](https://thomaswood.github.io/saturate/reference/qc_code_metrics.md)
  and includes prevalence, density, and dispersion.

- top_n:

  Integer. Number of top codes / co-occurrences to include.

## Value

An S3 object of class `"qc_report"`: a named list with elements
`project`, `corpus`, `codes`, `cooccurrence`, `coders`, and `metrics`.
