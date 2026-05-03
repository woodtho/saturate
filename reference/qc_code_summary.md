# Summarise coded segment counts per code

Summarise coded segment counts per code

## Usage

``` r
qc_code_summary(project, ...)
```

## Arguments

- project:

  A `qc_project` object.

- ...:

  Passed to
  [`qc_get_coded_segments()`](https://woodtho.github.io/saturate/reference/qc_get_coded_segments.md)
  for filtering.

## Value

A tibble: `code_id`, `code_name`, `n_segments`, `n_documents`.
