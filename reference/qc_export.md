# Export coded segments to CSV or xlsx

Export coded segments to CSV or xlsx

## Usage

``` r
qc_export(project, path, format = c("csv", "xlsx"), ...)
```

## Arguments

- project:

  A `qc_project` object.

- path:

  Character. Output file path.

- format:

  One of `"csv"`, `"xlsx"`.

- ...:

  Passed to
  [`qc_get_coded_segments()`](https://thomaswood.github.io/saturate/reference/qc_get_coded_segments.md)
  for filtering.

## Value

`path`, invisibly.
