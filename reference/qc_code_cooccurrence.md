# Code co-occurrence matrix

Returns a long-format table of code pairs that co-occur within the same
unit (document or overlapping segment).

## Usage

``` r
qc_code_cooccurrence(project, unit = c("document", "segment"))
```

## Arguments

- project:

  A `qc_project` object.

- unit:

  One of `"document"` (default) or `"segment"`. `"document"` counts
  pairs that appear anywhere in the same document; `"segment"` counts
  pairs whose spans overlap.

## Value

A tibble: `code1_id`, `code1_name`, `code2_id`, `code2_name`, `n`
(co-occurrence count). Ordered by `n` descending.
