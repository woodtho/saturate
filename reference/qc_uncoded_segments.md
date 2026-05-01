# Find uncoded text segments in a document

Splits a document into paragraphs or sentences and returns those that
have no overlapping active coding. Useful for navigating to unreviewed
text.

## Usage

``` r
qc_uncoded_segments(
  project,
  source_id,
  unit = c("paragraph", "sentence"),
  min_chars = 20L
)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

- unit:

  One of `"paragraph"` (default) or `"sentence"`.

- min_chars:

  Integer. Minimum segment length to report (default 20).

## Value

A tibble: `start`, `end`, `text`.
