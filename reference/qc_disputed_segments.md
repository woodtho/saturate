# Find disputed or draft-status segments in a document

Returns codings that are either in `"draft"` status or that overlap with
another coder's coding on a different code (a coder conflict).

## Usage

``` r
qc_disputed_segments(project, source_id)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

## Value

A tibble: `coding_id`, `code_name`, `coder`, `reason`, `selfirst`,
`selast`, `seltext`. Ordered by `selfirst`.
