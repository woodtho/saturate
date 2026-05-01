# Split a coding into two at a character position

Soft-deletes the original coding and creates two replacements:
`[selfirst, split_at]` and `[split_at + 1, selast]`. Both children
inherit the original's code, coder, source, and coding metadata.

## Usage

``` r
qc_split_coding(project, coding_id, split_at, memo1 = "", memo2 = "")
```

## Arguments

- project:

  A `qc_project` object.

- coding_id:

  Integer. The coding to split.

- split_at:

  Integer. Absolute character position (same coordinate system as
  `selfirst`/`selast`). Must be in `[selfirst, selast - 1]`.

- memo1, memo2:

  Character. Memos for the two new codings.

## Value

A two-row tibble of the created codings.
