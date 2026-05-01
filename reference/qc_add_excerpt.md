# Add an excerpt to a document

Creates a labelled passage (selfirst-selast) within a document, separate
from any coding. Excerpts can carry a memo and are displayed in the
coding view as a distinct underline highlight.

## Usage

``` r
qc_add_excerpt(
  project,
  source_id,
  selfirst,
  selast,
  memo = "",
  coder = "default"
)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

- selfirst:

  Integer. 1-based start character position.

- selast:

  Integer. 1-based end character position (inclusive).

- memo:

  Character. Note about why this passage was excerpted.

- coder:

  Character. Coder identifier.

## Value

A one-row tibble: `id`, `source_id`, `selfirst`, `selast`, `seltext`,
`memo`, `coder`, `created_at`.
