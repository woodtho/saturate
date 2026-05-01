# Retrieve memos across entity types

Returns a unified tibble of non-empty memos from codings, documents,
codes, and/or cases. Each row carries a `memo_type` flag so results can
be filtered or grouped after the fact.

## Usage

``` r
qc_get_memos(
  project,
  types = c("coding", "document", "code", "case"),
  code_ids = NULL,
  source_ids = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- types:

  Character vector. Subset of `c("coding","document","code","case")`.

- code_ids:

  Integer vector or `NULL`. Filter coding/code memos to these codes.

- source_ids:

  Integer vector or `NULL`. Filter coding/document memos to these
  documents.

## Value

A tibble: `memo_type`, `entity_id`, `source_name`, `code_name`, `coder`,
`memo`, `created_at`.
