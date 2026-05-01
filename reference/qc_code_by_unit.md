# Code a document by predefined text units

Computes unit boundaries within a document and applies a code to each
selected unit as a single coding that spans that unit's character range.
Useful for systematic coding of paragraphs, sentences, or structured
response items without manual selection.

## Usage

``` r
qc_code_by_unit(
  project,
  source_id,
  code_id,
  unit = c("paragraph", "sentence"),
  unit_indices = NULL,
  min_chars = 10L,
  coder = "default",
  coding_status = "validated",
  memo = ""
)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

- code_id:

  Integer. Code to apply.

- unit:

  One of `"paragraph"` (split on two or more blank lines) or
  `"sentence"` (linguistic boundaries via `stringi`).

- unit_indices:

  Integer vector or `NULL`. Which units to code (1-based). `NULL` codes
  every unit.

- min_chars:

  Integer. Skip units shorter than this (default `10L`).

- coder:

  Character. Coder identifier.

- coding_status:

  One of `"draft"` or `"validated"`.

- memo:

  Character. Memo applied to every coding created.

## Value

A tibble with one row per coding created: `id`, `unit_n`, `selfirst`,
`selast`, `seltext`.
