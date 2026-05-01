# List all documents in the project

List all documents in the project

## Usage

``` r
qc_list_documents(project, include_content = FALSE, segments = TRUE)
```

## Arguments

- project:

  A `qc_project` object.

- include_content:

  Logical. Include the full `content` column.

- segments:

  Logical. When `FALSE`, only root documents (`parent_id IS NULL`) are
  returned, hiding segments created by
  [`qc_segment_document()`](https://thomaswood.github.io/saturate/reference/qc_segment_document.md).

## Value

A tibble: `id`, `name`, `memo`, `filename`, `source_system`, `language`,
`source_type`, `doc_version`, `word_count`, `char_count`, `parent_id`,
`n_codings`, `n_coders`, `created_at` (and `content` when
`include_content = TRUE`).
