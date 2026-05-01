# Export a rich codebook

Extends
[`qc_export_codebook()`](https://thomaswood.github.io/saturate/reference/qc_export_codebook.md)
with optional example excerpts and more output formats including Word
and Excel.

## Usage

``` r
qc_export_codebook_full(
  project,
  format = c("docx", "xlsx", "csv", "json", "html"),
  include_definitions = TRUE,
  include_criteria = TRUE,
  include_memo = FALSE,
  include_examples = FALSE,
  n_examples = 2L,
  output_path = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- format:

  One of `"docx"`, `"xlsx"`, `"csv"`, `"json"`, `"html"`.

- include_definitions:

  Logical. Include code definitions.

- include_criteria:

  Logical. Include inclusion/exclusion criteria.

- include_memo:

  Logical. Include code memos.

- include_examples:

  Logical. Include example excerpts per code.

- n_examples:

  Integer. Maximum excerpts per code when `include_examples`.

- output_path:

  File path or `NULL`.

## Value

Path to the generated file (invisibly).
