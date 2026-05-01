# Export an analytical themes report

Generates a formatted document containing each theme's proposition,
narrative, definition, scope, linked codes/categories, and supporting
excerpts. Suitable for sharing with supervisors or writing up methods.

## Usage

``` r
qc_export_themes_report(
  project,
  format = c("docx", "html", "txt", "json"),
  theme_ids = NULL,
  include_excerpts = TRUE,
  include_narrative = TRUE,
  output_path = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- format:

  One of `"docx"`, `"html"`, `"txt"`, `"json"`.

- theme_ids:

  Integer vector or `NULL` (all themes).

- include_excerpts:

  Logical. Include coded excerpts under each theme.

- include_narrative:

  Logical. Include narrative and definition fields.

- output_path:

  File path or `NULL` (returns a temp file path).

## Value

Path to the generated file (invisibly).
