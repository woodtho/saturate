# Export the codebook to a file

Writes all active codes to a CSV, JSON, or Markdown file. CSV and JSON
include all fields: `code_key`, `name`, `color`, `memo`, `definition`,
`criteria`, `parent_name`, `depth`, `n_codings`, `deprecated`,
`deprecated_reason`, and `categories`. JSON requires jsonlite. Markdown
produces a human-readable reference document suitable for supplementary
materials.

## Usage

``` r
qc_export_codebook(project, path, format = c("csv", "json", "md"))
```

## Arguments

- project:

  A `qc_project` object.

- path:

  Character. Destination file path.

- format:

  One of `"csv"` (default), `"json"`, or `"md"`.

## Value

Invisibly, `path`.
