# Export the project analytical journal

Writes all journal entries to a file. Supported formats: `"docx"`
(Word), `"html"`, `"txt"` (plain text), and `"csv"`.

## Usage

``` r
qc_export_journal(project, path = NULL, format = "docx")
```

## Arguments

- project:

  A `qc_project` object.

- path:

  Character. Output file path. If `NULL`, a temp file is created and its
  path returned.

- format:

  Character. One of `"docx"`, `"html"`, `"txt"`, `"csv"`.

## Value

The output file path, invisibly.
