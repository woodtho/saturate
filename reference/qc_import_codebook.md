# Import a codebook from a file

Reads codes from a CSV or JSON file and adds them to the project. Codes
whose names already exist are skipped by default. Category names in the
file are created if absent, then linked to the imported code.

## Usage

``` r
qc_import_codebook(
  project,
  path,
  format = c("csv", "json"),
  skip_existing = TRUE
)
```

## Arguments

- project:

  A `qc_project` object.

- path:

  Character. Path to the import file.

- format:

  One of `"csv"` (default) or `"json"`.

- skip_existing:

  Logical. When `TRUE` (default), codes whose names already exist are
  silently skipped.

## Value

Invisibly, a one-row tibble: `imported`, `skipped`.

## Details

**CSV columns:** `name` (required), `color`, `memo`, `definition`,
`criteria`, `code_key`, `categories` (comma-separated names in a single
cell).

**JSON format:** array of objects – `name`, `color`, `memo`,
`definition`, `criteria`, `code_key`, `categories` (array of strings).
JSON import requires jsonlite.
