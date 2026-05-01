# Import multiple documents from a directory or tabular file

Handles three source types:

- **Directory**: imports every file whose extension matches
  `file_pattern`.

- **CSV / TSV**: each row becomes one document; `text_col` names the
  text column, `name_col` the display-name column (optional).

- **Excel (`.xlsx` / `.xls`)**: same row-per-document model.

## Usage

``` r
qc_import_batch(
  project,
  path,
  text_col = NULL,
  name_col = NULL,
  metadata_cols = NULL,
  format = NULL,
  language = "",
  file_pattern = "*",
  sheet = 1L,
  skip = 0L
)
```

## Arguments

- project:

  A `qc_project` object.

- path:

  Character. Path to a directory, CSV/TSV, or Excel file.

- text_col:

  Character. Column name containing document text (tabular sources
  only).

- name_col:

  Character or `NULL`. Column to use as document name (tabular). When
  `NULL`, names are generated from the source file and row number.

- metadata_cols:

  Character vector or `NULL`. Column names whose values are stored as
  source attributes.

- format:

  One of `"dir"`, `"csv"`, `"tsv"`, `"xlsx"`, `"xls"`, or `NULL`
  (auto-detect from extension / path type).

- language:

  Character. BCP-47 language tag applied to all imported docs.

- file_pattern:

  Glob passed to
  [`fs::dir_ls()`](https://fs.r-lib.org/reference/dir_ls.html) for
  directory import (default `"*"`).

- sheet:

  Integer. Sheet index for Excel files (default `1`).

- skip:

  Integer. Rows to skip before the header in tabular files.

## Value

A tibble with one row per imported document: `id`, `name`, `created_at`,
`row` (source row or filename).

## Details

Columns listed in `metadata_cols` are stored as `source_attributes` on
the imported document.
