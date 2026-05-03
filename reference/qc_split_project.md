# Create a coder copy of a project

Copies the codebook, cases, themes, and a subset (or all) of documents
into a new standalone `.duckdb` file. Codings are excluded by default so
each coder starts fresh. IDs are NOT preserved – the copy gets fresh
sequences – so the files can be merged back later by name / content-hash
matching.

## Usage

``` r
qc_split_project(
  project,
  path,
  source_ids = NULL,
  include_codings = FALSE,
  overwrite = FALSE
)
```

## Arguments

- project:

  A `qc_project` object.

- path:

  Character. Destination file path (`.duckdb`).

- source_ids:

  Integer vector of source IDs to include, or `NULL` for all.

- include_codings:

  Logical. Copy existing codings into the split file.

- overwrite:

  Logical. Overwrite `path` if it already exists.

## Value

The new `qc_project` (invisibly). Caller must call
[`qc_close()`](https://woodtho.github.io/saturate/reference/qc_close.md).
