# Merge a contributor project into a master project

Reads codes, documents, codings, themes, and memos from a contributor
`.duckdb` file and inserts any new items into `master`. Items that
already exist (matched by name / content hash) are skipped or replaced.

## Usage

``` r
qc_merge_project(
  master,
  contributor_path,
  on_conflict = c("skip", "replace"),
  coders = NULL
)
```

## Arguments

- master:

  A `qc_project` object (write target).

- contributor_path:

  Character. Path to the contributor `.duckdb` file.

- on_conflict:

  `"skip"` (default) leaves existing codings untouched. `"replace"`
  soft-deletes the existing coding and re-inserts.

- coders:

  Character vector. If provided, only import codings by these coder
  names.

## Value

Invisibly, a named list: `codes_added`, `sources_added`,
`codings_added`, `codings_skip`, `themes_added`, `memos_added`.
