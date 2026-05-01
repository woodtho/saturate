# Apply a code automatically using a regular expression

Scans document content for all matches of `pattern` and creates a
`'auto'`-sourced coding for each match. Useful for dictionary-based or
rule-based initial tagging.

## Usage

``` r
qc_auto_code(
  project,
  code_id,
  pattern,
  source_ids = NULL,
  coder = "auto",
  ignore_case = TRUE
)
```

## Arguments

- project:

  A `qc_project` object.

- code_id:

  Integer. Code to apply.

- pattern:

  Character. Perl-compatible regular expression.

- source_ids:

  Integer vector or `NULL`. Restrict to these documents.

- coder:

  Character. Coder identifier (default `"auto"`).

- ignore_case:

  Logical. (default `TRUE`).

## Value

Invisibly, the number of codings created.
