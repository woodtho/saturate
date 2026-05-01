# Full-text search across documents

Finds all occurrences of a pattern within document content, returning
each match with surrounding context.

## Usage

``` r
qc_search_documents(
  project,
  pattern,
  regex = FALSE,
  ignore_case = TRUE,
  accent_fold = FALSE,
  source_ids = NULL,
  context_chars = 80L
)
```

## Arguments

- project:

  A `qc_project` object.

- pattern:

  Character. The search term or regular expression.

- regex:

  Logical. When `TRUE`, `pattern` is treated as a Perl-compatible regex.
  When `FALSE` (default), it is matched literally.

- ignore_case:

  Logical. Case-insensitive search (default `TRUE`).

- accent_fold:

  Logical. When `TRUE` (requires `stringi`), both the document text and
  `pattern` are converted to ASCII-equivalent characters before
  matching, enabling accent-insensitive search (e.g. `"cafe"` matches
  `"cafe"`).

- source_ids:

  Integer vector or `NULL`. Restrict to these documents.

- context_chars:

  Integer. Characters of surrounding text to include on each side of the
  match (default 80).

## Value

A tibble: `source_id`, `source_name`, `match_n`, `match_start`,
`match_end`, `match_text`, `context`.
