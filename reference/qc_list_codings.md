# List codings, optionally filtered

List codings, optionally filtered

## Usage

``` r
qc_list_codings(project, source_id = NULL, code_id = NULL, coder = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer or `NULL`. Restrict to a single document.

- code_id:

  Integer or `NULL`. Restrict to a single code.

- coder:

  Character or `NULL`. Restrict to a single coder. Used to implement
  blind coding – pass the current coder's name to hide all other coders'
  annotations.

## Value

A tibble: `id`, `source_id`, `code_id`, `code_name`, `code_color`,
`selfirst`, `selast`, `seltext`, `memo`, `coder`, `confidence`,
`created_at`. Ordered by `selfirst`.
