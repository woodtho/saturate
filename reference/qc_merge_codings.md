# Merge two or more codings into one

All codings must belong to the same document. The merged coding spans
`min(selfirst)` to `max(selast)` of the group. Input codings are
soft-deleted.

## Usage

``` r
qc_merge_codings(project, coding_ids, code_id = NULL, memo = "")
```

## Arguments

- project:

  A `qc_project` object.

- coding_ids:

  Integer vector. At least two coding ids.

- code_id:

  Integer or `NULL`. Code for the merged coding. Defaults to the code of
  the first coding in `coding_ids`.

- memo:

  Character. Memo for the merged coding.

## Value

A one-row tibble of the created coding.
