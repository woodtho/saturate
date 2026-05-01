# List project journal entries

List project journal entries

## Usage

``` r
qc_list_project_memos(project, type = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- type:

  Character or `NULL`. Filter to a specific memo type; pass `NULL` to
  return all types.

## Value

A tibble: `id`, `content`, `memo_type`, `created_by`, `created_at`,
ordered newest-first.
