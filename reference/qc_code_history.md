# Retrieve the change history for one or all codes

Returns an append-only audit log. Each row records one mutation: a
`create` event when a code is first added, one `update` event per
changed field (with before/after values), and a `delete` event when the
code is soft-deleted.

## Usage

``` r
qc_code_history(project, code_id = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- code_id:

  Integer or `NULL`. When supplied, restricts results to that code. When
  `NULL`, returns history for all codes.

## Value

A tibble: `id`, `code_id`, `code_name`, `operation`, `field`,
`old_value`, `new_value`, `changed_at`. Ordered newest-first.
