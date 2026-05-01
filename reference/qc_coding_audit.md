# Retrieve the coding audit log

Returns an append-only record of every coding operation (create, delete,
update, reassign) across the project. Combined with
[`qc_code_history()`](https://thomaswood.github.io/saturate/reference/qc_code_history.md)
this gives a complete audit trail of all analytical decisions.

## Usage

``` r
qc_coding_audit(
  project,
  source_id = NULL,
  operation = NULL,
  from_date = NULL,
  to_date = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer or `NULL`. Filter to a single document.

- operation:

  Character or `NULL`. One of `"create"`, `"delete"`, `"update"`,
  `"reassign"`.

- from_date:

  Date/POSIXct or `NULL`. Earliest `changed_at` to include.

- to_date:

  Date/POSIXct or `NULL`. Latest `changed_at` to include.

## Value

A tibble ordered by `changed_at` descending: `id`, `coding_id`,
`operation`, `field`, `old_value`, `new_value`, `source_name`,
`code_name`, `selfirst`, `selast`, `seltext`, `coder`, `changed_by`,
`changed_at`.
