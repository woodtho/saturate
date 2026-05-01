# Lock or unlock a project against further edits

A locked project rejects all write operations (`qc_add_coding`,
`qc_import_document`, `qc_add_code`, etc.). Use this to freeze a dataset
after finalising coding so downstream exports are reproducible.

## Usage

``` r
qc_lock_project(project)

qc_unlock_project(project)

qc_is_locked(project)
```

## Arguments

- project:

  A `qc_project` object.

## Value

Invisibly `TRUE` (locked) or `FALSE` (unlocked).
