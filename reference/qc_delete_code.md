# Delete a code (soft delete)

Also soft-deletes all codings that used this code.

## Usage

``` r
qc_delete_code(project, id)
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Code id.

## Value

Invisibly, the number of codings also soft-deleted.
