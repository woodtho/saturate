# Remove a document (soft delete)

Also soft-deletes all codings attached to this document.

## Usage

``` r
qc_delete_document(project, id)
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Document id.

## Value

Invisibly, the number of codings also soft-deleted.
