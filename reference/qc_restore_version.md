# Restore a document to a previous version

Calls
[`qc_update_document_content()`](https://thomaswood.github.io/saturate/reference/qc_update_document_content.md)
with the archived content, which archives the current version first,
then writes the restored text.

## Usage

``` r
qc_restore_version(project, source_id, version)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

- version:

  Integer. Version to restore.

## Value

The updated document tibble from
[`qc_get_document()`](https://thomaswood.github.io/saturate/reference/qc_get_document.md).
