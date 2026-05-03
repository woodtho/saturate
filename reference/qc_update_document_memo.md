# Update the memo on a document

Update the memo on a document

## Usage

``` r
qc_update_document_memo(project, id, memo)
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Document id.

- memo:

  Character. New memo text.

## Value

The updated one-row tibble (same shape as
[`qc_get_document()`](https://woodtho.github.io/saturate/reference/qc_get_document.md)).
