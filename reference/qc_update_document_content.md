# Update a document's content, preserving the previous version

Archives the current content in `source_versions` before writing the new
text. All codings for this document are flagged
`coding_status = 'needs_review'` because their character offsets may no
longer be valid.

## Usage

``` r
qc_update_document_content(project, id, content, memo = "")
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Document id.

- content:

  Character. New document text.

- memo:

  Character. Version memo (reason for update).

## Value

The updated one-row tibble from
[`qc_get_document()`](https://thomaswood.github.io/saturate/reference/qc_get_document.md).
