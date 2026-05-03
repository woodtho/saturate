# Set the source type of a document

Source type categorises documents by data-collection method, e.g.
`"interview"`, `"focus_group"`, `"survey"`, `"observation"`. This label
is used by
[`qc_triangulate()`](https://woodtho.github.io/saturate/reference/qc_triangulate.md)
to compare code coverage across methods.

## Usage

``` r
qc_set_source_type(project, id, source_type)
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Document id.

- source_type:

  Character. Type label.

## Value

Invisibly, the updated document row.
