# Add an annotation to a document

Annotations are free-text notes attached to a character position (or the
document as a whole) and are distinct from coded segments – they do not
assign a code label, they express a thought or question about the text.

## Usage

``` r
qc_add_annotation(
  project,
  source_id,
  annotation,
  position = NULL,
  coder = "default"
)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

- annotation:

  Character. The annotation text.

- position:

  Integer or `NULL`. 1-based character position in the document. `NULL`
  attaches the annotation to the document as a whole.

- coder:

  Character. Coder identifier (default `"default"`).

## Value

A one-row tibble: `id`, `source_id`, `position`, `annotation`, `coder`,
`created_at`.
