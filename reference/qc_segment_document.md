# Segment a document into sub-units

Splits the content of an existing document into child documents, each
stored with `parent_id` pointing to the original. Useful for splitting
long interview transcripts into paragraphs, sentences, or speaker turns
before coding.

## Usage

``` r
qc_segment_document(
  project,
  source_id,
  method = c("paragraph", "sentence", "speaker_turn", "response_id"),
  min_chars = 20L,
  pattern = NULL,
  keep_parent = TRUE
)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document to segment.

- method:

  Segmentation method: `"paragraph"` (split on two or more consecutive
  newlines), `"sentence"` (linguistic sentence boundaries via
  `stringi`), `"speaker_turn"` (lines beginning with `SPEAKER:` or
  `Speaker:`), or `"response_id"` (lines beginning with a numeric or
  letter-prefixed identifier such as `Q1:` or `1.`).

- min_chars:

  Integer. Segments shorter than this (after trimming) are dropped
  (default `20L`).

- pattern:

  Character or `NULL`. Custom regex overriding the default turn/ID
  detection pattern (for `"speaker_turn"` and `"response_id"`).

- keep_parent:

  Logical. When `TRUE` (default) the original document is kept. When
  `FALSE` it is soft-deleted after its segments are created.

## Value

A tibble with one row per segment: `id`, `name`, `created_at`,
`segment_n`.
