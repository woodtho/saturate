# Retrieve all coded segments, optionally filtered

The primary analysis function. Returns a flat tibble of every coded
passage.

## Usage

``` r
qc_get_coded_segments(
  project,
  code_ids = NULL,
  must_have = NULL,
  must_not = NULL,
  source_ids = NULL,
  case_ids = NULL,
  category_ids = NULL,
  coder = NULL,
  coding_source = NULL,
  coding_status = NULL,
  source_attrs = NULL,
  limit = NULL,
  offset = 0L
)
```

## Arguments

- project:

  A `qc_project` object.

- code_ids:

  Integer vector or `NULL`. OR filter – return segments that carry any
  of these codes.

- must_have:

  Integer vector or `NULL`. AND filter – restrict to documents that
  contain *all* of these codes somewhere (any segment).

- must_not:

  Integer vector or `NULL`. NOT filter – exclude documents that carry
  any of these codes.

- source_ids:

  Integer vector or `NULL`. Restrict to these documents.

- case_ids:

  Integer vector or `NULL`. Restrict to documents linked to these cases.

- category_ids:

  Integer vector or `NULL`. Restrict to codes in these categories.

- coder:

  Character or `NULL`. Restrict to codings by this coder.

- coding_source:

  One of `"manual"`, `"auto"`, or `NULL` for all.

- coding_status:

  One of `"draft"`, `"validated"`, or `NULL` for all.

- source_attrs:

  Named list of source-attribute filters, e.g.
  `list(industry = "tech", size = "small")`. Every key-value pair must
  match a row in `source_attributes` for the document to be included
  (AND semantics across pairs).

- limit:

  Integer or `NULL`. Maximum number of rows to return. `NULL` (default)
  returns all matching rows. Use with `offset` for pagination.

- offset:

  Integer. Number of rows to skip before returning results (default 0).
  Only meaningful when `limit` is set.

## Value

A tibble: `coding_id`, `source_id`, `source_name`, `code_id`,
`code_name`, `code_color`, `category_names`, `selfirst`, `selast`,
`seltext`, `memo`, `coder`, `coding_source`, `coding_status`,
`created_at`.
