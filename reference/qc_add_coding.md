# Add a coded segment

Records that the passage from character offset `selfirst` to `selast`
(1-based, both inclusive, matching `substr(content, selfirst, selast)`)
in document `source_id` is tagged with `code_id`. The passage text is
snapshotted into `seltext` at write time.

## Usage

``` r
qc_add_coding(
  project,
  source_id,
  code_id,
  selfirst,
  selast,
  memo = "",
  coder = "default",
  coding_source = "manual",
  coding_status = "validated",
  confidence = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

- code_id:

  Integer. Code id.

- selfirst:

  Integer. 1-based start character position.

- selast:

  Integer. 1-based end character position (inclusive).

- memo:

  Character. Optional per-segment memo.

- coder:

  Character. Coder identifier (username or label).

- coding_source:

  One of `"manual"` or `"auto"`.

- coding_status:

  One of `"draft"` or `"validated"`.

- confidence:

  Integer 0-100 or `NULL`. Coder's confidence that this passage belongs
  under this code. `NULL` means unrated.

## Value

A one-row tibble: `id`, `source_id`, `code_id`, `selfirst`, `selast`,
`seltext`, `memo`, `coder`, `coding_source`, `coding_status`,
`confidence`, `created_at`.
