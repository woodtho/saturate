# Create a member check record

Records that a set of coded segments has been prepared for participant
review. Use
[`qc_export_member_check()`](https://thomaswood.github.io/saturate/reference/qc_export_member_check.md)
to generate the shareable document and
[`qc_record_member_response()`](https://thomaswood.github.io/saturate/reference/qc_record_member_response.md)
to log the participant's feedback.

## Usage

``` r
qc_create_member_check(
  project,
  source_id,
  participant_label,
  code_ids = NULL,
  created_by = NULL,
  return_by = "",
  return_to = "",
  return_instructions = "",
  notes = ""
)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

- participant_label:

  Character. Identifier for the participant (e.g. name, pseudonym, or
  participant ID).

- code_ids:

  Integer vector or `NULL`. Restrict to specific codes. `NULL` includes
  all active codings on the document.

- created_by:

  Character or `NULL`. Records who created this check. Defaults to the
  current system user.

- return_by:

  Character. Requested return date for the participant's response (free
  text, e.g. `"2025-03-01"`).

- return_to:

  Character. Contact name or address to return feedback to.

- return_instructions:

  Character. Instructions for how the participant should return their
  response.

- notes:

  Character. Internal notes about this member check.

## Value

A one-row tibble: `id`, `source_id`, `participant_label`, `status`,
`sent_at`.
