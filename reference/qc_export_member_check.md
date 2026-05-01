# Export a member check as a shareable document

Generates an HTML or plain-text document showing the coded segments for
participant review.

## Usage

``` r
qc_export_member_check(
  project,
  check_id,
  path = NULL,
  format = c("html", "txt", "docx")
)
```

## Arguments

- project:

  A `qc_project` object.

- check_id:

  Integer. Member check id (from
  [`qc_list_member_checks()`](https://thomaswood.github.io/saturate/reference/qc_list_member_checks.md)).

- path:

  Character or `NULL`. Output file path. When `NULL`, returns the
  content as a character string.

- format:

  One of `"html"` (default) or `"txt"`.

## Value

Invisibly, the file path written, or the content string when
`path = NULL`.
