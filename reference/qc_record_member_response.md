# Record a participant's response to a member check item

Updates one coding item within a member check. After every update the
overall check status is recalculated: `"confirmed"` when all items are
confirmed, `"disputed"` when any item is disputed, `"partial"` when
mixed, `"pending"` when nothing has been recorded yet.

## Usage

``` r
qc_record_member_response(
  project,
  check_id,
  coding_id,
  response = "",
  status = c("confirmed", "disputed", "other")
)
```

## Arguments

- project:

  A `qc_project` object.

- check_id:

  Integer. Member check id.

- coding_id:

  Integer. Coding id to update.

- response:

  Character. Free-text participant comment.

- status:

  One of `"confirmed"`, `"disputed"`, or `"other"`.

## Value

Invisibly, the updated check list (from
[`qc_list_member_checks()`](https://thomaswood.github.io/saturate/reference/qc_list_member_checks.md)).
