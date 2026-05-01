# Set all items in a member check to the same status

Convenience helper for bulk confirm or dispute. Calls
[`qc_record_member_response()`](https://thomaswood.github.io/saturate/reference/qc_record_member_response.md)
for every item in the check.

## Usage

``` r
qc_bulk_set_member_status(
  project,
  check_id,
  status = c("confirmed", "disputed")
)
```

## Arguments

- project:

  A `qc_project` object.

- check_id:

  Integer. Member check id.

- status:

  One of `"confirmed"` or `"disputed"`.

## Value

Invisibly, the updated check list.
