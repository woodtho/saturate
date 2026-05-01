# Merge one or more codes into a surviving code

All codings from `from_ids` are reassigned to `into_id`. The merged-away
codes are soft-deleted. A `'merge'` event is written to `code_history`
for each affected code so the operation is fully auditable.

## Usage

``` r
qc_merge_codes(project, from_ids, into_id)
```

## Arguments

- project:

  A `qc_project` object.

- from_ids:

  Integer vector. Codes to merge away (will be deleted).

- into_id:

  Integer. The surviving code that absorbs all codings.

## Value

Invisibly, a one-row tibble for the surviving code.
