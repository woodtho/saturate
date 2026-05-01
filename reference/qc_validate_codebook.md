# Validate the codebook for structural and quality issues

Runs a series of checks and returns a tibble of issues grouped by
severity. Uses a flat SQL fetch (not the recursive CTE) so it is safe to
call even when the hierarchy may be circular.

## Usage

``` r
qc_validate_codebook(project)
```

## Arguments

- project:

  A `qc_project` object.

## Value

A tibble: `code_id`, `code_name`, `issue_type`, `severity` (`"error"`,
`"warning"`, `"info"`), `message`. Ordered error -\> warning -\> info,
then alphabetically by code name. Returns an empty tibble (with a
success message) when no issues are found.

## Details

**Checks performed:**

- `orphan_parent` (error): `parent_id` references a non-existent code.

- `circular_hierarchy` (error): parent chain loops back to itself.

- `missing_code_key` (warning): no stable key assigned; call
  [`qc_set_code_key()`](https://thomaswood.github.io/saturate/reference/qc_set_code_key.md).

- `missing_definition` (warning): code has no definition text.

- `missing_criteria` (info): code has no inclusion/exclusion criteria.

- `unused_code` (info): code has zero active codings.

- `deprecated_with_codings` (warning): deprecated code still has active
  codings.
