# Compare two codebook snapshots

Returns a row-per-change tibble describing what differed between two
point-in-time snapshots. Codes are matched by their stable numeric `id`.

## Usage

``` r
qc_diff_snapshots(project, snapshot_id_1, snapshot_id_2)
```

## Arguments

- project:

  A `qc_project` object.

- snapshot_id_1:

  Integer. The earlier snapshot (baseline).

- snapshot_id_2:

  Integer. The later snapshot (comparison).

## Value

A tibble: `code_id`, `code_name`, `change_type`, `field`, `old_value`,
`new_value`. Returns an empty tibble with an info message when the
snapshots are identical.

## Details

**Change types:**

- `"added"`: code present in snapshot 2 but not snapshot 1.

- `"removed"`: code present in snapshot 1 but not snapshot 2.

- `"changed"`: code present in both but one or more fields differ.
