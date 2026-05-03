# Retrieve a codebook snapshot as a tibble

Retrieve a codebook snapshot as a tibble

## Usage

``` r
qc_get_snapshot(project, snapshot_id)
```

## Arguments

- project:

  A `qc_project` object.

- snapshot_id:

  Integer. The snapshot id from
  [`qc_list_snapshots()`](https://woodtho.github.io/saturate/reference/qc_list_snapshots.md).

## Value

A tibble of codes as they existed at snapshot time.
