# Detect exact and near-duplicate documents

Compares documents pairwise using MD5 hashes (exact duplicates) and
Jaccard similarity on word sets (near-duplicates). No additional
packages are required.

## Usage

``` r
qc_detect_duplicates(
  project,
  threshold = 0.85,
  method = c("both", "exact", "near"),
  source_ids = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- threshold:

  Numeric in `[0, 1]`. Minimum Jaccard similarity to report as a
  near-duplicate (default `0.85`). Exact duplicates always appear
  regardless of threshold.

- method:

  One of `"both"`, `"exact"`, or `"near"`.

- source_ids:

  Integer vector or `NULL`. Restrict comparison to these documents.

## Value

A tibble: `source_id_1`, `name_1`, `source_id_2`, `name_2`,
`similarity`, `type` (`"exact"` or `"near"`). An empty tibble if no
duplicates are found.
