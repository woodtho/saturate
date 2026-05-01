# Save a snapshot of the current codebook

Serialises all active codes (including names, colours, memos,
definitions, criteria, parent relationships, and category links) to JSON
and stores the result in the `codebook_snapshots` table. This provides a
reproducible, point-in-time record of the codebook state.

## Usage

``` r
qc_snapshot_codebook(project, label = "")
```

## Arguments

- project:

  A `qc_project` object.

- label:

  Character. Optional description for this snapshot (e.g.
  `"after initial coding round"`).

## Value

A one-row tibble: `id`, `label`, `created_at`.
