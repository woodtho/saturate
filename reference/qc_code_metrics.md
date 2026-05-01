# Per-code prevalence, density, and Gries' DP dispersion

For each code, computes how broadly it is used (prevalence across
documents), how densely it covers the corpus (density as a percentage of
total characters), and how evenly it is spread (Gries' DP dispersion,
where 0 = perfectly uniform, 1 = concentrated in a single document).

## Usage

``` r
qc_code_metrics(project, code_ids = NULL, source_ids = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

- source_ids:

  Integer vector or `NULL`. Restrict to these documents.

## Value

A tibble: `code_id`, `code_name`, `n_codings`, `n_documents`,
`total_documents`, `prevalence`, `mean_chars`, `total_chars_coded`,
`density`, `dispersion`. Ordered by `n_codings` descending.
