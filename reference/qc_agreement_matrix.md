# Agreement matrix across all coder pairs and codes

Calls
[`qc_agreement()`](https://thomaswood.github.io/saturate/reference/qc_agreement.md)
for every combination of coders and codes that appear in the project.

## Usage

``` r
qc_agreement_matrix(project, code_ids = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

## Value

A tibble with one row per (code, coder1, coder2) triple.
