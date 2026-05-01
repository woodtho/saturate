# Pairwise inter-coder agreement (Cohen's kappa) for one code

Computes Cohen's kappa at the document level: for each document coded by
at least one of the two coders, was the code applied (1) or not (0)?
Kappa measures agreement beyond chance on this binary decision.

## Usage

``` r
qc_agreement(project, code_id, coder1, coder2)
```

## Arguments

- project:

  A `qc_project` object.

- code_id:

  Integer. The code to evaluate.

- coder1:

  Character. First coder identifier.

- coder2:

  Character. Second coder identifier.

## Value

A one-row tibble: `code_id`, `code_name`, `coder1`, `coder2`, `n_docs`,
`n_agree`, `pct_agree`, `kappa`, `n11`, `n10`, `n01`, `n00`.

- `n11` = both coded, `n10` = coder1 only, `n01` = coder2 only,

- `n00` = neither (among docs seen by at least one coder).
