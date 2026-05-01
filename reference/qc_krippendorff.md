# Krippendorff's alpha for one code across multiple coders

Computes Krippendorff's alpha (nominal metric) for a single code across
any number of coders. The unit of analysis is the document: each coder
either applied the code to a document (1) or did not (0). Only documents
coded by at least two of the specified coders contribute to the
calculation.

## Usage

``` r
qc_krippendorff(project, code_id, coders = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- code_id:

  Integer. The code to evaluate.

- coders:

  Character vector or `NULL`. Restrict to these coders. Defaults to all
  coders in the project.

## Value

A one-row tibble: `code_id`, `code_name`, `n_coders`, `n_units`,
`observed_disagreement`, `expected_disagreement`, `alpha`.

## Details

Alpha interpretation: \> 0.8 = strong, 0.67-0.8 = tentative, \< 0.67 =
unreliable (Krippendorff 2004 thresholds for content analysis).
