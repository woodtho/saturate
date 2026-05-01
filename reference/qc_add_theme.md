# Create a new analytical theme

A theme is a patterned, coherent meaning that addresses the research
question. It integrates multiple categories or code clusters and should
be expressed as an analytical statement (proposition), not merely a
label.

## Usage

``` r
qc_add_theme(
  project,
  name,
  central_concept = "",
  narrative = "",
  definition = "",
  scope = "",
  code_ids = NULL,
  category_ids = NULL,
  created_by = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- name:

  Character. Short theme label.

- central_concept:

  Character. The central organizing idea in one sentence.

- narrative:

  Character. Extended analytical justification / proposition.

- definition:

  Character. What counts as belonging to this theme.

- scope:

  Character. Inclusion/exclusion criteria.

- code_ids:

  Integer vector or `NULL`. Direct code links to create.

- category_ids:

  Integer vector or `NULL`. Category links to create.

- created_by:

  Character or `NULL`. Defaults to the current system user.

## Value

A one-row tibble: `id`, `name`, `created_at`.
