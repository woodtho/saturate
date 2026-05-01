# Add a code to the project codebook

Add a code to the project codebook

## Usage

``` r
qc_add_code(
  project,
  name,
  color = "#4E79A7",
  memo = "",
  parent_id = NULL,
  definition = "",
  criteria = "",
  code_key = NULL,
  level = "",
  orientation = "",
  weight = NULL,
  weight_description = ""
)
```

## Arguments

- project:

  A `qc_project` object.

- name:

  Character. Code label. Must be unique.

- color:

  Character. Hex colour (e.g. `"#E15759"`).

- memo:

  Character. Short description / memo.

- parent_id:

  Integer or `NULL`. Parent code id for hierarchical taxonomies.

- definition:

  Character. Full definition of the code.

- criteria:

  Character. Inclusion/exclusion criteria for coders.

- code_key:

  Character or `NULL`. Stable slug (e.g. `"positive_affect"`).
  Auto-generated from `name` when `NULL`. Must be unique; may contain
  only lowercase letters, digits, and underscores.

- level:

  Character. Analytic level of the code (e.g. `"descriptive"`,
  `"interpretive"`).

- orientation:

  Character. Theoretical orientation of the code.

- weight:

  Numeric or `NULL`. Optional numeric weight for the code.

- weight_description:

  Character. Description of what the weight represents.

## Value

A one-row tibble: `id`, `name`, `color`, `memo`, `created_at`.
