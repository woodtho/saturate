# Update a code's fields

Each changed field is recorded in `code_history` before the update is
applied, preserving a complete audit trail.

## Usage

``` r
qc_update_code(
  project,
  id,
  name = NULL,
  color = NULL,
  memo = NULL,
  definition = NULL,
  criteria = NULL,
  parent_id = NULL,
  level = NULL,
  orientation = NULL,
  weight = NULL,
  weight_description = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Code id.

- name, color, memo, definition, criteria:

  Character scalars. Pass non-`NULL` to update.

- parent_id:

  Integer or `NA`. Pass an integer to set a parent; `NA` to make the
  code a root node.

- level:

  Character or `NULL`. Analytic level of the code.

- orientation:

  Character or `NULL`. Theoretical orientation of the code.

- weight:

  Numeric, `NA`, or `NULL`. Pass a number to set; `NA` to clear.

- weight_description:

  Character or `NULL`. Description of what the weight represents.

## Value

The updated one-row tibble.
