# Update a theme's fields

Each changed field is recorded in `theme_history` before the update is
applied. Fields not supplied (or `NULL`) are left unchanged.

## Usage

``` r
qc_update_theme(
  project,
  id,
  name = NULL,
  central_concept = NULL,
  narrative = NULL,
  definition = NULL,
  scope = NULL,
  changed_by = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Theme id.

- name, central_concept, narrative, definition, scope:

  Character or `NULL`.

- changed_by:

  Character or `NULL`. Defaults to the current system user.

## Value

Invisibly `NULL`.
