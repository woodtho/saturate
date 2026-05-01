# Add a code category

Add a code category

## Usage

``` r
qc_add_category(project, name, memo = "")
```

## Arguments

- project:

  A `qc_project` object.

- name:

  Character. Category name. Must be unique.

- memo:

  Character.

## Value

A one-row tibble: `id`, `name`, `memo`.
