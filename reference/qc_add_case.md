# Add a case (respondent / subject)

Add a case (respondent / subject)

## Usage

``` r
qc_add_case(project, name, memo = "")
```

## Arguments

- project:

  A `qc_project` object.

- name:

  Character. Case label. Must be unique.

- memo:

  Character.

## Value

A one-row tibble: `id`, `name`, `memo`, `created_at`.
