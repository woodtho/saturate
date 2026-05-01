# Retrieve or update project-level metadata

Retrieve or update project-level metadata

## Usage

``` r
qc_project_info(project, name = NULL, owner = NULL, memo = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- name, owner, memo:

  Character scalars. Pass non-`NULL` to update.

## Value

A one-row tibble: `name`, `owner`, `memo`, `created_at`, `locked`.
