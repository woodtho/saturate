# List code relations

List code relations

## Usage

``` r
qc_list_code_relations(project, code_id = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- code_id:

  Integer or `NULL`. When supplied, returns all relations where this
  code appears on either side.

## Value

A tibble: `id`, `code_id_1`, `name_1`, `code_id_2`, `name_2`,
`relation_type`, `note`, `created_at`.
