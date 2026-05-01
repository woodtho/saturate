# List excerpts

List excerpts

## Usage

``` r
qc_list_excerpts(project, source_id = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer or `NULL`. Filter to a single document.

## Value

A tibble: `id`, `source_id`, `source_name`, `selfirst`, `selast`,
`seltext`, `memo`, `coder`, `created_at`.
