# List all codes

List all codes

## Usage

``` r
qc_list_codes(project)
```

## Arguments

- project:

  A `qc_project` object.

## Value

A tibble with columns `id`, `name`, `color`, `memo`, `parent_id`,
`parent_name`, `definition`, `criteria`, `code_key`, `deprecated`,
`deprecated_reason`, `depth` (0 = root), `n_codings`, `categories`.
