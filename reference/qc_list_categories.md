# List all categories with their member codes

List all categories with their member codes

## Usage

``` r
qc_list_categories(project)
```

## Arguments

- project:

  A `qc_project` object.

## Value

A tibble: `category_id`, `category_name`, `code_id`, `code_name`,
`code_color`. Codes not in any category appear with `NA` category
columns.
