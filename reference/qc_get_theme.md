# Get full detail for a single theme

Returns the theme row together with its linked categories and directly
linked codes.

## Usage

``` r
qc_get_theme(project, theme_id)
```

## Arguments

- project:

  A `qc_project` object.

- theme_id:

  Integer. Theme id.

## Value

A list with elements `theme` (1-row tibble), `linked_cats` (tibble), and
`linked_codes` (tibble).
