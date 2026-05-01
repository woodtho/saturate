# Link categories to a theme

All codes in a linked category are included when computing theme
excerpts and the structure view. Re-linking a previously unlinked
category restores without duplicating.

## Usage

``` r
qc_link_theme_categories(project, theme_id, category_ids)
```

## Arguments

- project:

  A `qc_project` object.

- theme_id:

  Integer. Theme id.

- category_ids:

  Integer vector. One or more category ids.

## Value

Invisibly `NULL`.
