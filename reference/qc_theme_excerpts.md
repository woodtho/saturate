# Retrieve all coded excerpts for a theme

Returns every active coding whose code belongs to this theme – either
via a direct code link or via a linked category. Use this for internal
homogeneity checks: all passages should cohere around the theme's
central concept.

## Usage

``` r
qc_theme_excerpts(project, theme_id)
```

## Arguments

- project:

  A `qc_project` object.

- theme_id:

  Integer. Theme id.

## Value

A tibble: `id`, `source_id`, `code_id`, `selfirst`, `selast`, `seltext`,
`memo`, `coder`, `code_name`, `code_color`, `doc_name`, `source_type`.
