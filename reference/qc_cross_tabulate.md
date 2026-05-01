# Cross-tabulate code frequency by a case attribute

Returns a wide table: rows = attribute values, columns = code names,
cells = number of documents with that code-attribute combination.

## Usage

``` r
qc_cross_tabulate(project, attribute, code_ids = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- attribute:

  Character. The case attribute variable to cross-tabulate by (must
  match a `variable` value in `case_attributes`).

- code_ids:

  Integer vector or `NULL`. Restrict to these codes.

## Value

A tibble: `attribute_value`, then one column per code.
