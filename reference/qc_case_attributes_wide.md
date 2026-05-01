# Get all case attributes as a wide tibble

Pivots the EAV `case_attributes` table into one column per variable
name.

## Usage

``` r
qc_case_attributes_wide(project)
```

## Arguments

- project:

  A `qc_project` object.

## Value

A tibble: `case_id`, `case_name`, then one column per attribute
variable.
