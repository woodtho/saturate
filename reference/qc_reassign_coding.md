# Reassign a coding to a different code

Moves a single coding from its current code to `new_code_id`. Useful
after splitting a code to redistribute existing passages.

## Usage

``` r
qc_reassign_coding(project, coding_id, new_code_id)
```

## Arguments

- project:

  A `qc_project` object.

- coding_id:

  Integer. The coding to reassign.

- new_code_id:

  Integer. The target code.

## Value

Invisibly, `TRUE`.
