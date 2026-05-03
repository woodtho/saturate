# Mark a code as deprecated

Deprecated codes are retained and their historical codings are
preserved, but
[`qc_add_coding()`](https://woodtho.github.io/saturate/reference/qc_add_coding.md)
will reject new codings against them. Reverse with
[`qc_undeprecate_code()`](https://woodtho.github.io/saturate/reference/qc_undeprecate_code.md).

## Usage

``` r
qc_deprecate_code(project, id, reason = "")
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Code id.

- reason:

  Character. Optional explanation (stored and shown in exports).

## Value

Invisibly, `TRUE`.
