# Split a code into new codes

Creates `length(new_names)` new codes and logs the split in
`code_history`. The original code is left intact – use
[`qc_reassign_coding()`](https://thomaswood.github.io/saturate/reference/qc_reassign_coding.md)
or the Codebook "Review Codings" panel to move passages to the new
codes, then delete the original when done.

## Usage

``` r
qc_split_code(project, code_id, new_names, colors = NULL, memos = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- code_id:

  Integer. The code to split.

- new_names:

  Character vector (length \>= 2). Names for the new codes.

- colors:

  Character vector. Hex colours; recycled or defaulted to `"#4E79A7"`.

- memos:

  Character vector. Memos; recycled or defaulted to `""`.

## Value

A tibble of the newly created codes.
