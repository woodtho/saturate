# Find segments of one code near segments of another code

Returns all pairs of coded spans (one per code) within the same document
where the gap between them is at most `max_chars` characters.

## Usage

``` r
qc_proximity_query(project, code_id1, code_id2, max_chars = 200L)
```

## Arguments

- project:

  A `qc_project` object.

- code_id1:

  Integer. First code.

- code_id2:

  Integer. Second code.

- max_chars:

  Integer. Maximum character gap between spans (0 = overlapping).

## Value

A tibble: `source_name`, `coding1_id`, `c1_start`, `c1_end`, `c1_text`,
`coding2_id`, `c2_start`, `c2_end`, `c2_text`, `gap`.
