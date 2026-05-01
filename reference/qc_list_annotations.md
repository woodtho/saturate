# List annotations

List annotations

## Usage

``` r
qc_list_annotations(project, source_id = NULL, coder = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer or `NULL`. Restrict to a single document.

- coder:

  Character or `NULL`. Restrict to a single coder.

## Value

A tibble: `id`, `source_id`, `source_name`, `position`, `annotation`,
`coder`, `created_at`. Ordered by document then position.
