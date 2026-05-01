# Retrieve the content of a specific document version

Retrieve the content of a specific document version

## Usage

``` r
qc_get_version(project, source_id, version)
```

## Arguments

- project:

  A `qc_project` object.

- source_id:

  Integer. Document id.

- version:

  Integer. Version number (as returned by
  [`qc_list_versions()`](https://thomaswood.github.io/saturate/reference/qc_list_versions.md)).

## Value

A one-row tibble: `version`, `content`, `content_hash`, `memo`,
`imported_at`.
