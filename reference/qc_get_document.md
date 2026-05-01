# Retrieve a single document's full text

Retrieve a single document's full text

## Usage

``` r
qc_get_document(project, id)
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. The document id.

## Value

A one-row tibble: `id`, `name`, `content`, `memo`.
