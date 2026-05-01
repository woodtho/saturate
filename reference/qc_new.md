# Create a new saturate project

Initialises a DuckDB database at `path` and bootstraps the schema.

## Usage

``` r
qc_new(
  path,
  name = fs::path_ext_remove(fs::path_file(path)),
  owner = Sys.info()[["user"]],
  overwrite = FALSE
)
```

## Arguments

- path:

  Character. Path to the `.duckdb` file to create.

- name:

  Character. Project name stored in metadata.

- owner:

  Character. Owner name.

- overwrite:

  Logical. Overwrite an existing file if `TRUE`.

## Value

A `qc_project` object (invisibly).
