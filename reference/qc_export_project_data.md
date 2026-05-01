# Export a raw project database table

Exports any major project table as-is for archival, secondary analysis,
or transfer between tools.

## Usage

``` r
qc_export_project_data(
  project,
  table_name = "codings",
  format = c("csv", "json", "xlsx"),
  output_path = NULL
)
```

## Arguments

- project:

  A `qc_project` object.

- table_name:

  One of the supported table names (see Details).

- format:

  One of `"csv"`, `"json"`, `"xlsx"`.

- output_path:

  File path or `NULL`.

## Value

Path to the generated file (invisibly).

## Details

Supported table names: `"documents"`, `"codes"`, `"codings"`,
`"categories"`, `"category_links"`, `"themes"`, `"cases"`,
`"case_attributes"`, `"annotations"`, `"memos"`, `"coding_audit"`,
`"code_history"`.
