# Add an entry to the project analytical journal

Appends a timestamped memo to the project-level reflexivity / analytical
journal. Entries are append-only (no updates, only soft deletes) so the
research audit trail is preserved.

## Usage

``` r
qc_add_project_memo(project, content, type = "analytical", created_by = NULL)
```

## Arguments

- project:

  A `qc_project` object.

- content:

  Character. The memo text (supports Markdown).

- type:

  Character. One of `"analytical"`, `"reflexivity"`, `"decision"`,
  `"methodological"`, or any custom label.

- created_by:

  Character or `NULL`. Researcher identifier; defaults to the system
  username.

## Value

A one-row tibble: `id`, `content`, `memo_type`, `created_by`,
`created_at`.
