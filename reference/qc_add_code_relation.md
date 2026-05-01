# Add a non-hierarchical relationship between two codes

Records a directed or symmetric semantic link between codes, e.g.
`"related_to"`, `"broader_than"`, `"narrower_than"`, or
`"co_occurs_with"`. The relation is stored as a directed edge
(`code_id_1 -> code_id_2`), but
[`qc_list_code_relations()`](https://thomaswood.github.io/saturate/reference/qc_list_code_relations.md)
returns both directions when filtering by a single code.

## Usage

``` r
qc_add_code_relation(project, code_id_1, code_id_2, relation_type, note = "")
```

## Arguments

- project:

  A `qc_project` object.

- code_id_1, code_id_2:

  Integer. The two codes to link.

- relation_type:

  Character. A short label for the relationship. Recommended vocabulary:
  `"related_to"`, `"broader_than"`, `"narrower_than"`,
  `"co_occurs_with"`, `"contradicts"`, `"precedes"`. Any string is
  accepted.

- note:

  Character. Optional explanation of the relationship.

## Value

A one-row tibble: `id`, `code_id_1`, `name_1`, `code_id_2`, `name_2`,
`relation_type`, `note`, `created_at`.
