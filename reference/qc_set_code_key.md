# Assign a stable key to a code

Sets a human-readable slug on a code that stays constant across renames.
Keys must be unique across all active codes and may contain only
lowercase letters, digits, and underscores.

## Usage

``` r
qc_set_code_key(project, id, key)
```

## Arguments

- project:

  A `qc_project` object.

- id:

  Integer. Code id.

- key:

  Character. The key to assign (e.g. `"positive_affect"`).

## Value

Invisibly, `key`.
