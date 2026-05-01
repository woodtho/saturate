# Import a text document into the project

Supports `.txt`, `.csv`, `.docx`, `.pdf`, and spreadsheet files.
Requires `readtext` (any format), `officer` (`.docx`), `pdftools`
(`.pdf`), or `readxl` (`.xlsx`/`.xls`) to be installed for the
corresponding format.

## Usage

``` r
qc_import_document(
  project,
  path = NULL,
  content = NULL,
  name = NULL,
  memo = "",
  language = "",
  parent_id = NULL,
  source_type = ""
)
```

## Arguments

- project:

  A `qc_project` object.

- path:

  Character. Path to a file. When `NULL`, `content` must be given.

- content:

  Character scalar. Raw document text (used when `path = NULL`).

- name:

  Character. Display name. Defaults to filename without extension.

- memo:

  Character. Initial memo text.

- language:

  Character. BCP-47 language tag, e.g. `"en"`, `"fr-CA"`.

- parent_id:

  Integer or `NULL`. Parent document id for segments.

- source_type:

  Character. Data-collection method label, e.g. `"interview"`,
  `"focus_group"`, `"survey"`, `"observation"`. Used by
  [`qc_triangulate()`](https://thomaswood.github.io/saturate/reference/qc_triangulate.md)
  and
  [`qc_saturation_curve()`](https://thomaswood.github.io/saturate/reference/qc_saturation_curve.md).

## Value

A one-row tibble: `id`, `name`, `created_at`.

## Details

Unicode text is normalised to NFC on import when `stringi` is available.
An MD5 hash of the content is stored; if an identical document already
exists, a warning is emitted.
