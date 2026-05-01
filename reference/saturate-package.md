# saturate: Qualitative Text Coding with DuckDB and Shiny

A modern replacement for RQDA. Manage coding projects in DuckDB,
annotate text passages programmatically or via a Shiny GUI, and retrieve
coded segments as tidy tibbles.

### Core workflow

1.  Create or open a project with
    [`qc_new()`](https://thomaswood.github.io/saturate/reference/qc_new.md)
    /
    [`qc_open()`](https://thomaswood.github.io/saturate/reference/qc_open.md).

2.  Import documents with
    [`qc_import_document()`](https://thomaswood.github.io/saturate/reference/qc_import_document.md).

3.  Define codes with
    [`qc_add_code()`](https://thomaswood.github.io/saturate/reference/qc_add_code.md).

4.  Code passages with
    [`qc_add_coding()`](https://thomaswood.github.io/saturate/reference/qc_add_coding.md)
    or interactively via
    [`shiny_saturate()`](https://thomaswood.github.io/saturate/reference/shiny_saturate.md).

5.  Retrieve results with
    [`qc_get_coded_segments()`](https://thomaswood.github.io/saturate/reference/qc_get_coded_segments.md).

6.  Close the project with
    [`qc_close()`](https://thomaswood.github.io/saturate/reference/qc_close.md).

## Author

**Maintainer**: Thomas Wood <thomaswood1994@gmail.com>
