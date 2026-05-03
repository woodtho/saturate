# Launch the saturate Shiny GUI

Opens an interactive coding interface. Pass a `brand` list to apply
organisation-specific colours and a custom app name – useful for
institutions that want to present the tool under their own branding.

## Usage

``` r
shiny_saturate(project = NULL, brand = NULL, max_upload_mb = 500L, ...)
```

## Arguments

- project:

  A `qc_project` object created by
  [`qc_new()`](https://woodtho.github.io/saturate/reference/qc_new.md)
  or
  [`qc_open()`](https://woodtho.github.io/saturate/reference/qc_open.md).
  If `NULL` (the default), a project picker is shown before the main
  interface launches – letting the user open an existing `.duckdb` file
  or create a new one.

- brand:

  Optional named list for visual branding. Supported keys:

  `name`

  :   App title shown in the navbar (default: `"saturate"`).

  `primary`

  :   CSS hex colour for the navbar and primary buttons (e.g.
      `"#003366"`). Ensure \>= 4.5:1 contrast with white.

  `primary_hover`

  :   Slightly darker version for hover states.

  `primary_fg`

  :   Foreground (text) colour on `primary` backgrounds (default:
      `"#ffffff"`).

  `accent`

  :   Accent colour used in charts and sparklines.

  `custom_css`

  :   A raw CSS string appended last – override anything.

- max_upload_mb:

  Integer. Maximum file size (MB) accepted by the Merge file-upload
  input (default: 500 MB).

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Called for its side effect; does not return normally.
