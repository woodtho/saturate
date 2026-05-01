# Colour-blind-safe colour palette

Returns `n` hex colours from a perceptually-distinct, colour-blind-safe
palette. Useful for assigning visually separable colours to codes when
your corpus will be shared with participants who have colour-vision
deficiency.

## Usage

``` r
qc_cb_palette(n, type = c("okabe_ito", "wong", "tol"))
```

## Arguments

- n:

  Integer. Number of colours required.

- type:

  One of `"okabe_ito"` (default), `"wong"`, or `"tol"`.

## Value

A character vector of `n` hex colour codes (e.g. `"#E69F00"`).

## Details

Available palettes:

- **`"okabe_ito"`** (default): The 8-colour Okabe & Ito palette, safe
  for all forms of colour-blindness including monochromacy. Black is
  included.

- **`"wong"`**: Paul Tol's revision of Okabe-Ito, prioritising contrast
  on both white and black backgrounds.

- **`"tol"`**: Paul Tol's 7-colour bright scheme. No black; higher
  saturation than Okabe-Ito.

When `n` exceeds the palette length the sequence wraps (cycles).
