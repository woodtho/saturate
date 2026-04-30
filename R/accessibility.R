#' Colour-blind-safe colour palette
#'
#' Returns `n` hex colours from a perceptually-distinct, colour-blind-safe
#' palette. Useful for assigning visually separable colours to codes when
#' your corpus will be shared with participants who have colour-vision
#' deficiency.
#'
#' Available palettes:
#' - **`"okabe_ito"`** (default): The 8-colour Okabe & Ito palette, safe for
#'   all forms of colour-blindness including monochromacy. Black is included.
#' - **`"wong"`**: Paul Tol's revision of Okabe-Ito, prioritising contrast on
#'   both white and black backgrounds.
#' - **`"tol"`**: Paul Tol's 7-colour bright scheme. No black; higher
#'   saturation than Okabe-Ito.
#'
#' When `n` exceeds the palette length the sequence wraps (cycles).
#'
#' @param n Integer. Number of colours required.
#' @param type One of `"okabe_ito"` (default), `"wong"`, or `"tol"`.
#'
#' @return A character vector of `n` hex colour codes (e.g. `"#E69F00"`).
#' @export
qc_cb_palette <- function(n, type = c("okabe_ito", "wong", "tol")) {
  type <- match.arg(type)
  n    <- as.integer(n)
  if (n < 0L) rlang::abort("`n` must be a non-negative integer.")
  if (n == 0L) return(character(0L))

  palettes <- list(
    okabe_ito = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                  "#0072B2", "#D55E00", "#CC79A7", "#000000"),
    wong      = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
    tol       = c("#4477AA", "#EE6677", "#228833", "#CCBB44",
                  "#66CCEE", "#AA3377", "#BBBBBB")
  )
  pal <- palettes[[type]]
  pal[((seq_len(n) - 1L) %% length(pal)) + 1L]
}
