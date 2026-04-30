#' @importFrom utils head
#' @importFrom stats setNames
#' @importFrom rlang .data
utils::globalVariables(c("n_coders", "n_sources"))

.onLoad <- function(libname, pkgname) {
  app_dir <- system.file("app", package = pkgname, lib.loc = libname)
  if (nchar(app_dir) > 0L)
    shiny::addResourcePath("saturate-assets", app_dir)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "saturate ", utils::packageVersion("saturate"),
    " \u2014 use shiny_saturate() to launch the GUI"
  )
}
