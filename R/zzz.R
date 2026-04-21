.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "saturate ", utils::packageVersion("saturate"),
    " — use shiny_saturate() to launch the GUI"
  )
}
