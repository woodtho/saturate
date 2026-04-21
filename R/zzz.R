.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "qualcoder ", utils::packageVersion("qualcoder"),
    " — use shiny_qualcoder() to launch the GUI"
  )
}
