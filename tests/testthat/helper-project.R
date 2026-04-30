make_test_project <- function(.env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".duckdb", .local_envir = .env)
  qc_new(path, name = "test-project", owner = "tester")
}
