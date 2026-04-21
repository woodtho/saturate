make_test_project <- function() {
  path <- withr::local_tempfile(fileext = ".duckdb")
  qc_new(path, name = "test-project", owner = "tester")
}
