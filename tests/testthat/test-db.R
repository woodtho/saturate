test_that("qc_new creates a valid project", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  expect_s3_class(proj, "qc_project")
  expect_true(DBI::dbIsValid(proj$con))
  expect_true(DBI::dbExistsTable(proj$con, "project_meta"))
})

test_that("qc_project_info returns correct defaults", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  info <- qc_project_info(proj)
  expect_equal(nrow(info), 1L)
  expect_equal(info$name,  "test-project")
  expect_equal(info$owner, "tester")
})

test_that("qc_project_info updates fields", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_project_info(proj, name = "Renamed", memo = "notes here")
  info <- qc_project_info(proj)
  expect_equal(info$name, "Renamed")
  expect_equal(info$memo, "notes here")
})

test_that("qc_new errors if file exists and overwrite = FALSE", {
  proj <- make_test_project()
  path <- proj$path
  qc_close(proj)

  expect_error(qc_new(path, overwrite = FALSE), "already exists")
})

test_that("qc_new overwrites when overwrite = TRUE", {
  proj  <- make_test_project()
  path  <- proj$path
  qc_close(proj)

  proj2 <- qc_new(path, name = "fresh", overwrite = TRUE)
  on.exit(qc_close(proj2))
  expect_equal(qc_project_info(proj2)$name, "fresh")
})

test_that("qc_open errors on non-existent path", {
  expect_error(qc_open(tempfile(fileext = ".duckdb")), "not found")
})

test_that("qc_close invalidates the connection", {
  proj <- make_test_project()
  qc_close(proj)
  expect_false(DBI::dbIsValid(proj$con))
})

test_that("print.qc_project does not error", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  expect_output(print(proj), "qc_project")
})
