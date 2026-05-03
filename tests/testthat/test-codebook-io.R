test_that("qc_export_codebook writes CSV with correct columns", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_add_code(proj, "alpha", color = "#FF0000", memo = "first theme")
  qc_add_code(proj, "beta",  color = "#00FF00")

  tmp <- withr::local_tempfile(fileext = ".csv")
  qc_export_codebook(proj, tmp, format = "csv")

  df <- read.csv(tmp, stringsAsFactors = FALSE)
  expect_equal(nrow(df), 2L)
  expect_true(all(c("name", "color", "memo", "categories") %in% names(df)))
  expect_true("alpha" %in% df$name)
})

test_that("qc_import_codebook creates codes from CSV", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))

  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "name,color,memo,categories",
    "theme1,#E15759,a theme,",
    "theme2,#59A14F,,"
  ), tmp)

  result <- qc_import_codebook(proj, tmp, format = "csv")
  expect_equal(result$imported, 2L)
  expect_equal(result$skipped,  0L)

  codes <- qc_list_codes(proj)
  expect_equal(nrow(codes), 2L)
  expect_true("theme1" %in% codes$name)
})

test_that("qc_import_codebook skips existing codes when skip_existing = TRUE", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_add_code(proj, "existing")

  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("name,color", "existing,#FF0000", "new_code,#00FF00"), tmp)

  result <- qc_import_codebook(proj, tmp, format = "csv")
  expect_equal(result$imported, 1L)
  expect_equal(result$skipped,  1L)
  expect_equal(nrow(qc_list_codes(proj)), 2L)
})

test_that("qc_import_codebook creates categories and links them", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))

  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "name,color,memo,categories",
    "theme1,#E15759,,cat_a"
  ), tmp)

  qc_import_codebook(proj, tmp, format = "csv")

  codes <- qc_list_codes(proj)
  expect_true(grepl("cat_a", codes$categories[[1L]]))
})

test_that("export then import round-trips the codebook", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_add_code(proj, "c1", color = "#111111", memo = "memo one")
  qc_add_code(proj, "c2", color = "#222222")

  tmp <- withr::local_tempfile(fileext = ".csv")
  qc_export_codebook(proj, tmp, format = "csv")

  proj2 <- make_test_project()
  on.exit(qc_close(proj2), add = TRUE)
  qc_import_codebook(proj2, tmp, format = "csv")

  codes2 <- qc_list_codes(proj2)
  expect_equal(nrow(codes2), 2L)
  row1 <- codes2[codes2$name == "c1", ]
  expect_equal(row1$color[[1L]], "#111111")
  expect_equal(row1$memo[[1L]], "memo one")
})

test_that("qc_export_codebook errors on non-existent format", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))
  tmp <- withr::local_tempfile(fileext = ".txt")
  expect_error(qc_export_codebook(proj, tmp, format = "xml"))
})

test_that("qc_import_codebook errors on missing file", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))
  expect_error(qc_import_codebook(proj, "/no/such/file.csv"))
})
