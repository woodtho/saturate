test_that("qc_get_coded_segments returns all active codings", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc1 <- qc_import_document(proj, content = "Hello world", name = "d1")
  doc2 <- qc_import_document(proj, content = "Goodbye moon", name = "d2")
  c1   <- qc_add_code(proj, "code1")
  c2   <- qc_add_code(proj, "code2")
  qc_add_coding(proj, doc1$id, c1$id, 1L, 5L)
  qc_add_coding(proj, doc2$id, c2$id, 1L, 7L)

  segs <- qc_get_coded_segments(proj)
  expect_equal(nrow(segs), 2L)
  expect_true("seltext" %in% names(segs))
  expect_true("code_color" %in% names(segs))
})

test_that("code_ids filter works", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc <- qc_import_document(proj, content = "Hello world", name = "d")
  c1  <- qc_add_code(proj, "keep")
  c2  <- qc_add_code(proj, "drop")
  qc_add_coding(proj, doc$id, c1$id, 1L, 5L)
  qc_add_coding(proj, doc$id, c2$id, 7L, 11L)

  segs <- qc_get_coded_segments(proj, code_ids = c1$id)
  expect_equal(nrow(segs), 1L)
  expect_equal(segs$code_name[[1L]], "keep")
})

test_that("source_ids filter works", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  d1  <- qc_import_document(proj, content = "doc one text", name = "d1")
  d2  <- qc_import_document(proj, content = "doc two text", name = "d2")
  c1  <- qc_add_code(proj, "c1")
  qc_add_coding(proj, d1$id, c1$id, 1L, 3L)
  qc_add_coding(proj, d2$id, c1$id, 1L, 3L)

  segs <- qc_get_coded_segments(proj, source_ids = d1$id)
  expect_equal(nrow(segs), 1L)
  expect_equal(segs$source_name[[1L]], "d1")
})

test_that("category_ids filter returns only codes in that category", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text text text", name = "d")
  c1   <- qc_add_code(proj, "incat")
  c2   <- qc_add_code(proj, "outcat")
  cat  <- qc_add_category(proj, "mycat")
  qc_link_code_category(proj, c1$id, cat$id)
  qc_add_coding(proj, doc$id, c1$id, 1L, 4L)
  qc_add_coding(proj, doc$id, c2$id, 6L, 9L)

  segs <- qc_get_coded_segments(proj, category_ids = cat$id)
  expect_equal(nrow(segs), 1L)
  expect_equal(segs$code_name[[1L]], "incat")
})

test_that("qc_code_summary counts match manual count", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "ABCDE FGHIJ", name = "d")
  c1   <- qc_add_code(proj, "c1")
  qc_add_coding(proj, doc$id, c1$id, 1L, 5L)
  qc_add_coding(proj, doc$id, c1$id, 7L, 11L)

  summ <- qc_code_summary(proj)
  expect_equal(summ$n_segments[[1L]],  2L)
  expect_equal(summ$n_documents[[1L]], 1L)
})

test_that("qc_export writes a CSV file", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "Hello world", name = "d")
  c1   <- qc_add_code(proj, "c1")
  qc_add_coding(proj, doc$id, c1$id, 1L, 5L)

  out <- withr::local_tempfile(fileext = ".csv")
  qc_export(proj, out, format = "csv")
  expect_true(file.exists(out))
  df <- utils::read.csv(out)
  expect_equal(nrow(df), 1L)
})
