test_that("qc_import_document from content returns correct columns", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  out <- qc_import_document(proj, content = "Hello world", name = "doc1")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("id", "name", "created_at"))
  expect_equal(out$name, "doc1")
})

test_that("qc_list_documents shows imported doc with n_codings = 0", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_import_document(proj, content = "Hello world", name = "doc1")

  docs <- qc_list_documents(proj)
  expect_equal(nrow(docs), 1L)
  expect_equal(docs$n_codings[[1L]], 0L)
  expect_equal(docs$word_count[[1L]], 2L)
  expect_equal(docs$char_count[[1L]], 11L)
})

test_that("qc_get_document returns content", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  imported <- qc_import_document(proj, content = "full text here", name = "d")

  doc <- qc_get_document(proj, imported$id)
  expect_equal(doc$content, "full text here")
})

test_that("qc_update_document_memo updates memo", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  imp <- qc_import_document(proj, content = "x", name = "d")
  qc_update_document_memo(proj, imp$id, "new memo")

  doc <- qc_get_document(proj, imp$id)
  expect_equal(doc$memo, "new memo")
})

test_that("qc_delete_document soft-deletes the document", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  imp <- qc_import_document(proj, content = "x", name = "d")

  qc_delete_document(proj, imp$id)
  docs <- qc_list_documents(proj)
  expect_equal(nrow(docs), 0L)
})

test_that("qc_delete_document also soft-deletes attached codings", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  imp  <- qc_import_document(proj, content = "Hello world", name = "d")
  code <- qc_add_code(proj, "testcode")
  qc_add_coding(proj, imp$id, code$id, 1L, 5L)

  n <- qc_delete_document(proj, imp$id)
  expect_equal(n, 1L)
  segs <- qc_get_coded_segments(proj)
  expect_equal(nrow(segs), 0L)
})

test_that("qc_import_document errors when both path and content given", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  expect_error(qc_import_document(proj, path = "x", content = "y"), "not both")
})

test_that("include_content = TRUE adds content column", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_import_document(proj, content = "abc", name = "d")
  docs <- qc_list_documents(proj, include_content = TRUE)
  expect_true("content" %in% names(docs))
})
