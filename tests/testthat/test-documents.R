test_that("qc_import_document content= stores and retrieves content", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  out <- qc_import_document(proj, content = "Hello world", name = "doc1")
  doc <- qc_get_document(proj, out$id)
  expect_equal(doc$content, "Hello world")
})

test_that("qc_import_document path= reads a text file", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines("content from file", tmp)

  out <- qc_import_document(proj, path = tmp)
  doc <- qc_get_document(proj, out$id)
  expect_equal(doc$content, "content from file")
})

test_that("qc_list_documents returns correct column set", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_import_document(proj, content = "Hello world", name = "doc1")
  docs <- qc_list_documents(proj)

  expected_cols <- c("id", "name", "memo", "filename", "source_system",
                     "language", "source_type", "doc_version", "word_count",
                     "char_count", "parent_id", "n_codings", "n_coders",
                     "created_at")
  expect_true(all(expected_cols %in% names(docs)))
})

test_that("qc_get_document returns content", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  imported <- qc_import_document(proj, content = "full text here", name = "d")
  doc <- qc_get_document(proj, imported$id)
  expect_equal(doc$content, "full text here")
})

test_that("qc_update_document_memo persists memo", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  imp <- qc_import_document(proj, content = "x", name = "d")
  qc_update_document_memo(proj, imp$id, "updated memo")
  doc <- qc_get_document(proj, imp$id)
  expect_equal(doc$memo, "updated memo")
})

test_that("qc_delete_document soft-deletes the document", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  imp <- qc_import_document(proj, content = "x", name = "d")
  qc_delete_document(proj, imp$id)
  expect_equal(nrow(qc_list_documents(proj)), 0L)
})

test_that("qc_import_document records non-NA created_at timestamp", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  out <- qc_import_document(proj, content = "text", name = "d")
  expect_false(is.na(out$created_at))
})

test_that("qc_import_document stores source_type", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  out <- qc_import_document(proj, content = "text", name = "d",
                             source_type = "interview")
  doc <- qc_get_document(proj, out$id)
  expect_equal(doc$source_type, "interview")
})

test_that(".source_type_options on fresh project returns at least five defaults", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  opts <- saturate:::.source_type_options(proj)
  expect_true(length(opts) >= 5L)
  expect_true("interview"   %in% opts)
  expect_true("focus_group" %in% opts)
  expect_true("survey"      %in% opts)
  expect_true("observation" %in% opts)
  expect_true("document"    %in% opts)
})

test_that(".source_type_options returns project-specific type added via qc_set_source_type", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  doc <- qc_import_document(proj, content = "text", name = "d",
                             source_type = "diary_entry")
  opts <- saturate:::.source_type_options(proj)
  expect_true("diary_entry" %in% opts)
})
