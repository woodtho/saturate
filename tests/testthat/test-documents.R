# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc <- qc_import_document(proj, content = "Hello world", name = "doc1")

# ── Retrieval tests ───────────────────────────────────────────────────────────

test_that("qc_import_document content= stores and retrieves content", {
  out <- qc_import_document(proj, content = "Hello world", name = "content-doc")
  fetched <- qc_get_document(proj, out$id)
  expect_equal(fetched$content, "Hello world")
})

test_that("qc_import_document path= reads a text file", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines("content from file", tmp)

  out <- qc_import_document(proj, path = tmp)
  fetched <- qc_get_document(proj, out$id)
  expect_equal(fetched$content, "content from file")
})

test_that("qc_list_documents returns correct column set", {
  docs <- qc_list_documents(proj)

  expected_cols <- c("id", "name", "memo", "filename", "source_system",
                     "language", "source_type", "doc_version", "word_count",
                     "char_count", "parent_id", "n_codings", "n_coders",
                     "created_at")
  expect_true(all(expected_cols %in% names(docs)))
})

test_that("qc_get_document returns content", {
  imported <- qc_import_document(proj, content = "full text here", name = "get-doc")
  fetched <- qc_get_document(proj, imported$id)
  expect_equal(fetched$content, "full text here")
})

test_that("qc_update_document_memo persists memo", {
  imp <- qc_import_document(proj, content = "x", name = "memo-doc")
  qc_update_document_memo(proj, imp$id, "updated memo")
  fetched <- qc_get_document(proj, imp$id)
  expect_equal(fetched$memo, "updated memo")
})

test_that("qc_import_document records non-NA created_at timestamp", {
  out <- qc_import_document(proj, content = "text", name = "ts-doc")
  expect_false(is.na(out$created_at))
})

test_that("qc_import_document stores source_type", {
  out <- qc_import_document(proj, content = "text", name = "st-doc",
                             source_type = "interview")
  fetched <- qc_get_document(proj, out$id)
  expect_equal(fetched$source_type, "interview")
})

# ── Delete test ───────────────────────────────────────────────────────────────

test_that("qc_delete_document soft-deletes the document", {
  imp <- qc_import_document(proj, content = "x", name = "del-doc")
  qc_delete_document(proj, imp$id)
  expect_false(imp$id %in% qc_list_documents(proj)$id)
})

# ── Source type options ───────────────────────────────────────────────────────

test_that(".source_type_options on fresh project returns at least five defaults", {
  p2 <- make_test_project()
  withr::defer(qc_close(p2))

  opts <- saturate:::.source_type_options(p2)
  expect_true(length(opts) >= 5L)
  expect_true("interview"   %in% opts)
  expect_true("focus_group" %in% opts)
  expect_true("survey"      %in% opts)
  expect_true("observation" %in% opts)
  expect_true("document"    %in% opts)
})

test_that(".source_type_options returns project-specific type added via qc_set_source_type", {
  qc_import_document(proj, content = "text", name = "diary-doc",
                     source_type = "diary_entry")
  opts <- saturate:::.source_type_options(proj)
  expect_true("diary_entry" %in% opts)
})
