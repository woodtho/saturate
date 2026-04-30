test_that("qc_update_document_content archives the previous version", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "original text", name = "d")

  qc_update_document_content(proj, doc$id, "updated text", memo = "minor fix")
  versions <- qc_list_versions(proj, doc$id)
  expect_equal(nrow(versions), 1L)
})

test_that("qc_update_document_content increments doc_version", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "v1", name = "d")

  updated <- qc_update_document_content(proj, doc$id, "v2")
  expect_equal(updated$doc_version, 2L)
})

test_that("qc_update_document_content sets new content", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "old content", name = "d")

  qc_update_document_content(proj, doc$id, "new content")
  live <- qc_get_document(proj, doc$id)
  expect_equal(live$content, "new content")
})

test_that("qc_get_version retrieves archived content", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "version one", name = "d")

  qc_update_document_content(proj, doc$id, "version two")
  v1 <- qc_get_version(proj, doc$id, 1L)
  expect_equal(v1$content, "version one")
})

test_that("qc_get_version errors on missing version", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  expect_error(qc_get_version(proj, doc$id, 99L))
})

test_that("qc_restore_version restores old content", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "original", name = "d")

  qc_update_document_content(proj, doc$id, "changed")
  qc_restore_version(proj, doc$id, 1L)

  live <- qc_get_document(proj, doc$id)
  expect_equal(live$content, "original")
})

test_that("qc_list_versions returns empty before any update", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  versions <- qc_list_versions(proj, doc$id)
  expect_equal(nrow(versions), 0L)
})
