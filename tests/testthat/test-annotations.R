test_that("qc_add_annotation returns one-row tibble", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  ann <- qc_add_annotation(proj, doc$id, "interesting point")
  expect_equal(ann$annotation, "interesting point")
  expect_equal(ann$source_id, doc$id)
})

test_that("qc_add_annotation with position records offset", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "Hello world", name = "d")

  ann <- qc_add_annotation(proj, doc$id, "at start", position = 1L)
  expect_equal(ann$position, 1L)
})

test_that("qc_add_annotation without position has NULL position", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  ann <- qc_add_annotation(proj, doc$id, "whole doc note")
  expect_true(is.na(ann$position))
})

test_that("qc_list_annotations returns all for a document", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  qc_add_annotation(proj, doc$id, "note 1")
  qc_add_annotation(proj, doc$id, "note 2")
  anns <- qc_list_annotations(proj, source_id = doc$id)
  expect_equal(nrow(anns), 2L)
})

test_that("qc_list_annotations coder filter works", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  qc_add_annotation(proj, doc$id, "by alice", coder = "alice")
  qc_add_annotation(proj, doc$id, "by bob",   coder = "bob")

  anns <- qc_list_annotations(proj, coder = "alice")
  expect_equal(nrow(anns), 1L)
  expect_equal(anns$coder[[1L]], "alice")
})

test_that("qc_update_annotation changes text", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  ann <- qc_add_annotation(proj, doc$id, "original")
  qc_update_annotation(proj, ann$id, "revised")

  anns <- qc_list_annotations(proj)
  expect_equal(anns$annotation[[1L]], "revised")
})

test_that("qc_delete_annotation soft-deletes it", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  ann <- qc_add_annotation(proj, doc$id, "to delete")
  qc_delete_annotation(proj, ann$id)
  expect_equal(nrow(qc_list_annotations(proj)), 0L)
})

test_that("qc_list_annotations includes source_name", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "mydoc")

  qc_add_annotation(proj, doc$id, "note")
  anns <- qc_list_annotations(proj)
  expect_equal(anns$source_name[[1L]], "mydoc")
})
