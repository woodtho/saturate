# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc <- qc_import_document(proj, content = "Hello world", name = "ann-doc")

# ── Basic annotation tests ────────────────────────────────────────────────────

test_that("qc_add_annotation returns one-row tibble", {
  ann <- qc_add_annotation(proj, doc$id, "interesting point")
  expect_equal(ann$annotation, "interesting point")
  expect_equal(ann$source_id, doc$id)
})

test_that("qc_add_annotation with position records offset", {
  ann <- qc_add_annotation(proj, doc$id, "at start", position = 1L)
  expect_equal(ann$position, 1L)
})

test_that("qc_add_annotation without position has NULL position", {
  ann <- qc_add_annotation(proj, doc$id, "whole doc note")
  expect_true(is.na(ann$position))
})

test_that("qc_list_annotations returns all for a document", {
  doc2 <- qc_import_document(proj, content = "text", name = "ann-list-doc")

  qc_add_annotation(proj, doc2$id, "note 1")
  qc_add_annotation(proj, doc2$id, "note 2")
  anns <- qc_list_annotations(proj, source_id = doc2$id)
  expect_equal(nrow(anns), 2L)
})

test_that("qc_list_annotations coder filter works", {
  doc2 <- qc_import_document(proj, content = "text", name = "ann-coder-doc")

  qc_add_annotation(proj, doc2$id, "by alice", coder = "alice")
  qc_add_annotation(proj, doc2$id, "by bob",   coder = "bob")

  anns <- qc_list_annotations(proj, coder = "alice")
  expect_true(nrow(anns) >= 1L)
  expect_true(all(anns$coder == "alice"))
})

test_that("qc_update_annotation changes text", {
  doc2 <- qc_import_document(proj, content = "text", name = "ann-upd-doc")

  ann <- qc_add_annotation(proj, doc2$id, "original-ann-upd")
  qc_update_annotation(proj, ann$id, "revised-ann-upd")

  anns <- qc_list_annotations(proj, source_id = doc2$id)
  expect_equal(anns$annotation[[1L]], "revised-ann-upd")
})

test_that("qc_delete_annotation soft-deletes it", {
  doc2 <- qc_import_document(proj, content = "text", name = "ann-del-doc")

  ann <- qc_add_annotation(proj, doc2$id, "to delete")
  qc_delete_annotation(proj, ann$id)
  expect_false(ann$id %in% qc_list_annotations(proj, source_id = doc2$id)$id)
})

test_that("qc_list_annotations includes source_name", {
  doc2 <- qc_import_document(proj, content = "text", name = "ann-sname-mydoc")

  qc_add_annotation(proj, doc2$id, "note")
  anns <- qc_list_annotations(proj, source_id = doc2$id)
  expect_equal(anns$source_name[[1L]], "ann-sname-mydoc")
})
