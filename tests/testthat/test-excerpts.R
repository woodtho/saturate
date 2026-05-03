# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc <- qc_import_document(proj, content = "Hello world sample text", name = "exc-doc")

# ── Basic CRUD ────────────────────────────────────────────────────────────────

test_that("qc_add_excerpt returns correct seltext", {
  doc2 <- qc_import_document(proj, content = "Hello world", name = "exc-hello")
  ex <- qc_add_excerpt(proj, doc2$id, 1L, 5L, memo = "greeting")
  expect_equal(ex$seltext, "Hello")
  expect_equal(ex$memo, "greeting")
})

test_that("qc_list_excerpts returns all excerpts for a document", {
  doc2 <- qc_import_document(proj, content = "ABCDEFGHIJ", name = "exc-list")

  qc_add_excerpt(proj, doc2$id, 1L, 3L)
  qc_add_excerpt(proj, doc2$id, 5L, 8L)
  exs <- qc_list_excerpts(proj, doc2$id)
  expect_equal(nrow(exs), 2L)
  expect_equal(exs$selfirst, c(1L, 5L))
})

test_that("qc_list_excerpts with NULL source_id returns all excerpts", {
  d1 <- qc_import_document(proj, content = "aaaa", name = "exc-all-d1")
  d2 <- qc_import_document(proj, content = "bbbb", name = "exc-all-d2")

  ex1 <- qc_add_excerpt(proj, d1$id, 1L, 2L)
  ex2 <- qc_add_excerpt(proj, d2$id, 1L, 2L)
  exs <- qc_list_excerpts(proj)
  expect_true(ex1$id %in% exs$id)
  expect_true(ex2$id %in% exs$id)
})

test_that("qc_update_excerpt_memo changes memo", {
  doc2 <- qc_import_document(proj, content = "text here", name = "exc-upd")

  ex <- qc_add_excerpt(proj, doc2$id, 1L, 4L, memo = "old")
  qc_update_excerpt_memo(proj, ex$id, "updated memo")

  exs <- qc_list_excerpts(proj, doc2$id)
  expect_equal(exs$memo[[1L]], "updated memo")
})

test_that("qc_delete_excerpt soft-deletes it", {
  doc2 <- qc_import_document(proj, content = "text", name = "exc-del")

  ex <- qc_add_excerpt(proj, doc2$id, 1L, 4L)
  qc_delete_excerpt(proj, ex$id)
  expect_false(ex$id %in% qc_list_excerpts(proj, doc2$id)$id)
})

test_that("qc_add_excerpt errors when selfirst > selast", {
  doc2 <- qc_import_document(proj, content = "abcde", name = "exc-err")
  expect_error(qc_add_excerpt(proj, doc2$id, 5L, 2L), "selfirst")
})

test_that("qc_add_excerpt includes source_name in list", {
  doc2 <- qc_import_document(proj, content = "sample text", name = "exc-sname-mydoc")

  qc_add_excerpt(proj, doc2$id, 1L, 6L)
  exs <- qc_list_excerpts(proj, doc2$id)
  expect_equal(exs$source_name[[1L]], "exc-sname-mydoc")
})
