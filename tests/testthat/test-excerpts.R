test_that("qc_add_excerpt returns correct seltext", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "Hello world", name = "d")

  ex <- qc_add_excerpt(proj, doc$id, 1L, 5L, memo = "greeting")
  expect_equal(ex$seltext, "Hello")
  expect_equal(ex$memo, "greeting")
})

test_that("qc_list_excerpts returns all excerpts for a document", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "ABCDEFGHIJ", name = "d")

  qc_add_excerpt(proj, doc$id, 1L, 3L)
  qc_add_excerpt(proj, doc$id, 5L, 8L)
  exs <- qc_list_excerpts(proj, doc$id)
  expect_equal(nrow(exs), 2L)
  expect_equal(exs$selfirst, c(1L, 5L))
})

test_that("qc_list_excerpts with NULL source_id returns all excerpts", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  d1 <- qc_import_document(proj, content = "aaaa", name = "d1")
  d2 <- qc_import_document(proj, content = "bbbb", name = "d2")

  qc_add_excerpt(proj, d1$id, 1L, 2L)
  qc_add_excerpt(proj, d2$id, 1L, 2L)
  exs <- qc_list_excerpts(proj)
  expect_equal(nrow(exs), 2L)
})

test_that("qc_update_excerpt_memo changes memo", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text here", name = "d")

  ex <- qc_add_excerpt(proj, doc$id, 1L, 4L, memo = "old")
  qc_update_excerpt_memo(proj, ex$id, "updated memo")

  exs <- qc_list_excerpts(proj, doc$id)
  expect_equal(exs$memo[[1L]], "updated memo")
})

test_that("qc_delete_excerpt soft-deletes it", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")

  ex <- qc_add_excerpt(proj, doc$id, 1L, 4L)
  qc_delete_excerpt(proj, ex$id)
  expect_equal(nrow(qc_list_excerpts(proj, doc$id)), 0L)
})

test_that("qc_add_excerpt errors when selfirst > selast", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "abcde", name = "d")

  expect_error(qc_add_excerpt(proj, doc$id, 5L, 2L), "selfirst")
})

test_that("qc_add_excerpt includes source_name in list", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "sample text", name = "mydoc")

  qc_add_excerpt(proj, doc$id, 1L, 6L)
  exs <- qc_list_excerpts(proj)
  expect_equal(exs$source_name[[1L]], "mydoc")
})
