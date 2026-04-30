test_that("qc_add_coding snapshots seltext correctly", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "Hello world", name = "d")
  code <- qc_add_code(proj, "greet")

  cod <- qc_add_coding(proj, doc$id, code$id, selfirst = 1L, selast = 5L)
  expect_equal(cod$seltext, "Hello")
})

test_that("qc_list_codings ordered by selfirst", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "ABCDEFGH", name = "d")
  c1   <- qc_add_code(proj, "code1")
  c2   <- qc_add_code(proj, "code2")

  qc_add_coding(proj, doc$id, c2$id, 5L, 8L)
  qc_add_coding(proj, doc$id, c1$id, 1L, 3L)

  codings <- qc_list_codings(proj, doc$id)
  expect_equal(codings$selfirst, c(1L, 5L))
})

test_that("qc_list_codings includes code_name and code_color", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")
  code <- qc_add_code(proj, "theme", color = "#F00000")
  qc_add_coding(proj, doc$id, code$id, 1L, 4L)

  codings <- qc_list_codings(proj, doc$id)
  expect_equal(codings$code_name[[1L]],  "theme")
  expect_equal(codings$code_color[[1L]], "#F00000")
})

test_that("qc_delete_coding removes it from list", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")
  code <- qc_add_code(proj, "c1")
  cod  <- qc_add_coding(proj, doc$id, code$id, 1L, 4L)

  qc_delete_coding(proj, cod$id)
  expect_equal(nrow(qc_list_codings(proj, doc$id)), 0L)
})

test_that("qc_add_coding errors when selfirst < 1", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")
  code <- qc_add_code(proj, "c1")
  expect_error(qc_add_coding(proj, doc$id, code$id, 0L, 3L), "selfirst")
})

test_that("build_highlighted_html returns an htmltools tag", {
  doc   <- "Hello world this is a test."
  codings <- tibble::tibble(
    selfirst   = c(1L, 7L),
    selast     = c(5L, 11L),
    code_name  = c("A", "B"),
    code_color = c("#4E79A7", "#F28E2B"),
    id         = c(1L, 2L),
    memo       = c("", "")
  )
  html <- build_highlighted_html(doc, codings)
  expect_s3_class(html, "shiny.tag")
})
