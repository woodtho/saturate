make_two_coder_project <- function(.env = parent.frame()) {
  proj <- make_test_project(.env)
  doc1 <- qc_import_document(proj, content = "text one", name = "d1")
  doc2 <- qc_import_document(proj, content = "text two", name = "d2")
  c1   <- qc_add_code(proj, "theme")

  # coder_a codes both docs; coder_b codes only doc1
  qc_add_coding(proj, doc1$id, c1$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, doc2$id, c1$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, doc1$id, c1$id, 1L, 4L, coder = "coder_b")

  list(proj = proj, c1 = c1)
}

# ── qc_agreement ────────────────────────────────────────────────────────────────

test_that("qc_agreement returns one-row tibble with kappa", {
  x    <- make_two_coder_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  ag <- qc_agreement(proj, x$c1$id, "coder_a", "coder_b")
  expect_equal(nrow(ag), 1L)
  expect_true("kappa" %in% names(ag))
  expect_true(is.numeric(ag$kappa))
})

test_that("qc_agreement n11 equals both coders applying code to same doc", {
  x    <- make_two_coder_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  ag <- qc_agreement(proj, x$c1$id, "coder_a", "coder_b")
  expect_equal(ag$n11[[1L]], 1L)
})

test_that("qc_agreement n10 equals coder_a only (doc2)", {
  x    <- make_two_coder_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  ag <- qc_agreement(proj, x$c1$id, "coder_a", "coder_b")
  expect_equal(ag$n10[[1L]], 1L)
})

test_that("qc_agreement errors on nonexistent code", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  expect_error(qc_agreement(proj, 9999L, "a", "b"))
})

test_that("qc_agreement errors when no codings for coders", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "c")
  expect_error(qc_agreement(proj, c1$id, "nobody", "nobody_else"))
})

# ── qc_krippendorff ─────────────────────────────────────────────────────────────

test_that("qc_krippendorff returns alpha in tibble", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc1 <- qc_import_document(proj, content = "text one", name = "d1")
  doc2 <- qc_import_document(proj, content = "text two", name = "d2")
  c1   <- qc_add_code(proj, "theme")
  qc_add_coding(proj, doc1$id, c1$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, doc2$id, c1$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, doc1$id, c1$id, 1L, 4L, coder = "coder_b")
  qc_add_coding(proj, doc2$id, c1$id, 1L, 4L, coder = "coder_b")

  kri <- qc_krippendorff(proj, c1$id, coders = c("coder_a", "coder_b"))
  expect_true("alpha" %in% names(kri))
  expect_equal(nrow(kri), 1L)
})

test_that("qc_krippendorff errors with fewer than 2 coders", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "c")
  expect_error(qc_krippendorff(proj, c1$id, coders = c("coder_a")))
})

# ── qc_agreement_matrix ─────────────────────────────────────────────────────────

test_that("qc_agreement_matrix returns one row per coder pair per code", {
  x    <- make_two_coder_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  mat <- qc_agreement_matrix(proj)
  expect_true(nrow(mat) >= 1L)
  expect_true("kappa" %in% names(mat))
})
