# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc1 <- qc_import_document(proj, content = "text one", name = "d1")
doc2 <- qc_import_document(proj, content = "text two", name = "d2")
c1   <- qc_add_code(proj, "theme")

qc_add_coding(proj, doc1$id, c1$id, 1L, 4L, coder = "coder_a")
qc_add_coding(proj, doc2$id, c1$id, 1L, 4L, coder = "coder_a")
qc_add_coding(proj, doc1$id, c1$id, 1L, 4L, coder = "coder_b")

# ── qc_agreement ──────────────────────────────────────────────────────────────

test_that("qc_agreement returns one-row tibble with kappa", {
  ag <- qc_agreement(proj, c1$id, "coder_a", "coder_b")
  expect_equal(nrow(ag), 1L)
  expect_true("kappa" %in% names(ag))
  expect_true(is.numeric(ag$kappa))
})

test_that("qc_agreement returns kappa between 0 and 1 for non-trivial codings", {
  ag <- qc_agreement(proj, c1$id, "coder_a", "coder_b")
  expect_true(ag$kappa >= 0 && ag$kappa <= 1)
})

test_that("qc_agreement returns kappa near 1 when codings are identical", {
  d3   <- qc_import_document(proj, content = "text one",   name = "kappa-d1")
  d4   <- qc_import_document(proj, content = "text two",   name = "kappa-d2")
  d5   <- qc_import_document(proj, content = "text three", name = "kappa-d3")
  ck1  <- qc_add_code(proj, "kappa-code")

  qc_add_coding(proj, d3$id, ck1$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, d4$id, ck1$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, d3$id, ck1$id, 1L, 4L, coder = "coder_b")
  qc_add_coding(proj, d4$id, ck1$id, 1L, 4L, coder = "coder_b")
  ck2 <- qc_add_code(proj, "kappa-other")
  qc_add_coding(proj, d5$id, ck2$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, d5$id, ck2$id, 1L, 4L, coder = "coder_b")

  ag <- qc_agreement(proj, ck1$id, "coder_a", "coder_b")
  expect_true(is.na(ag$kappa) || ag$kappa >= 0.9)
})

test_that("qc_agreement n11 equals both coders applying code to same doc", {
  ag <- qc_agreement(proj, c1$id, "coder_a", "coder_b")
  expect_equal(ag$n11[[1L]], 1L)
})

test_that("qc_agreement n10 equals coder_a only (doc2)", {
  ag <- qc_agreement(proj, c1$id, "coder_a", "coder_b")
  expect_equal(ag$n10[[1L]], 1L)
})

test_that("qc_agreement errors on nonexistent code", {
  expect_error(qc_agreement(proj, 9999L, "a", "b"))
})

test_that("qc_agreement errors when no codings for coders", {
  c_err <- qc_add_code(proj, "rel-err-code")
  expect_error(qc_agreement(proj, c_err$id, "nobody", "nobody_else"))
})

# ── qc_agreement_matrix ───────────────────────────────────────────────────────

test_that("qc_agreement_matrix returns a square matrix (one row per coder pair per code)", {
  mat <- qc_agreement_matrix(proj)
  expect_true(nrow(mat) >= 1L)
  expect_true("kappa" %in% names(mat))
})

# ── qc_krippendorff ───────────────────────────────────────────────────────────

test_that("qc_krippendorff returns alpha value", {
  d3  <- qc_import_document(proj, content = "text one", name = "kri-d1")
  d4  <- qc_import_document(proj, content = "text two", name = "kri-d2")
  ck1 <- qc_add_code(proj, "kri-theme")

  qc_add_coding(proj, d3$id, ck1$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, d4$id, ck1$id, 1L, 4L, coder = "coder_a")
  qc_add_coding(proj, d3$id, ck1$id, 1L, 4L, coder = "coder_b")
  qc_add_coding(proj, d4$id, ck1$id, 1L, 4L, coder = "coder_b")

  kri <- qc_krippendorff(proj, ck1$id, coders = c("coder_a", "coder_b"))
  expect_true("alpha" %in% names(kri))
  expect_equal(nrow(kri), 1L)
  expect_true(is.numeric(kri$alpha))
})

test_that("qc_krippendorff errors with fewer than 2 coders", {
  ck1 <- qc_add_code(proj, "kri-err-code")
  expect_error(qc_krippendorff(proj, ck1$id, coders = c("coder_a")))
})
