# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc1 <- qc_import_document(proj, content = "AAABBB CCCDDD EEEFFF", name = "d1")
doc2 <- qc_import_document(proj, content = "GGGHHH IIIJJJ KKKLL",  name = "d2")
c1   <- qc_add_code(proj, "code1")
c2   <- qc_add_code(proj, "code2")
qc_add_coding(proj, doc1$id, c1$id, 1L,  6L)
qc_add_coding(proj, doc1$id, c2$id, 8L,  13L)
qc_add_coding(proj, doc2$id, c1$id, 1L,  6L)

# ── qc_code_matrix ────────────────────────────────────────────────────────────

test_that("qc_code_matrix by=document returns wide tibble with code columns", {
  mat <- qc_code_matrix(proj, by = "document")
  expect_true("document_id"   %in% names(mat))
  expect_true("document_name" %in% names(mat))
  expect_true("code1" %in% names(mat))
  expect_true("code2" %in% names(mat))
})

test_that("qc_code_matrix by=document fills zero for absent codes", {
  mat <- qc_code_matrix(proj, by = "document")
  d2_row <- mat[mat$document_name == "d2", ]
  expect_equal(d2_row$code2[[1L]], 0L)
})

test_that("qc_code_matrix by=document values=binary caps at 1", {
  qc_add_coding(proj, doc1$id, c1$id, 8L, 13L)

  mat <- qc_code_matrix(proj, by = "document", values = "binary")
  d1_row <- mat[mat$document_name == "d1", ]
  expect_equal(d1_row$code1[[1L]], 1L)
})

test_that("qc_code_matrix by=document values=chars returns character counts", {
  mat <- qc_code_matrix(proj, by = "document", values = "chars")
  expect_true(is.numeric(mat$code1[[1L]]))
})

test_that("qc_code_matrix returns empty tibble when no codings", {
  p2 <- make_test_project()
  withr::defer(qc_close(p2))
  mat <- suppressWarnings(qc_code_matrix(p2))
  expect_equal(nrow(mat), 0L)
})

# ── qc_code_metrics ───────────────────────────────────────────────────────────

test_that("qc_code_metrics returns prevalence column", {
  met <- qc_code_metrics(proj)
  expect_true("prevalence" %in% names(met))
  expect_true(all(met$prevalence >= 0 & met$prevalence <= 100))
})

test_that("qc_code_metrics returns dispersion column", {
  met <- qc_code_metrics(proj)
  expect_true("dispersion" %in% names(met))
})

test_that("qc_code_metrics returns empty tibble with warning when no codings", {
  p2 <- make_test_project()
  withr::defer(qc_close(p2))
  met <- suppressWarnings(qc_code_metrics(p2))
  expect_equal(nrow(met), 0L)
})

# ── qc_code_summary ───────────────────────────────────────────────────────────

test_that("qc_code_summary n_segments matches manual count", {
  doc3 <- qc_import_document(proj, content = "AABBCC DDEEFF", name = "summ-d")
  c3   <- qc_add_code(proj, "summ-c1")
  qc_add_coding(proj, doc3$id, c3$id, 1L, 6L)
  qc_add_coding(proj, doc3$id, c3$id, 8L, 13L)

  summ <- qc_code_summary(proj)
  summ_row <- summ[summ$code_name == "summ-c1", ]
  expect_equal(summ_row$n_segments[[1L]], 2L)
  expect_equal(summ_row$n_documents[[1L]], 1L)
})

# ── qc_saturation_curve ───────────────────────────────────────────────────────

test_that("qc_saturation_curve cumulative_codes is non-decreasing", {
  curve <- qc_saturation_curve(proj)
  expect_true(all(diff(curve$cumulative_codes) >= 0))
})

test_that("qc_saturation_curve returns empty tibble when no codings", {
  p2 <- make_test_project()
  withr::defer(qc_close(p2))
  curve <- qc_saturation_curve(p2)
  expect_equal(nrow(curve), 0L)
})

test_that("qc_saturation_curve order_by=first_coded also works", {
  curve <- qc_saturation_curve(proj, order_by = "first_coded")
  expect_true("cumulative_codes" %in% names(curve))
})

# ── qc_code_cooccurrence ──────────────────────────────────────────────────────

test_that("qc_code_cooccurrence returns co-occurring pair", {
  coocc <- qc_code_cooccurrence(proj, unit = "document")
  expect_true(nrow(coocc) >= 1L)
  expect_true("n" %in% names(coocc))
})

test_that("qc_code_cooccurrence unit=segment works", {
  doc3 <- qc_import_document(proj, content = "overlapping text here", name = "coocc-d")
  c3   <- qc_add_code(proj, "coocc-c1")
  c4   <- qc_add_code(proj, "coocc-c2")
  qc_add_coding(proj, doc3$id, c3$id, 1L, 11L)
  qc_add_coding(proj, doc3$id, c4$id, 5L, 15L)

  coocc <- qc_code_cooccurrence(proj, unit = "segment")
  expect_true(nrow(coocc) >= 1L)
})
