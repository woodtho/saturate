make_coded_project <- function(.env = parent.frame()) {
  proj <- make_test_project(.env)
  doc1 <- qc_import_document(proj, content = "AAABBB CCCDDD EEEFFF", name = "d1")
  doc2 <- qc_import_document(proj, content = "GGGHHH IIIJJJ KKKLL",  name = "d2")
  c1   <- qc_add_code(proj, "code1")
  c2   <- qc_add_code(proj, "code2")
  qc_add_coding(proj, doc1$id, c1$id, 1L,  6L)
  qc_add_coding(proj, doc1$id, c2$id, 8L,  13L)
  qc_add_coding(proj, doc2$id, c1$id, 1L,  6L)
  list(proj = proj, d1 = doc1, d2 = doc2, c1 = c1, c2 = c2)
}

# ── qc_code_matrix ──────────────────────────────────────────────────────────────

test_that("qc_code_matrix by=document returns wide tibble with code columns", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  mat <- qc_code_matrix(proj, by = "document")
  expect_true("document_id"   %in% names(mat))
  expect_true("document_name" %in% names(mat))
  expect_true("code1" %in% names(mat))
  expect_true("code2" %in% names(mat))
})

test_that("qc_code_matrix by=document fills zero for absent codes", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  mat <- qc_code_matrix(proj, by = "document")
  d2_row <- mat[mat$document_name == "d2", ]
  expect_equal(d2_row$code2[[1L]], 0L)
})

test_that("qc_code_matrix by=document values=binary caps at 1", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))
  qc_add_coding(proj, x$d1$id, x$c1$id, 8L, 13L)

  mat <- qc_code_matrix(proj, by = "document", values = "binary")
  d1_row <- mat[mat$document_name == "d1", ]
  expect_equal(d1_row$code1[[1L]], 1L)
})

test_that("qc_code_matrix by=document values=chars returns character counts", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  mat <- qc_code_matrix(proj, by = "document", values = "chars")
  expect_true(is.numeric(mat$code1[[1L]]))
})

test_that("qc_code_matrix returns empty tibble when no codings", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  mat <- suppressWarnings(qc_code_matrix(proj))
  expect_equal(nrow(mat), 0L)
})

# ── qc_code_metrics ─────────────────────────────────────────────────────────────

test_that("qc_code_metrics returns prevalence column", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  met <- qc_code_metrics(proj)
  expect_true("prevalence" %in% names(met))
  expect_true(all(met$prevalence >= 0 & met$prevalence <= 100))
})

test_that("qc_code_metrics returns dispersion column", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  met <- qc_code_metrics(proj)
  expect_true("dispersion" %in% names(met))
})

test_that("qc_code_metrics returns empty tibble with warning when no codings", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  met <- suppressWarnings(qc_code_metrics(proj))
  expect_equal(nrow(met), 0L)
})

# ── qc_code_summary ─────────────────────────────────────────────────────────────

test_that("qc_code_summary n_segments matches manual count", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "AABBCC DDEEFF", name = "d")
  c1   <- qc_add_code(proj, "c1")
  qc_add_coding(proj, doc$id, c1$id, 1L, 6L)
  qc_add_coding(proj, doc$id, c1$id, 8L, 13L)

  summ <- qc_code_summary(proj)
  expect_equal(summ$n_segments[[1L]], 2L)
  expect_equal(summ$n_documents[[1L]], 1L)
})

# ── qc_saturation_curve ─────────────────────────────────────────────────────────

test_that("qc_saturation_curve cumulative_codes is non-decreasing", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  curve <- qc_saturation_curve(proj)
  expect_true(all(diff(curve$cumulative_codes) >= 0))
})

test_that("qc_saturation_curve returns empty tibble when no codings", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  curve <- qc_saturation_curve(proj)
  expect_equal(nrow(curve), 0L)
})

test_that("qc_saturation_curve order_by=first_coded also works", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  curve <- qc_saturation_curve(proj, order_by = "first_coded")
  expect_true("cumulative_codes" %in% names(curve))
})

# ── qc_code_cooccurrence ────────────────────────────────────────────────────────

test_that("qc_code_cooccurrence returns co-occurring pair", {
  x    <- make_coded_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  coocc <- qc_code_cooccurrence(proj, unit = "document")
  expect_true(nrow(coocc) >= 1L)
  expect_true("n" %in% names(coocc))
})

test_that("qc_code_cooccurrence unit=segment works", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "overlapping text here", name = "d")
  c1   <- qc_add_code(proj, "c1")
  c2   <- qc_add_code(proj, "c2")
  qc_add_coding(proj, doc$id, c1$id, 1L, 11L)
  qc_add_coding(proj, doc$id, c2$id, 5L, 15L)

  coocc <- qc_code_cooccurrence(proj, unit = "segment")
  expect_equal(nrow(coocc), 1L)
})
