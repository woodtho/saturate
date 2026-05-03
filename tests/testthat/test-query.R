# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc1 <- qc_import_document(proj, content = "Hello world", name = "d1")
doc2 <- qc_import_document(proj, content = "Goodbye moon", name = "d2")
c1   <- qc_add_code(proj, "code1")
c2   <- qc_add_code(proj, "code2")
qc_add_coding(proj, doc1$id, c1$id, 1L, 5L)
qc_add_coding(proj, doc2$id, c2$id, 1L, 7L)

# ── qc_get_coded_segments ─────────────────────────────────────────────────────

test_that("qc_get_coded_segments returns all active codings", {
  segs <- qc_get_coded_segments(proj)
  expect_true(nrow(segs) >= 2L)
  expect_true("seltext"    %in% names(segs))
  expect_true("code_color" %in% names(segs))
})

test_that("code_ids filter works", {
  doc3  <- qc_import_document(proj, content = "Hello world", name = "qry-filter-d")
  keep  <- qc_add_code(proj, "qry-keep")
  drop  <- qc_add_code(proj, "qry-drop")
  qc_add_coding(proj, doc3$id, keep$id, 1L, 5L)
  qc_add_coding(proj, doc3$id, drop$id, 7L, 11L)

  segs <- qc_get_coded_segments(proj, code_ids = keep$id)
  expect_true(all(segs$code_name[segs$source_id == doc3$id] == "qry-keep"))
})

test_that("source_ids filter works", {
  d3 <- qc_import_document(proj, content = "doc one text", name = "qry-src-d1")
  d4 <- qc_import_document(proj, content = "doc two text", name = "qry-src-d2")
  cx <- qc_add_code(proj, "qry-src-c1")
  qc_add_coding(proj, d3$id, cx$id, 1L, 3L)
  qc_add_coding(proj, d4$id, cx$id, 1L, 3L)

  segs <- qc_get_coded_segments(proj, source_ids = d3$id)
  expect_true(all(segs$source_id[segs$source_name %in% c("qry-src-d1", "qry-src-d2")] == d3$id))
})

test_that("category_ids filter returns only codes in that category", {
  doc3 <- qc_import_document(proj, content = "text text text", name = "qry-cat-d")
  ci   <- qc_add_code(proj, "qry-incat")
  co   <- qc_add_code(proj, "qry-outcat")
  cat  <- qc_add_category(proj, "qry-mycat")
  qc_link_code_category(proj, ci$id, cat$id)
  qc_add_coding(proj, doc3$id, ci$id, 1L, 4L)
  qc_add_coding(proj, doc3$id, co$id, 6L, 9L)

  segs <- qc_get_coded_segments(proj, category_ids = cat$id)
  expect_true(all(segs$code_name[segs$source_id == doc3$id] == "qry-incat"))
})

# ── qc_code_summary ───────────────────────────────────────────────────────────

test_that("qc_code_summary counts match manual count", {
  doc3 <- qc_import_document(proj, content = "ABCDE FGHIJ", name = "summ-d")
  cs   <- qc_add_code(proj, "summ-c1")
  qc_add_coding(proj, doc3$id, cs$id, 1L, 5L)
  qc_add_coding(proj, doc3$id, cs$id, 7L, 11L)

  summ <- qc_code_summary(proj)
  summ_row <- summ[summ$code_name == "summ-c1", ]
  expect_equal(summ_row$n_segments[[1L]],  2L)
  expect_equal(summ_row$n_documents[[1L]], 1L)
})

# ── qc_export ─────────────────────────────────────────────────────────────────

test_that("qc_export writes a CSV file", {
  skip_on_cran()
  out <- withr::local_tempfile(fileext = ".csv")
  qc_export(proj, out, format = "csv")
  expect_true(file.exists(out))
  df <- utils::read.csv(out)
  expect_true(nrow(df) >= 1L)
})
