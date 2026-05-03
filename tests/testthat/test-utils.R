# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc <- qc_import_document(proj, content = "text here", name = "utils-doc")
c1  <- qc_add_code(proj, "utils-c1")
cod <- qc_add_coding(proj, doc$id, c1$id, 1L, 4L)

# ── qc_cb_palette ─────────────────────────────────────────────────────────────

test_that("qc_cb_palette returns n colours", {
  pal <- qc_cb_palette(3L)
  expect_length(pal, 3L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("qc_cb_palette n=0 returns empty character vector", {
  pal <- qc_cb_palette(0L)
  expect_length(pal, 0L)
})

test_that("qc_cb_palette cycles correctly for n=20", {
  pal <- qc_cb_palette(20L, type = "okabe_ito")
  expect_length(pal, 20L)
  expect_equal(pal[[9L]], pal[[1L]])
})

test_that("qc_cb_palette type=wong works", {
  pal <- qc_cb_palette(4L, type = "wong")
  expect_length(pal, 4L)
})

test_that("qc_cb_palette type=tol works", {
  pal <- qc_cb_palette(4L, type = "tol")
  expect_length(pal, 4L)
})

test_that("qc_cb_palette errors on negative n", {
  expect_error(qc_cb_palette(-1L))
})

# ── qc_set_code_key ───────────────────────────────────────────────────────────

test_that("qc_set_code_key persists key to the code", {
  ck <- qc_add_code(proj, "utils-my_theme")
  qc_set_code_key(proj, ck$id, "t01")
  codes <- qc_list_codes(proj)
  row <- codes[codes$id == ck$id, ]
  expect_equal(row$code_key[[1L]], "t01")
})

# ── qc_deprecate_code / qc_undeprecate_code ───────────────────────────────────

test_that("qc_deprecate_code marks code as deprecated", {
  ck <- qc_add_code(proj, "utils-old_code")
  qc_deprecate_code(proj, ck$id, reason = "no longer needed")
  codes <- qc_list_codes(proj)
  row <- codes[codes$id == ck$id, ]
  expect_equal(row$deprecated[[1L]], 1L)
})

test_that("qc_undeprecate_code restores deprecated = FALSE", {
  ck <- qc_add_code(proj, "utils-revived")
  qc_deprecate_code(proj, ck$id)
  qc_undeprecate_code(proj, ck$id)
  codes <- qc_list_codes(proj)
  row <- codes[codes$id == ck$id, ]
  expect_equal(row$deprecated[[1L]], 0L)
})

# ── qc_update_coding_memo ─────────────────────────────────────────────────────

test_that("qc_update_coding_memo persists memo text", {
  qc_update_coding_memo(proj, cod$id, "new memo text")
  codings <- qc_list_codings(proj, doc$id)
  row <- codings[codings$id == cod$id, ]
  expect_equal(row$memo[[1L]], "new memo text")
})

# ── qc_update_coding_confidence ───────────────────────────────────────────────

test_that("qc_update_coding_confidence persists confidence value", {
  qc_update_coding_confidence(proj, cod$id, 80L)
  codings <- qc_list_codings(proj, doc$id)
  row <- codings[codings$id == cod$id, ]
  expect_equal(row$confidence[[1L]], 80L)
})

# ── qc_merge_codings ──────────────────────────────────────────────────────────

test_that("qc_merge_codings merges two codings into one", {
  doc2 <- qc_import_document(proj, content = "ABCDEFGHIJ", name = "merge-cod-d")
  cm1  <- qc_add_code(proj, "merge-cod-code1")
  cd1  <- qc_add_coding(proj, doc2$id, cm1$id, 1L, 3L)
  cd2  <- qc_add_coding(proj, doc2$id, cm1$id, 6L, 9L)

  merged <- qc_merge_codings(proj, c(cd1$id, cd2$id))
  expect_equal(merged$selfirst, 1L)
  expect_equal(merged$selast,   9L)
  expect_equal(nrow(qc_list_codings(proj, doc2$id)), 1L)
})

# ── qc_split_coding ───────────────────────────────────────────────────────────

test_that("qc_split_coding produces two codings from one", {
  doc2 <- qc_import_document(proj, content = "ABCDEFGHIJ", name = "split-cod-d")
  cs1  <- qc_add_code(proj, "split-cod-code")
  cds  <- qc_add_coding(proj, doc2$id, cs1$id, 1L, 10L)

  qc_split_coding(proj, cds$id, split_at = 5L)
  codings <- qc_list_codings(proj, doc2$id)
  expect_equal(nrow(codings), 2L)
  expect_equal(codings$selfirst[[1L]], 1L)
  expect_equal(codings$selast[[1L]],  5L)
  expect_equal(codings$selfirst[[2L]], 6L)
  expect_equal(codings$selast[[2L]],  10L)
})

# ── qc_set_source_type ────────────────────────────────────────────────────────

test_that("qc_set_source_type persists source type", {
  doc2 <- qc_import_document(proj, content = "interview text", name = "stype-int1")
  qc_set_source_type(proj, doc2$id, "interview")
  d <- qc_get_document(proj, doc2$id)
  expect_equal(d$source_type, "interview")
})

# ── qc_export ─────────────────────────────────────────────────────────────────

test_that("qc_export writes a CSV file when format=csv", {
  skip_on_cran()
  out <- withr::local_tempfile(fileext = ".csv")
  qc_export(proj, out, format = "csv")
  expect_true(file.exists(out))
})
