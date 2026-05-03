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
  proj <- make_test_project()
  on.exit(qc_close(proj))

  c1 <- qc_add_code(proj, "my_theme")
  qc_set_code_key(proj, c1$id, "t01")
  codes <- qc_list_codes(proj)
  expect_equal(codes$code_key[[1L]], "t01")
})

# ── qc_deprecate_code / qc_undeprecate_code ───────────────────────────────────

test_that("qc_deprecate_code marks code as deprecated", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  c1 <- qc_add_code(proj, "old_code")
  qc_deprecate_code(proj, c1$id, reason = "no longer needed")
  codes <- qc_list_codes(proj)
  expect_equal(codes$deprecated[[1L]], 1L)
})

test_that("qc_undeprecate_code restores deprecated = FALSE", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  c1 <- qc_add_code(proj, "revived")
  qc_deprecate_code(proj, c1$id)
  qc_undeprecate_code(proj, c1$id)
  codes <- qc_list_codes(proj)
  expect_equal(codes$deprecated[[1L]], 0L)
})

# ── qc_update_coding_memo ─────────────────────────────────────────────────────

test_that("qc_update_coding_memo persists memo text", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  doc <- qc_import_document(proj, content = "text here", name = "d")
  c1  <- qc_add_code(proj, "c1")
  cod <- qc_add_coding(proj, doc$id, c1$id, 1L, 4L)

  qc_update_coding_memo(proj, cod$id, "new memo text")
  codings <- qc_list_codings(proj, doc$id)
  expect_equal(codings$memo[[1L]], "new memo text")
})

# ── qc_update_coding_confidence ───────────────────────────────────────────────

test_that("qc_update_coding_confidence persists confidence value", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  doc <- qc_import_document(proj, content = "text here", name = "d")
  c1  <- qc_add_code(proj, "c1")
  cod <- qc_add_coding(proj, doc$id, c1$id, 1L, 4L)

  qc_update_coding_confidence(proj, cod$id, 80L)
  codings <- qc_list_codings(proj, doc$id)
  expect_equal(codings$confidence[[1L]], 80L)
})

# ── qc_merge_codings ──────────────────────────────────────────────────────────

test_that("qc_merge_codings merges two codings into one", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  doc  <- qc_import_document(proj, content = "ABCDEFGHIJ", name = "d")
  c1   <- qc_add_code(proj, "code1")
  cod1 <- qc_add_coding(proj, doc$id, c1$id, 1L, 3L)
  cod2 <- qc_add_coding(proj, doc$id, c1$id, 6L, 9L)

  merged <- qc_merge_codings(proj, c(cod1$id, cod2$id))
  expect_equal(merged$selfirst, 1L)
  expect_equal(merged$selast,   9L)
  expect_equal(nrow(qc_list_codings(proj, doc$id)), 1L)
})

# ── qc_split_coding ───────────────────────────────────────────────────────────

test_that("qc_split_coding produces two codings from one", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  doc <- qc_import_document(proj, content = "ABCDEFGHIJ", name = "d")
  c1  <- qc_add_code(proj, "code")
  cod <- qc_add_coding(proj, doc$id, c1$id, 1L, 10L)

  qc_split_coding(proj, cod$id, split_at = 5L)
  codings <- qc_list_codings(proj, doc$id)
  expect_equal(nrow(codings), 2L)
  expect_equal(codings$selfirst[[1L]], 1L)
  expect_equal(codings$selast[[1L]],  5L)
  expect_equal(codings$selfirst[[2L]], 6L)
  expect_equal(codings$selast[[2L]],  10L)
})

# ── qc_set_source_type ────────────────────────────────────────────────────────

test_that("qc_set_source_type persists source type", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  doc <- qc_import_document(proj, content = "interview text", name = "int1")
  qc_set_source_type(proj, doc$id, "interview")
  d <- qc_get_document(proj, doc$id)
  expect_equal(d$source_type, "interview")
})

# ── qc_export ─────────────────────────────────────────────────────────────────

test_that("qc_export writes a CSV file when format=csv", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))

  doc <- qc_import_document(proj, content = "text here", name = "d")
  c1  <- qc_add_code(proj, "c1")
  qc_add_coding(proj, doc$id, c1$id, 1L, 4L)

  out <- withr::local_tempfile(fileext = ".csv")
  qc_export(proj, out, format = "csv")
  expect_true(file.exists(out))
})
