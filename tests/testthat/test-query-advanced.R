# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

d1 <- qc_import_document(proj, content = "The quick brown fox", name = "d1")
d2 <- qc_import_document(proj, content = "A lazy dog slept",   name = "d2")
c1 <- qc_add_code(proj, "animal")
c2 <- qc_add_code(proj, "action")
qc_add_coding(proj, d1$id, c1$id, 11L, 19L, coder = "alice")
qc_add_coding(proj, d1$id, c2$id, 1L,  3L,  coder = "bob")
qc_add_coding(proj, d2$id, c1$id, 3L,  11L, coder = "alice")

# ── must_have / must_not filters ─────────────────────────────────────────────

test_that("must_have returns only docs with all specified codes", {
  segs <- qc_get_coded_segments(proj, must_have = c(c1$id, c2$id))
  expect_true(all(segs$source_name == "d1"))
})

test_that("must_not excludes docs with the excluded code", {
  segs <- qc_get_coded_segments(proj, must_not = c2$id)
  expect_false(any(segs$source_name == "d1"))
})

# ── coder filter ─────────────────────────────────────────────────────────────

test_that("coder filter restricts to one coder's codings", {
  segs <- qc_get_coded_segments(proj, coder = "bob")
  expect_true(all(segs$coder == "bob"))
})

# ── case_ids filter ───────────────────────────────────────────────────────────

test_that("case_ids filter returns segments from linked documents only", {
  case <- qc_add_case(proj, "respondent1")
  qc_link_case_source(proj, case$id, d1$id)

  segs <- qc_get_coded_segments(proj, case_ids = case$id)
  expect_true(all(segs$source_name == "d1"))
})

# ── limit / offset ────────────────────────────────────────────────────────────

test_that("limit restricts number of rows returned", {
  segs <- qc_get_coded_segments(proj, limit = 2L)
  expect_equal(nrow(segs), 2L)
})

test_that("offset skips rows", {
  all_segs <- qc_get_coded_segments(proj)
  page2    <- qc_get_coded_segments(proj, limit = 10L, offset = 1L)
  expect_equal(nrow(page2), nrow(all_segs) - 1L)
})

# ── qc_code_by_unit ───────────────────────────────────────────────────────────

test_that("qc_code_by_unit paragraph creates one coding per paragraph", {
  doc3 <- qc_import_document(proj,
    content = "Para one text here.\n\nPara two text here.\n\nPara three.",
    name = "multi-para")
  c3 <- qc_add_code(proj, "para_code")

  result <- qc_code_by_unit(proj, doc3$id, c3$id, unit = "paragraph")
  expect_equal(nrow(result), 3L)
  expect_equal(result$unit_n, 1L:3L)
})

test_that("qc_code_by_unit with unit_indices codes only selected units", {
  doc3 <- qc_import_document(proj,
    content = "First paragraph here.\n\nSecond paragraph here.",
    name = "two-para")
  c3 <- qc_add_code(proj, "selected")

  result <- qc_code_by_unit(proj, doc3$id, c3$id, unit = "paragraph",
                             unit_indices = 1L)
  expect_equal(nrow(result), 1L)
})

test_that("qc_code_by_unit errors on out-of-range unit_indices", {
  doc3 <- qc_import_document(proj, content = "Only one paragraph.", name = "one-para")
  c3 <- qc_add_code(proj, "unit-err-code")

  expect_error(qc_code_by_unit(proj, doc3$id, c3$id, unit = "paragraph",
                                unit_indices = 99L))
})

# ── qc_search_documents ───────────────────────────────────────────────────────

test_that("qc_search_documents finds matching documents", {
  qc_import_document(proj, content = "The unicorn is a mythical creature", name = "search-d1")
  qc_import_document(proj, content = "Dragons breathe fire", name = "search-d2")

  results <- qc_search_documents(proj, "unicorn")
  expect_true(nrow(results) >= 1L)
  expect_true("search-d1" %in% results$source_name)
})

test_that("qc_search_documents returns empty tibble for no match", {
  results <- qc_search_documents(proj, "xyznotfound99999")
  expect_equal(nrow(results), 0L)
})

# ── qc_triangulate ────────────────────────────────────────────────────────────

test_that("qc_triangulate returns wide tibble with source_type columns", {
  di1 <- qc_import_document(proj, content = "interview text here",
                             name = "tri-i1", source_type = "interview")
  ds1 <- qc_import_document(proj, content = "survey text here",
                             name = "tri-s1", source_type = "survey")
  ct1 <- qc_add_code(proj, "tri-support")
  qc_add_coding(proj, di1$id, ct1$id, 1L, 9L)
  qc_add_coding(proj, ds1$id, ct1$id, 1L, 6L)

  tri <- qc_triangulate(proj)
  expect_true("code_name" %in% names(tri))
  expect_true("interview" %in% names(tri))
  expect_true("survey"    %in% names(tri))
  expect_true("total"     %in% names(tri))
})

test_that("qc_triangulate returns empty tibble when no codings", {
  p2 <- make_test_project()
  withr::defer(qc_close(p2))
  expect_equal(nrow(qc_triangulate(p2)), 0L)
})

test_that("qc_triangulate metric=documents counts distinct docs", {
  dt1 <- qc_import_document(proj, content = "text A", name = "tri-cnt-d1",
                             source_type = "interview")
  ct1 <- qc_add_code(proj, "tri-cnt-c1")
  qc_add_coding(proj, dt1$id, ct1$id, 1L, 4L)
  qc_add_coding(proj, dt1$id, ct1$id, 6L, 6L)

  tri <- qc_triangulate(proj, metric = "documents")
  row <- tri[tri$code_name == "tri-cnt-c1", ]
  expect_equal(row$interview[[1L]], 1L)
})

# ── qc_cross_tabulate ─────────────────────────────────────────────────────────

test_that("qc_cross_tabulate returns wide tibble by case attribute", {
  doc3 <- qc_import_document(proj, content = "some text content", name = "xtab-d")
  case <- qc_add_case(proj, "xtab-p1")
  qc_link_case_source(proj, case$id, doc3$id)
  qc_set_case_attribute(proj, case$id, "region", "north")
  ct1  <- qc_add_code(proj, "xtab-theme")
  qc_add_coding(proj, doc3$id, ct1$id, 1L, 4L)

  tab <- qc_cross_tabulate(proj, attribute = "region")
  expect_true("attribute_value" %in% names(tab))
  expect_true("xtab-theme" %in% names(tab))
})
