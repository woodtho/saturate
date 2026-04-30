make_query_project <- function(.env = parent.frame()) {
  proj <- make_test_project(.env)
  d1   <- qc_import_document(proj, content = "The quick brown fox", name = "d1")
  d2   <- qc_import_document(proj, content = "A lazy dog slept",   name = "d2")
  c1   <- qc_add_code(proj, "animal")
  c2   <- qc_add_code(proj, "action")
  qc_add_coding(proj, d1$id, c1$id, 11L, 19L, coder = "alice")
  qc_add_coding(proj, d1$id, c2$id, 1L,  3L,  coder = "bob")
  qc_add_coding(proj, d2$id, c1$id, 3L,  11L, coder = "alice")
  list(proj = proj, d1 = d1, d2 = d2, c1 = c1, c2 = c2)
}

# ── must_have / must_not filters ─────────────────────────────────────────────

test_that("must_have returns only docs with all specified codes", {
  x    <- make_query_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  segs <- qc_get_coded_segments(proj, must_have = c(x$c1$id, x$c2$id))
  expect_true(all(segs$source_name == "d1"))
})

test_that("must_not excludes docs with the excluded code", {
  x    <- make_query_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  segs <- qc_get_coded_segments(proj, must_not = x$c2$id)
  expect_false(any(segs$source_name == "d1"))
})

# ── coder filter ─────────────────────────────────────────────────────────────

test_that("coder filter restricts to one coder's codings", {
  x    <- make_query_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  segs <- qc_get_coded_segments(proj, coder = "bob")
  expect_true(all(segs$coder == "bob"))
})

# ── case_ids filter ──────────────────────────────────────────────────────────

test_that("case_ids filter returns segments from linked documents only", {
  x    <- make_query_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "respondent1")
  qc_link_case_source(proj, case$id, x$d1$id)

  segs <- qc_get_coded_segments(proj, case_ids = case$id)
  expect_true(all(segs$source_name == "d1"))
})

# ── limit / offset ───────────────────────────────────────────────────────────

test_that("limit restricts number of rows returned", {
  x    <- make_query_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  segs <- qc_get_coded_segments(proj, limit = 2L)
  expect_equal(nrow(segs), 2L)
})

test_that("offset skips rows", {
  x    <- make_query_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  all_segs  <- qc_get_coded_segments(proj)
  page2     <- qc_get_coded_segments(proj, limit = 10L, offset = 1L)
  expect_equal(nrow(page2), nrow(all_segs) - 1L)
})

# ── qc_code_by_unit ──────────────────────────────────────────────────────────

test_that("qc_code_by_unit paragraph creates one coding per paragraph", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj,
    content = "Para one text here.\n\nPara two text here.\n\nPara three.",
    name = "multi-para")
  c1   <- qc_add_code(proj, "para_code")

  result <- qc_code_by_unit(proj, doc$id, c1$id, unit = "paragraph")
  expect_equal(nrow(result), 3L)
  expect_equal(result$unit_n, 1L:3L)
})

test_that("qc_code_by_unit with unit_indices codes only selected units", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj,
    content = "First paragraph here.\n\nSecond paragraph here.",
    name = "two-para")
  c1   <- qc_add_code(proj, "selected")

  result <- qc_code_by_unit(proj, doc$id, c1$id, unit = "paragraph",
                             unit_indices = 1L)
  expect_equal(nrow(result), 1L)
})

test_that("qc_code_by_unit errors on out-of-range unit_indices", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj,
    content = "Only one paragraph.", name = "one-para")
  c1   <- qc_add_code(proj, "code")

  expect_error(qc_code_by_unit(proj, doc$id, c1$id, unit = "paragraph",
                                unit_indices = 99L))
})

# ── qc_search_documents ──────────────────────────────────────────────────────

test_that("qc_search_documents finds matching documents", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_import_document(proj, content = "The unicorn is a mythical creature", name = "d1")
  qc_import_document(proj, content = "Dragons breathe fire", name = "d2")

  results <- qc_search_documents(proj, "unicorn")
  expect_equal(nrow(results), 1L)
  expect_equal(results$source_name[[1L]], "d1")
})

test_that("qc_search_documents returns empty tibble for no match", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_import_document(proj, content = "Some text here", name = "d1")

  results <- qc_search_documents(proj, "xyznotfound")
  expect_equal(nrow(results), 0L)
})

# ── qc_triangulate ───────────────────────────────────────────────────────────

test_that("qc_triangulate returns wide tibble with source_type columns", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  d1 <- qc_import_document(proj, content = "interview text here",
                            name = "i1", source_type = "interview")
  d2 <- qc_import_document(proj, content = "survey text here",
                            name = "s1", source_type = "survey")
  c1 <- qc_add_code(proj, "support")
  qc_add_coding(proj, d1$id, c1$id, 1L, 9L)
  qc_add_coding(proj, d2$id, c1$id, 1L, 6L)

  tri <- qc_triangulate(proj)
  expect_true("code_name"  %in% names(tri))
  expect_true("interview"  %in% names(tri))
  expect_true("survey"     %in% names(tri))
  expect_true("total"      %in% names(tri))
})

test_that("qc_triangulate returns empty tibble when no codings", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  expect_equal(nrow(qc_triangulate(proj)), 0L)
})

test_that("qc_triangulate metric=documents counts distinct docs", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  d1 <- qc_import_document(proj, content = "text A", name = "d1",
                            source_type = "interview")
  c1 <- qc_add_code(proj, "c1")
  qc_add_coding(proj, d1$id, c1$id, 1L, 4L)
  qc_add_coding(proj, d1$id, c1$id, 6L, 6L)

  tri <- qc_triangulate(proj, metric = "documents")
  expect_equal(tri$interview[[1L]], 1L)
})

# ── qc_cross_tabulate ────────────────────────────────────────────────────────

test_that("qc_cross_tabulate returns wide tibble by case attribute", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "some text content", name = "d")
  case <- qc_add_case(proj, "p1")
  qc_link_case_source(proj, case$id, doc$id)
  qc_set_case_attribute(proj, case$id, "region", "north")
  c1   <- qc_add_code(proj, "theme")
  qc_add_coding(proj, doc$id, c1$id, 1L, 4L)

  tab <- qc_cross_tabulate(proj, attribute = "region")
  expect_true("attribute_value" %in% names(tab))
  expect_true("theme" %in% names(tab))
})
