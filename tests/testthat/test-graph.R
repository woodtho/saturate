# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

d1 <- qc_import_document(proj, content = "Document one text here", name = "doc1")
d2 <- qc_import_document(proj, content = "Document two text here", name = "doc2")
c1 <- qc_add_code(proj, "topic_a")
c2 <- qc_add_code(proj, "topic_b")
qc_add_coding(proj, d1$id, c1$id, 1L, 8L)
qc_add_coding(proj, d1$id, c2$id, 10L, 16L)
qc_add_coding(proj, d2$id, c1$id, 1L, 8L)

# ── qc_document_graph similarity ─────────────────────────────────────────────

test_that("qc_document_graph similarity returns nodes and edges", {
  g <- qc_document_graph(proj, type = "similarity")
  expect_true(is.list(g))
  expect_true(all(c("nodes", "edges") %in% names(g)))
})

test_that("qc_document_graph similarity edge connects documents sharing a code", {
  g <- qc_document_graph(proj, type = "similarity")
  expect_equal(nrow(g$edges), 1L)
})

test_that("qc_document_graph similarity returns empty when no shared codes", {
  p2 <- make_test_project()
  withr::defer(qc_close(p2))
  pd1 <- qc_import_document(p2, content = "first text here",  name = "d1")
  pd2 <- qc_import_document(p2, content = "second text here", name = "d2")
  pc1 <- qc_add_code(p2, "only_d1")
  pc2 <- qc_add_code(p2, "only_d2")
  qc_add_coding(p2, pd1$id, pc1$id, 1L, 5L)
  qc_add_coding(p2, pd2$id, pc2$id, 1L, 6L)

  g <- qc_document_graph(p2, type = "similarity")
  expect_equal(nrow(g$edges), 0L)
})

# ── qc_document_graph bipartite ───────────────────────────────────────────────

test_that("qc_document_graph bipartite returns both doc and code nodes", {
  g <- qc_document_graph(proj, type = "bipartite")
  groups <- unique(g$nodes$group)
  expect_true("document" %in% groups)
  expect_true("code"     %in% groups)
})

# ── qc_document_graph cooccurrence ───────────────────────────────────────────

test_that("qc_document_graph cooccurrence returns code nodes", {
  g <- qc_document_graph(proj, type = "cooccurrence")
  expect_true(all(c("nodes", "edges") %in% names(g)))
  expect_true(nrow(g$nodes) >= 1L)
})

# ── qc_as_igraph ─────────────────────────────────────────────────────────────

test_that("qc_as_igraph returns an igraph object", {
  skip_if_not_installed("igraph")
  ig <- qc_as_igraph(proj)
  expect_s3_class(ig, "igraph")
})
