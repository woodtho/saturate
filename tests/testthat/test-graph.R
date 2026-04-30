make_graph_project <- function(.env = parent.frame()) {
  proj <- make_test_project(.env)
  d1 <- qc_import_document(proj, content = "Document one text here", name = "doc1")
  d2 <- qc_import_document(proj, content = "Document two text here", name = "doc2")
  c1 <- qc_add_code(proj, "topic_a")
  c2 <- qc_add_code(proj, "topic_b")
  qc_add_coding(proj, d1$id, c1$id, 1L, 8L)
  qc_add_coding(proj, d1$id, c2$id, 10L, 16L)
  qc_add_coding(proj, d2$id, c1$id, 1L, 8L)
  list(proj = proj, d1 = d1, d2 = d2, c1 = c1, c2 = c2)
}

# ── qc_document_graph similarity ────────────────────────────────────────────────

test_that("qc_document_graph similarity returns nodes and edges", {
  x    <- make_graph_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  g <- qc_document_graph(proj, type = "similarity")
  expect_true(is.list(g))
  expect_true(all(c("nodes", "edges") %in% names(g)))
})

test_that("qc_document_graph similarity edge connects documents sharing a code", {
  x    <- make_graph_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  g <- qc_document_graph(proj, type = "similarity")
  expect_equal(nrow(g$edges), 1L)
})

test_that("qc_document_graph similarity returns empty when no shared codes", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  d1 <- qc_import_document(proj, content = "first text here", name = "d1")
  d2 <- qc_import_document(proj, content = "second text here", name = "d2")
  c1 <- qc_add_code(proj, "only_d1")
  c2 <- qc_add_code(proj, "only_d2")
  qc_add_coding(proj, d1$id, c1$id, 1L, 5L)
  qc_add_coding(proj, d2$id, c2$id, 1L, 6L)

  g <- qc_document_graph(proj, type = "similarity")
  expect_equal(nrow(g$edges), 0L)
})

# ── qc_document_graph bipartite ─────────────────────────────────────────────────

test_that("qc_document_graph bipartite returns both doc and code nodes", {
  x    <- make_graph_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  g <- qc_document_graph(proj, type = "bipartite")
  groups <- unique(g$nodes$group)
  expect_true("document" %in% groups)
  expect_true("code"     %in% groups)
})

# ── qc_document_graph cooccurrence ──────────────────────────────────────────────

test_that("qc_document_graph cooccurrence returns code nodes", {
  x    <- make_graph_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  g <- qc_document_graph(proj, type = "cooccurrence")
  expect_true(all(c("nodes", "edges") %in% names(g)))
  expect_true(nrow(g$nodes) >= 1L)
})

# ── qc_as_igraph ────────────────────────────────────────────────────────────────

test_that("qc_as_igraph returns an igraph object", {
  skip_if_not_installed("igraph")
  x    <- make_graph_project()
  proj <- x$proj
  on.exit(qc_close(proj))

  ig <- qc_as_igraph(proj)
  expect_s3_class(ig, "igraph")
})
