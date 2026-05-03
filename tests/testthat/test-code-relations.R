# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

c1 <- qc_add_code(proj, "broader")
c2 <- qc_add_code(proj, "narrower")

# ── Basic CRUD ────────────────────────────────────────────────────────────────

test_that("qc_add_code_relation returns correct columns", {
  rel <- qc_add_code_relation(proj, c1$id, c2$id, "broader_than",
                               note = "c1 subsumes c2")
  expect_equal(rel$relation_type, "broader_than")
  expect_equal(rel$name_1, "broader")
  expect_equal(rel$name_2, "narrower")
  expect_equal(rel$note, "c1 subsumes c2")
})

test_that("qc_list_code_relations returns all relations", {
  c3 <- qc_add_code(proj, "rel-a")
  c4 <- qc_add_code(proj, "rel-b")
  c5 <- qc_add_code(proj, "rel-c")

  qc_add_code_relation(proj, c3$id, c4$id, "related_to")
  qc_add_code_relation(proj, c4$id, c5$id, "co_occurs_with")

  rels <- qc_list_code_relations(proj)
  expect_true(nrow(rels) >= 2L)
})

test_that("qc_list_code_relations code_id filter returns both directions", {
  cx <- qc_add_code(proj, "rel-x")
  cy <- qc_add_code(proj, "rel-y")
  cz <- qc_add_code(proj, "rel-z")

  qc_add_code_relation(proj, cx$id, cy$id, "related_to")
  qc_add_code_relation(proj, cz$id, cy$id, "related_to")

  rels <- qc_list_code_relations(proj, code_id = cy$id)
  expect_equal(nrow(rels), 2L)
})

test_that("qc_delete_code_relation removes it", {
  cp <- qc_add_code(proj, "rel-p")
  cq <- qc_add_code(proj, "rel-q")

  rel <- qc_add_code_relation(proj, cp$id, cq$id, "related_to")
  qc_delete_code_relation(proj, rel$id)
  expect_false(rel$id %in% qc_list_code_relations(proj)$id)
})

test_that("qc_add_code_relation errors when code_id_1 == code_id_2", {
  cs <- qc_add_code(proj, "rel-self")
  expect_error(qc_add_code_relation(proj, cs$id, cs$id, "related_to"))
})
