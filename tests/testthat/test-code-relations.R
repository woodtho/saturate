test_that("qc_add_code_relation returns correct columns", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "broader")
  c2 <- qc_add_code(proj, "narrower")

  rel <- qc_add_code_relation(proj, c1$id, c2$id, "broader_than",
                               note = "c1 subsumes c2")
  expect_equal(rel$relation_type, "broader_than")
  expect_equal(rel$name_1, "broader")
  expect_equal(rel$name_2, "narrower")
  expect_equal(rel$note, "c1 subsumes c2")
})

test_that("qc_list_code_relations returns all relations", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "a")
  c2 <- qc_add_code(proj, "b")
  c3 <- qc_add_code(proj, "c")

  qc_add_code_relation(proj, c1$id, c2$id, "related_to")
  qc_add_code_relation(proj, c2$id, c3$id, "co_occurs_with")

  rels <- qc_list_code_relations(proj)
  expect_equal(nrow(rels), 2L)
})

test_that("qc_list_code_relations code_id filter returns both directions", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "x")
  c2 <- qc_add_code(proj, "y")
  c3 <- qc_add_code(proj, "z")

  qc_add_code_relation(proj, c1$id, c2$id, "related_to")
  qc_add_code_relation(proj, c3$id, c2$id, "related_to")

  rels <- qc_list_code_relations(proj, code_id = c2$id)
  expect_equal(nrow(rels), 2L)
})

test_that("qc_delete_code_relation removes it", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "p")
  c2 <- qc_add_code(proj, "q")

  rel <- qc_add_code_relation(proj, c1$id, c2$id, "related_to")
  qc_delete_code_relation(proj, rel$id)
  expect_equal(nrow(qc_list_code_relations(proj)), 0L)
})

test_that("qc_add_code_relation errors when code_id_1 == code_id_2", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "self")
  expect_error(qc_add_code_relation(proj, c1$id, c1$id, "related_to"))
})
