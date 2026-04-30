test_that("qc_add_project_memo returns one-row tibble", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  m <- qc_add_project_memo(proj, "Initial thoughts", type = "analytical")
  expect_equal(m$content, "Initial thoughts")
  expect_equal(m$memo_type, "analytical")
})

test_that("qc_list_project_memos returns all entries", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_project_memo(proj, "memo one")
  qc_add_project_memo(proj, "memo two")
  memos <- qc_list_project_memos(proj)
  expect_equal(nrow(memos), 2L)
})

test_that("qc_list_project_memos type filter works", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_project_memo(proj, "analytical note", type = "analytical")
  qc_add_project_memo(proj, "decision note",   type = "decision")

  memos <- qc_list_project_memos(proj, type = "analytical")
  expect_equal(nrow(memos), 1L)
  expect_equal(memos$memo_type[[1L]], "analytical")
})

test_that("qc_delete_project_memo soft-deletes entry", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  m <- qc_add_project_memo(proj, "to be deleted")
  qc_delete_project_memo(proj, m$id)
  memos <- qc_list_project_memos(proj)
  expect_equal(nrow(memos), 0L)
})

test_that("qc_add_project_memo uses custom created_by", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  m <- qc_add_project_memo(proj, "note", created_by = "researcher_A")
  expect_equal(m$created_by, "researcher_A")
})

test_that("qc_add_project_memo accepts reflexivity type", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  m <- qc_add_project_memo(proj, "reflective note", type = "reflexivity")
  expect_equal(m$memo_type, "reflexivity")
})
