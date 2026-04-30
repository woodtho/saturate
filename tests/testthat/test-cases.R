test_that("qc_add_case returns correct tibble", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "Alice", memo = "main respondent")
  expect_equal(case$name, "Alice")
  expect_equal(case$memo, "main respondent")
})

test_that("qc_list_cases shows n_sources = 0 initially", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_case(proj, "Bob")
  cases <- qc_list_cases(proj)
  expect_equal(nrow(cases), 1L)
  expect_equal(cases$n_sources[[1L]], 0L)
})

test_that("qc_link_case_source increments n_sources", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "Carol")
  doc  <- qc_import_document(proj, content = "text", name = "d1")
  qc_link_case_source(proj, case$id, doc$id)

  cases <- qc_list_cases(proj)
  expect_equal(cases$n_sources[[1L]], 1L)
})

test_that("qc_unlink_case_source decrements n_sources", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "Dan")
  doc  <- qc_import_document(proj, content = "x", name = "d")
  qc_link_case_source(proj, case$id, doc$id)
  qc_unlink_case_source(proj, case$id, doc$id)

  cases <- qc_list_cases(proj)
  expect_equal(cases$n_sources[[1L]], 0L)
})

test_that("qc_update_case changes name", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "old_name")
  qc_update_case(proj, case$id, name = "new_name")
  cases <- qc_list_cases(proj)
  expect_equal(cases$name[[1L]], "new_name")
})

test_that("qc_update_case changes memo", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "Eve")
  qc_update_case(proj, case$id, memo = "updated")
  cases <- qc_list_cases(proj)
  expect_equal(cases$memo[[1L]], "updated")
})

test_that("qc_delete_case removes it from list", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "gone")
  qc_delete_case(proj, case$id)
  expect_equal(nrow(qc_list_cases(proj)), 0L)
})

test_that("qc_set_case_attribute and qc_list_case_attributes round-trip", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "Fred")
  qc_set_case_attribute(proj, case$id, "age_group", "30-40")
  attrs <- qc_list_case_attributes(proj, case$id)
  expect_equal(nrow(attrs), 1L)
  expect_equal(attrs$variable[[1L]], "age_group")
  expect_equal(attrs$value[[1L]], "30-40")
})

test_that("qc_set_case_attribute upserts on conflict", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "Greta")
  qc_set_case_attribute(proj, case$id, "region", "north")
  qc_set_case_attribute(proj, case$id, "region", "south")
  attrs <- qc_list_case_attributes(proj, case$id)
  expect_equal(nrow(attrs), 1L)
  expect_equal(attrs$value[[1L]], "south")
})

test_that("qc_delete_case_attribute removes the attribute", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "Hans")
  qc_set_case_attribute(proj, case$id, "job", "teacher")
  qc_delete_case_attribute(proj, case$id, "job")
  attrs <- qc_list_case_attributes(proj, case$id)
  expect_equal(nrow(attrs), 0L)
})

test_that("qc_case_attributes_wide returns one column per attribute", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  case <- qc_add_case(proj, "Iris")
  qc_set_case_attribute(proj, case$id, "gender", "F")
  qc_set_case_attribute(proj, case$id, "cohort", "A")

  wide <- qc_case_attributes_wide(proj)
  expect_true("gender" %in% names(wide))
  expect_true("cohort" %in% names(wide))
})

test_that("qc_case_attributes_wide returns id/name columns when no attributes", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_case(proj, "Jay")
  wide <- qc_case_attributes_wide(proj)
  expect_true("case_id"   %in% names(wide))
  expect_true("case_name" %in% names(wide))
})
