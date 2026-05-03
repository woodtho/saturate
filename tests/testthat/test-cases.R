# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc <- qc_import_document(proj, content = "text", name = "case-doc")

# ── Basic CRUD ────────────────────────────────────────────────────────────────

test_that("qc_add_case returns correct tibble", {
  case <- qc_add_case(proj, "Alice", memo = "main respondent")
  expect_equal(case$name, "Alice")
  expect_equal(case$memo, "main respondent")
})

test_that("qc_list_cases shows n_sources = 0 initially", {
  case <- qc_add_case(proj, "Bob")
  cases <- qc_list_cases(proj)
  bob_row <- cases[cases$id == case$id, ]
  expect_equal(bob_row$n_sources[[1L]], 0L)
})

test_that("qc_link_case_source increments n_sources", {
  case <- qc_add_case(proj, "Carol")
  qc_link_case_source(proj, case$id, doc$id)

  cases <- qc_list_cases(proj)
  carol_row <- cases[cases$id == case$id, ]
  expect_equal(carol_row$n_sources[[1L]], 1L)
})

test_that("qc_unlink_case_source decrements n_sources", {
  case <- qc_add_case(proj, "Dan")
  doc2 <- qc_import_document(proj, content = "x", name = "dan-doc")
  qc_link_case_source(proj, case$id, doc2$id)
  qc_unlink_case_source(proj, case$id, doc2$id)

  cases <- qc_list_cases(proj)
  dan_row <- cases[cases$id == case$id, ]
  expect_equal(dan_row$n_sources[[1L]], 0L)
})

test_that("qc_update_case changes name", {
  case <- qc_add_case(proj, "old_name")
  qc_update_case(proj, case$id, name = "new_name")
  cases <- qc_list_cases(proj)
  row <- cases[cases$id == case$id, ]
  expect_equal(row$name[[1L]], "new_name")
})

test_that("qc_update_case changes memo", {
  case <- qc_add_case(proj, "Eve")
  qc_update_case(proj, case$id, memo = "updated")
  cases <- qc_list_cases(proj)
  row <- cases[cases$id == case$id, ]
  expect_equal(row$memo[[1L]], "updated")
})

test_that("qc_delete_case removes it from list", {
  case <- qc_add_case(proj, "gone")
  qc_delete_case(proj, case$id)
  expect_false(case$id %in% qc_list_cases(proj)$id)
})

# ── Attributes ────────────────────────────────────────────────────────────────

test_that("qc_set_case_attribute and qc_list_case_attributes round-trip", {
  case <- qc_add_case(proj, "Fred")
  qc_set_case_attribute(proj, case$id, "age_group", "30-40")
  attrs <- qc_list_case_attributes(proj, case$id)
  expect_equal(nrow(attrs), 1L)
  expect_equal(attrs$variable[[1L]], "age_group")
  expect_equal(attrs$value[[1L]], "30-40")
})

test_that("qc_set_case_attribute upserts on conflict", {
  case <- qc_add_case(proj, "Greta")
  qc_set_case_attribute(proj, case$id, "region", "north")
  qc_set_case_attribute(proj, case$id, "region", "south")
  attrs <- qc_list_case_attributes(proj, case$id)
  expect_equal(nrow(attrs), 1L)
  expect_equal(attrs$value[[1L]], "south")
})

test_that("qc_delete_case_attribute removes the attribute", {
  case <- qc_add_case(proj, "Hans")
  qc_set_case_attribute(proj, case$id, "job", "teacher")
  qc_delete_case_attribute(proj, case$id, "job")
  attrs <- qc_list_case_attributes(proj, case$id)
  expect_equal(nrow(attrs), 0L)
})

test_that("qc_case_attributes_wide returns one column per attribute", {
  case <- qc_add_case(proj, "Iris")
  qc_set_case_attribute(proj, case$id, "gender", "F")
  qc_set_case_attribute(proj, case$id, "cohort", "A")

  wide <- qc_case_attributes_wide(proj)
  expect_true("gender" %in% names(wide))
  expect_true("cohort" %in% names(wide))
})

test_that("qc_case_attributes_wide returns id/name columns when no attributes", {
  p2 <- make_test_project()
  withr::defer(qc_close(p2))

  qc_add_case(p2, "Jay")
  wide <- qc_case_attributes_wide(p2)
  expect_true("case_id"   %in% names(wide))
  expect_true("case_name" %in% names(wide))
})
