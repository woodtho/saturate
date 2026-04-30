test_that("qc_validate_codebook returns empty tibble when no issues", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  c1 <- qc_add_code(proj, "c1")
  doc <- qc_import_document(proj, content = "text", name = "d")
  qc_add_coding(proj, doc$id, c1$id, 1L, 4L)
  qc_update_code(proj, c1$id, definition = "defined", criteria = "clear")
  qc_set_code_key(proj, c1$id, "key1")

  result <- qc_validate_codebook(proj)
  expect_equal(nrow(result), 0L)
})

test_that("qc_validate_codebook detects missing_code_key warning", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  c1 <- qc_add_code(proj, "no_key")
  # qc_add_code auto-generates a key; blank it out to trigger the check
  DBI::dbExecute(proj$con,
    "UPDATE codes SET code_key = NULL WHERE id = ?",
    list(c1$id))
  result <- qc_validate_codebook(proj)
  expect_true(any(result$issue_type == "missing_code_key"))
  expect_true(any(result$severity == "warning"))
})

test_that("qc_validate_codebook detects missing_definition warning", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_code(proj, "no_def")
  result <- qc_validate_codebook(proj)
  expect_true(any(result$issue_type == "missing_definition"))
})

test_that("qc_validate_codebook detects unused_code info", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_code(proj, "unused")
  result <- qc_validate_codebook(proj)
  expect_true(any(result$issue_type == "unused_code"))
  expect_true(any(result$severity == "info"))
})

test_that("qc_validate_codebook returns correct columns", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_code(proj, "c1")
  result <- qc_validate_codebook(proj)
  expect_true(all(c("code_id", "code_name", "issue_type", "severity", "message") %in% names(result)))
})

test_that("qc_validate_codebook detects deprecated_with_codings warning", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  c1  <- qc_add_code(proj, "old_code")
  doc <- qc_import_document(proj, content = "some text here", name = "d")
  qc_add_coding(proj, doc$id, c1$id, 1L, 4L)
  qc_deprecate_code(proj, c1$id, reason = "superseded")

  result <- qc_validate_codebook(proj)
  expect_true(any(result$issue_type == "deprecated_with_codings"))
})
