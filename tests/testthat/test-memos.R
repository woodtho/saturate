# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

# ── Type tests ────────────────────────────────────────────────────────────────

test_that("qc_add_project_memo returns 1-row tibble with correct columns", {
  m <- qc_add_project_memo(proj, "My first memo", type = "analytical")
  expect_equal(nrow(m), 1L)
  expect_named(m, c("id", "content", "memo_type", "created_by", "created_at"))
})

test_that("qc_add_project_memo accepts analytical type", {
  m <- qc_add_project_memo(proj, "note", type = "analytical")
  expect_equal(m$memo_type, "analytical")
})

test_that("qc_add_project_memo accepts reflexivity type", {
  m <- qc_add_project_memo(proj, "note", type = "reflexivity")
  expect_equal(m$memo_type, "reflexivity")
})

test_that("qc_add_project_memo accepts decision type", {
  m <- qc_add_project_memo(proj, "note", type = "decision")
  expect_equal(m$memo_type, "decision")
})

test_that("qc_add_project_memo accepts methodological type", {
  m <- qc_add_project_memo(proj, "note", type = "methodological")
  expect_equal(m$memo_type, "methodological")
})

test_that("qc_add_project_memo accepts other type", {
  m <- qc_add_project_memo(proj, "note", type = "other")
  expect_equal(m$memo_type, "other")
})

test_that("qc_add_project_memo uses created_by argument", {
  m <- qc_add_project_memo(proj, "note", created_by = "researcher_A")
  expect_equal(m$created_by, "researcher_A")
})

# ── List / ordering tests ─────────────────────────────────────────────────────

test_that("qc_list_project_memos returns entries ordered newest-first", {
  m1 <- qc_add_project_memo(proj, "list-order-first",  type = "analytical")
  m2 <- qc_add_project_memo(proj, "list-order-second", type = "analytical")

  memos <- qc_list_project_memos(proj)
  expect_true(memos$id[[1L]] > memos$id[[2L]])
})

test_that("qc_list_project_memos type= filters to matching type only", {
  qc_add_project_memo(proj, "type-filter-analytical", type = "analytical")
  qc_add_project_memo(proj, "type-filter-decision",   type = "decision")

  memos <- qc_list_project_memos(proj, type = "decision")
  expect_true(nrow(memos) >= 1L)
  expect_true(all(memos$memo_type == "decision"))
})

# ── Delete test ───────────────────────────────────────────────────────────────

test_that("qc_delete_project_memo soft-deletes entry from list", {
  m <- qc_add_project_memo(proj, "to delete")
  qc_delete_project_memo(proj, m$id)
  expect_false(m$id %in% qc_list_project_memos(proj)$id)
})

# ── Export tests ──────────────────────────────────────────────────────────────

test_that("qc_export_journal csv returns a path that exists with expected columns", {
  skip_on_cran()
  qc_add_project_memo(proj, "a csv memo", type = "analytical", created_by = "tester")
  p <- qc_export_journal(proj, format = "csv")
  expect_true(file.exists(p))

  lines <- readLines(p)
  header <- lines[[1L]]
  expect_true(grepl("id",         header))
  expect_true(grepl("content",    header))
  expect_true(grepl("memo_type",  header))
  expect_true(grepl("created_by", header))
  expect_true(grepl("created_at", header))
})

test_that("qc_export_journal txt file exists and contains memo text", {
  skip_on_cran()
  qc_add_project_memo(proj, "unique-memo-text-xyz", type = "analytical")
  p <- qc_export_journal(proj, format = "txt")
  expect_true(file.exists(p))

  content <- paste(readLines(p), collapse = "\n")
  expect_true(grepl("unique-memo-text-xyz", content))
})

test_that("qc_export_journal html file exists and is non-empty HTML", {
  skip_on_cran()
  qc_add_project_memo(proj, "html memo", type = "analytical")
  p <- qc_export_journal(proj, format = "html")
  expect_true(file.exists(p))

  content <- paste(readLines(p), collapse = "\n")
  expect_true(grepl("<!DOCTYPE html>", content, fixed = TRUE))
  expect_true(nchar(content) > 0L)
})

test_that("qc_export_journal path= copies to supplied path and returns it", {
  skip_on_cran()
  qc_add_project_memo(proj, "copy memo", type = "analytical")
  dest <- withr::local_tempfile(fileext = ".csv")
  result <- qc_export_journal(proj, path = dest, format = "csv")
  expect_equal(result, dest)
  expect_true(file.exists(dest))
})

test_that("qc_export_journal on empty project returns file without error", {
  skip_on_cran()
  p2 <- make_test_project()
  withr::defer(qc_close(p2))
  expect_no_error({
    p <- qc_export_journal(p2, format = "csv")
    expect_true(file.exists(p))
  })
})
