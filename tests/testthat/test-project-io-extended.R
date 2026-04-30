# ── Lock / unlock ───────────────────────────────────────────────────────────────

test_that("qc_lock_project makes project locked", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_lock_project(proj)
  expect_true(qc_is_locked(proj))
})

test_that("qc_unlock_project restores unlocked state", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_lock_project(proj)
  qc_unlock_project(proj)
  expect_false(qc_is_locked(proj))
})

test_that("qc_is_locked returns FALSE for a new project", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  expect_false(qc_is_locked(proj))
})

test_that("write operations error on a locked project", {
  proj <- make_test_project()
  on.exit({ qc_unlock_project(proj); qc_close(proj) })

  qc_lock_project(proj)
  expect_error(qc_add_code(proj, "should_fail"), "locked")
})

# ── qc_split_project ────────────────────────────────────────────────────────────

test_that("qc_split_project copies codes to the new file", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_add_code(proj, "shared_theme")
  doc <- qc_import_document(proj, content = "sample content here", name = "d1")

  split_path <- withr::local_tempfile(fileext = ".duckdb")
  child <- qc_split_project(proj, split_path)
  on.exit(qc_close(child), add = TRUE)

  codes <- qc_list_codes(child)
  expect_equal(nrow(codes), 1L)
  expect_equal(codes$name[[1L]], "shared_theme")
})

test_that("qc_split_project copies documents", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_import_document(proj, content = "doc content", name = "d1")

  split_path <- withr::local_tempfile(fileext = ".duckdb")
  child <- qc_split_project(proj, split_path)
  on.exit(qc_close(child), add = TRUE)

  docs <- qc_list_documents(child)
  expect_equal(nrow(docs), 1L)
})

test_that("qc_split_project excludes codings by default", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text here", name = "d")
  c1   <- qc_add_code(proj, "code")
  qc_add_coding(proj, doc$id, c1$id, 1L, 4L)

  split_path <- withr::local_tempfile(fileext = ".duckdb")
  child <- qc_split_project(proj, split_path)
  on.exit(qc_close(child), add = TRUE)

  expect_equal(nrow(qc_get_coded_segments(child)), 0L)
})

test_that("qc_split_project include_codings=TRUE copies codings", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text here", name = "d")
  c1   <- qc_add_code(proj, "code")
  qc_add_coding(proj, doc$id, c1$id, 1L, 4L)

  split_path <- withr::local_tempfile(fileext = ".duckdb")
  child <- qc_split_project(proj, split_path, include_codings = TRUE)
  on.exit(qc_close(child), add = TRUE)

  expect_equal(nrow(qc_get_coded_segments(child)), 1L)
})

test_that("qc_split_project errors when path already exists and overwrite=FALSE", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  existing <- withr::local_tempfile(fileext = ".duckdb")
  child1 <- qc_split_project(proj, existing)
  qc_close(child1)

  expect_error(qc_split_project(proj, existing, overwrite = FALSE))
})

# ── qc_merge_project ────────────────────────────────────────────────────────────

test_that("qc_merge_project imports codings from contributor", {
  master <- make_test_project()
  on.exit(qc_close(master))
  qc_add_code(master, "shared")
  qc_import_document(master, content = "shared content text", name = "doc1")

  contrib_path <- withr::local_tempfile(fileext = ".duckdb")
  contrib <- qc_split_project(master, contrib_path, include_codings = FALSE)

  doc_c <- qc_list_documents(contrib)
  code_c <- qc_list_codes(contrib)
  qc_add_coding(contrib, doc_c$id[[1L]], code_c$id[[1L]], 1L, 6L,
                coder = "coder_b")
  qc_close(contrib)

  result <- qc_merge_project(master, contrib_path)
  expect_equal(result$codings_added, 1L)
})

test_that("qc_merge_project skips duplicate codings by default", {
  master <- make_test_project()
  on.exit(qc_close(master))
  qc_add_code(master, "existing")
  doc <- qc_import_document(master, content = "text here", name = "d")
  c1  <- qc_list_codes(master)
  qc_add_coding(master, doc$id, c1$id[[1L]], 1L, 4L, coder = "alice")

  contrib_path <- withr::local_tempfile(fileext = ".duckdb")
  contrib <- qc_split_project(master, contrib_path, include_codings = TRUE)
  qc_close(contrib)

  result <- qc_merge_project(master, contrib_path, on_conflict = "skip")
  expect_equal(result$codings_skip, 1L)
})

test_that("qc_merge_project errors when contributor file not found", {
  master <- make_test_project()
  on.exit(qc_close(master))
  expect_error(qc_merge_project(master, "/no/such/file.duckdb"), "not found")
})
