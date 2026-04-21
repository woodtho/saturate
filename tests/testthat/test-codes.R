test_that("qc_add_code returns correct tibble", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  code <- qc_add_code(proj, "theme1", color = "#E15759", memo = "a theme")
  expect_equal(code$name,  "theme1")
  expect_equal(code$color, "#E15759")
  expect_equal(code$memo,  "a theme")
})

test_that("qc_add_code errors on duplicate name", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_add_code(proj, "dup")
  expect_error(qc_add_code(proj, "dup"))
})

test_that("qc_list_codes includes n_codings and categories", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_add_code(proj, "c1")

  codes <- qc_list_codes(proj)
  expect_true("n_codings" %in% names(codes))
  expect_true("categories" %in% names(codes))
  expect_equal(codes$n_codings[[1L]], 0L)
})

test_that("qc_update_code changes name", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "old")
  qc_update_code(proj, c1$id, name = "new")
  codes <- qc_list_codes(proj)
  expect_equal(codes$name[[1L]], "new")
})

test_that("qc_delete_code removes code from list", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "gone")
  qc_delete_code(proj, c1$id)
  expect_equal(nrow(qc_list_codes(proj)), 0L)
})

test_that("category link/unlink round-trip", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  code <- qc_add_code(proj, "c1")
  cat  <- qc_add_category(proj, "cat1")

  qc_link_code_category(proj, code$id, cat$id)
  cats <- qc_list_categories(proj)
  expect_equal(cats$code_name[[1L]], "c1")

  qc_unlink_code_category(proj, code$id, cat$id)
  cats2 <- qc_list_categories(proj)
  expect_true(all(is.na(cats2$code_id)))
})

test_that("qc_add_category errors on duplicate name", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_add_category(proj, "cat1")
  expect_error(qc_add_category(proj, "cat1"))
})

# ── History tests ─────────────────────────────────────────────────────────────

test_that("qc_add_code writes a create event to history", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  code <- qc_add_code(proj, "theme1")

  hist <- qc_code_history(proj, code$id)
  expect_equal(nrow(hist), 1L)
  expect_equal(hist$operation[[1L]], "create")
  expect_equal(hist$new_value[[1L]], "theme1")
  expect_true(is.na(hist$old_value[[1L]]))
})

test_that("qc_update_code writes one update event per changed field", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  code <- qc_add_code(proj, "original", color = "#111111", memo = "old memo")

  qc_update_code(proj, code$id, name = "renamed", color = "#222222")
  hist <- qc_code_history(proj, code$id)

  # create + 2 update events
  expect_equal(nrow(hist), 3L)
  update_rows <- hist[hist$operation == "update", ]
  expect_setequal(update_rows$field, c("name", "color"))

  name_row <- update_rows[update_rows$field == "name", ]
  expect_equal(name_row$old_value, "original")
  expect_equal(name_row$new_value, "renamed")
})

test_that("qc_update_code skips unchanged fields in history", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  code <- qc_add_code(proj, "same", color = "#AABBCC")

  # Update with the same color — should not log an event for color
  qc_update_code(proj, code$id, name = "changed", color = "#AABBCC")
  hist <- qc_code_history(proj, code$id)

  update_rows <- hist[hist$operation == "update", ]
  expect_equal(nrow(update_rows), 1L)
  expect_equal(update_rows$field, "name")
})

test_that("qc_delete_code writes a delete event to history", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  code <- qc_add_code(proj, "gone")

  qc_delete_code(proj, code$id)
  hist <- qc_code_history(proj, code$id)

  del_row <- hist[hist$operation == "delete", ]
  expect_equal(nrow(del_row), 1L)
  expect_equal(del_row$old_value, "gone")
})

test_that("qc_code_history with NULL code_id returns all codes", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  qc_add_code(proj, "alpha")
  qc_add_code(proj, "beta")

  hist <- qc_code_history(proj)
  expect_equal(nrow(hist), 2L)
  expect_setequal(hist$new_value, c("alpha", "beta"))
})

# ── Merge tests ───────────────────────────────────────────────────────────────

test_that("qc_merge_codes moves codings and soft-deletes the merged code", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc <- qc_import_document(proj, content = "hello world", name = "d1")
  c1  <- qc_add_code(proj, "code1")
  c2  <- qc_add_code(proj, "code2")
  qc_add_coding(proj, doc$id, c1$id, 1L, 5L)
  qc_add_coding(proj, doc$id, c2$id, 7L, 11L)

  qc_merge_codes(proj, from_ids = c2$id, into_id = c1$id)

  codes <- qc_list_codes(proj)
  expect_equal(nrow(codes), 1L)
  expect_equal(codes$name[[1L]], "code1")
  expect_equal(codes$n_codings[[1L]], 2L)
})

test_that("qc_merge_codes logs history for both codes", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "alpha")
  c2 <- qc_add_code(proj, "beta")

  qc_merge_codes(proj, from_ids = c2$id, into_id = c1$id)

  hist_from <- qc_code_history(proj, c2$id)
  merge_from <- hist_from[hist_from$operation == "merge", ]
  expect_equal(nrow(merge_from), 1L)
  expect_equal(merge_from$new_value[[1L]], "alpha")

  hist_into <- qc_code_history(proj, c1$id)
  merge_into <- hist_into[hist_into$operation == "merge", ]
  expect_equal(nrow(merge_into), 1L)
  expect_equal(merge_into$new_value[[1L]], "beta")
})

test_that("qc_merge_codes errors when from_ids contains into_id", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "c1")
  expect_error(qc_merge_codes(proj, from_ids = c1$id, into_id = c1$id))
})

# ── Split tests ───────────────────────────────────────────────────────────────

test_that("qc_split_code creates new codes and logs history", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1        <- qc_add_code(proj, "umbrella")
  new_codes <- qc_split_code(proj, c1$id, c("part1", "part2"))

  codes <- qc_list_codes(proj)
  expect_equal(nrow(codes), 3L)
  expect_true("part1" %in% codes$name)
  expect_true("part2" %in% codes$name)

  hist     <- qc_code_history(proj, c1$id)
  split_row <- hist[hist$operation == "split", ]
  expect_equal(nrow(split_row), 1L)
  expect_true(grepl("part1", split_row$new_value[[1L]]))
})

test_that("qc_split_code errors when fewer than 2 names supplied", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "c1")
  expect_error(qc_split_code(proj, c1$id, "only_one"))
})

test_that("qc_split_code original code is preserved", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  c1 <- qc_add_code(proj, "original")
  qc_split_code(proj, c1$id, c("a", "b"))

  codes <- qc_list_codes(proj)
  expect_true("original" %in% codes$name)
})

# ── Reassign tests ────────────────────────────────────────────────────────────

test_that("qc_reassign_coding moves a coding to another code", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc    <- qc_import_document(proj, content = "hello world", name = "d1")
  c1     <- qc_add_code(proj, "code1")
  c2     <- qc_add_code(proj, "code2")
  coding <- qc_add_coding(proj, doc$id, c1$id, 1L, 5L)

  qc_reassign_coding(proj, coding$id, c2$id)

  codes <- qc_list_codes(proj)
  r1    <- codes[codes$id == c1$id, ]
  r2    <- codes[codes$id == c2$id, ]
  expect_equal(r1$n_codings[[1L]], 0L)
  expect_equal(r2$n_codings[[1L]], 1L)
})
