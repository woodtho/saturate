# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc <- qc_import_document(proj, content = "hello world", name = "shared-doc")

# ── Basic CRUD ────────────────────────────────────────────────────────────────

test_that("qc_add_code returns correct tibble", {
  code <- qc_add_code(proj, "theme1", color = "#E15759", memo = "a theme")
  expect_equal(code$name,  "theme1")
  expect_equal(code$color, "#E15759")
  expect_equal(code$memo,  "a theme")
})

test_that("qc_add_code errors on duplicate name", {
  qc_add_code(proj, "dup")
  expect_error(qc_add_code(proj, "dup"))
})

test_that("qc_list_codes includes n_codings and categories", {
  qc_add_code(proj, "list-codes-c1")
  codes <- qc_list_codes(proj)
  expect_true("n_codings"  %in% names(codes))
  expect_true("categories" %in% names(codes))
})

test_that("qc_update_code changes name", {
  c1 <- qc_add_code(proj, "update-old")
  qc_update_code(proj, c1$id, name = "update-new")
  codes <- qc_list_codes(proj)
  expect_true("update-new" %in% codes$name)
})

test_that("qc_delete_code removes code from list", {
  c1 <- qc_add_code(proj, "gone-code")
  qc_delete_code(proj, c1$id)
  expect_false(c1$id %in% qc_list_codes(proj)$id)
})

# ── Category link/unlink ──────────────────────────────────────────────────────

test_that("category link/unlink round-trip", {
  code <- qc_add_code(proj, "cat-link-code")
  cat  <- qc_add_category(proj, "cat-link-cat")

  qc_link_code_category(proj, code$id, cat$id)
  cats <- qc_list_categories(proj)
  linked_row <- cats[!is.na(cats$code_id) & cats$code_id == code$id, ]
  expect_equal(linked_row$code_name[[1L]], "cat-link-code")

  qc_unlink_code_category(proj, code$id, cat$id)
  cats2 <- qc_list_categories(proj)
  row2 <- cats2[cats2$category_id == cat$id, ]
  expect_true(all(is.na(row2$code_id)))
})

test_that("qc_add_category errors on duplicate name", {
  qc_add_category(proj, "dup-cat")
  expect_error(qc_add_category(proj, "dup-cat"))
})

# ── History tests ─────────────────────────────────────────────────────────────

test_that("qc_add_code writes a create event to history", {
  code <- qc_add_code(proj, "hist-theme1")

  hist <- qc_code_history(proj, code$id)
  expect_equal(nrow(hist), 1L)
  expect_equal(hist$operation[[1L]], "create")
  expect_equal(hist$new_value[[1L]], "hist-theme1")
  expect_true(is.na(hist$old_value[[1L]]))
})

test_that("qc_update_code writes one update event per changed field", {
  code <- qc_add_code(proj, "hist-original", color = "#111111", memo = "old memo")

  qc_update_code(proj, code$id, name = "hist-renamed", color = "#222222")
  hist <- qc_code_history(proj, code$id)

  expect_equal(nrow(hist), 3L)
  update_rows <- hist[hist$operation == "update", ]
  expect_setequal(update_rows$field, c("name", "color"))

  name_row <- update_rows[update_rows$field == "name", ]
  expect_equal(name_row$old_value, "hist-original")
  expect_equal(name_row$new_value, "hist-renamed")
})

test_that("qc_update_code skips unchanged fields in history", {
  code <- qc_add_code(proj, "hist-same", color = "#AABBCC")

  qc_update_code(proj, code$id, name = "hist-changed", color = "#AABBCC")
  hist <- qc_code_history(proj, code$id)

  update_rows <- hist[hist$operation == "update", ]
  expect_equal(nrow(update_rows), 1L)
  expect_equal(update_rows$field, "name")
})

test_that("qc_delete_code writes a delete event to history", {
  code <- qc_add_code(proj, "hist-gone")

  qc_delete_code(proj, code$id)
  hist <- qc_code_history(proj, code$id)

  del_row <- hist[hist$operation == "delete", ]
  expect_equal(nrow(del_row), 1L)
  expect_equal(del_row$old_value, "hist-gone")
})

test_that("qc_code_history with NULL code_id returns all codes", {
  c_alpha <- qc_add_code(proj, "hist-alpha")
  c_beta  <- qc_add_code(proj, "hist-beta")

  hist <- qc_code_history(proj)
  expect_true("hist-alpha" %in% hist$new_value)
  expect_true("hist-beta"  %in% hist$new_value)
})

# ── Merge tests ───────────────────────────────────────────────────────────────

test_that("qc_merge_codes moves codings and soft-deletes the merged code", {
  c1 <- qc_add_code(proj, "merge-code1")
  c2 <- qc_add_code(proj, "merge-code2")
  qc_add_coding(proj, doc$id, c1$id, 1L, 5L)
  qc_add_coding(proj, doc$id, c2$id, 7L, 11L)

  qc_merge_codes(proj, from_ids = c2$id, into_id = c1$id)

  codes <- qc_list_codes(proj)
  expect_false(c2$id %in% codes$id)
  merged <- codes[codes$id == c1$id, ]
  expect_equal(merged$n_codings[[1L]], 2L)
})

test_that("qc_merge_codes logs history for both codes", {
  c1 <- qc_add_code(proj, "merge-alpha")
  c2 <- qc_add_code(proj, "merge-beta")

  qc_merge_codes(proj, from_ids = c2$id, into_id = c1$id)

  hist_from <- qc_code_history(proj, c2$id)
  merge_from <- hist_from[hist_from$operation == "merge", ]
  expect_equal(nrow(merge_from), 1L)
  expect_equal(merge_from$new_value[[1L]], "merge-alpha")

  hist_into <- qc_code_history(proj, c1$id)
  merge_into <- hist_into[hist_into$operation == "merge", ]
  expect_equal(nrow(merge_into), 1L)
  expect_equal(merge_into$new_value[[1L]], "merge-beta")
})

test_that("qc_merge_codes errors when from_ids contains into_id", {
  c1 <- qc_add_code(proj, "merge-self")
  expect_error(qc_merge_codes(proj, from_ids = c1$id, into_id = c1$id))
})

# ── Split tests ───────────────────────────────────────────────────────────────

test_that("qc_split_code creates new codes and logs history", {
  c1        <- qc_add_code(proj, "split-umbrella")
  new_codes <- qc_split_code(proj, c1$id, c("split-part1", "split-part2"))

  codes <- qc_list_codes(proj)
  expect_true("split-part1" %in% codes$name)
  expect_true("split-part2" %in% codes$name)

  hist      <- qc_code_history(proj, c1$id)
  split_row <- hist[hist$operation == "split", ]
  expect_equal(nrow(split_row), 1L)
  expect_true(grepl("split-part1", split_row$new_value[[1L]]))
})

test_that("qc_split_code errors when fewer than 2 names supplied", {
  c1 <- qc_add_code(proj, "split-err")
  expect_error(qc_split_code(proj, c1$id, "only_one"))
})

test_that("qc_split_code original code is preserved", {
  c1 <- qc_add_code(proj, "split-original")
  qc_split_code(proj, c1$id, c("split-a", "split-b"))

  codes <- qc_list_codes(proj)
  expect_true("split-original" %in% codes$name)
})

# ── Reassign tests ────────────────────────────────────────────────────────────

test_that("qc_reassign_coding moves a coding to another code", {
  c1     <- qc_add_code(proj, "reassign-code1")
  c2     <- qc_add_code(proj, "reassign-code2")
  coding <- qc_add_coding(proj, doc$id, c1$id, 1L, 5L)

  qc_reassign_coding(proj, coding$id, c2$id)

  codes <- qc_list_codes(proj)
  r1    <- codes[codes$id == c1$id, ]
  r2    <- codes[codes$id == c2$id, ]
  expect_equal(r1$n_codings[[1L]], 0L)
  expect_equal(r2$n_codings[[1L]], 1L)
})

# ── Definition / deprecated tests ────────────────────────────────────────────

test_that("qc_add_code stores definition", {
  qc_add_code(proj, "def_code", definition = "A detailed definition")
  codes <- qc_list_codes(proj)
  def_row <- codes[codes$name == "def_code", ]
  expect_equal(def_row$definition[[1L]], "A detailed definition")
})

test_that("qc_list_codes deprecated column is FALSE for a new code", {
  qc_add_code(proj, "fresh_code")
  codes <- qc_list_codes(proj)
  fresh_row <- codes[codes$name == "fresh_code", ]
  expect_equal(fresh_row$deprecated[[1L]], 0L)
})

test_that("qc_deprecate_code sets deprecated = TRUE", {
  c1 <- qc_add_code(proj, "dep-old_code")
  qc_deprecate_code(proj, c1$id)
  codes <- qc_list_codes(proj)
  row <- codes[codes$id == c1$id, ]
  expect_equal(row$deprecated[[1L]], 1L)
})

test_that("qc_undeprecate_code restores deprecated = FALSE", {
  c1 <- qc_add_code(proj, "dep-revived_code")
  qc_deprecate_code(proj, c1$id)
  qc_undeprecate_code(proj, c1$id)
  codes <- qc_list_codes(proj)
  row <- codes[codes$id == c1$id, ]
  expect_equal(row$deprecated[[1L]], 0L)
})

test_that("qc_merge_codes moves codings to target and soft-deletes source", {
  src <- qc_add_code(proj, "merge2-source_code")
  tgt <- qc_add_code(proj, "merge2-target_code")
  qc_add_coding(proj, doc$id, src$id, 1L, 5L)

  qc_merge_codes(proj, from_ids = src$id, into_id = tgt$id)

  codes <- qc_list_codes(proj)
  expect_false(src$id %in% codes$id)
  tgt_row <- codes[codes$id == tgt$id, ]
  expect_equal(tgt_row$n_codings[[1L]], 1L)
})

test_that("qc_split_code creates a new child code", {
  c1 <- qc_add_code(proj, "split2-parent_code")
  qc_split_code(proj, c1$id, c("split2-child_a", "split2-child_b"))

  codes <- qc_list_codes(proj)
  expect_true("split2-child_a" %in% codes$name)
  expect_true("split2-child_b" %in% codes$name)
})
