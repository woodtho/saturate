test_that("qc_snapshot_codebook returns one-row tibble", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_code(proj, "alpha")
  snap <- qc_snapshot_codebook(proj, label = "round 1")
  expect_equal(snap$label, "round 1")
  expect_equal(nrow(snap), 1L)
})

test_that("qc_list_snapshots shows n_codes correctly", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_code(proj, "c1")
  qc_add_code(proj, "c2")
  qc_snapshot_codebook(proj, label = "two codes")

  snaps <- qc_list_snapshots(proj)
  expect_equal(nrow(snaps), 1L)
  expect_equal(snaps$n_codes[[1L]], 2L)
})

test_that("qc_get_snapshot returns tibble with code columns", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_code(proj, "theme1", color = "#FF0000", memo = "first")
  snap <- qc_snapshot_codebook(proj)
  df   <- qc_get_snapshot(proj, snap$id)

  expect_s3_class(df, "tbl_df")
  expect_true("name" %in% names(df))
  expect_equal(df$name[[1L]], "theme1")
})

test_that("qc_get_snapshot errors on invalid id", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  expect_error(qc_get_snapshot(proj, 9999L))
})

test_that("qc_diff_snapshots returns empty tibble when identical", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_code(proj, "c1")
  s1 <- qc_snapshot_codebook(proj, label = "baseline")
  s2 <- qc_snapshot_codebook(proj, label = "same")

  diff <- qc_diff_snapshots(proj, s1$id, s2$id)
  expect_equal(nrow(diff), 0L)
})

test_that("qc_diff_snapshots detects added code", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  qc_add_code(proj, "original")
  s1 <- qc_snapshot_codebook(proj, label = "before")

  qc_add_code(proj, "new_code")
  s2 <- qc_snapshot_codebook(proj, label = "after")

  diff <- qc_diff_snapshots(proj, s1$id, s2$id)
  expect_true(any(diff$change_type == "added"))
  expect_true("new_code" %in% diff$code_name)
})

test_that("qc_diff_snapshots detects changed field", {
  proj <- make_test_project()
  on.exit(qc_close(proj))

  c1 <- qc_add_code(proj, "mutable", color = "#111111")
  s1 <- qc_snapshot_codebook(proj)

  qc_update_code(proj, c1$id, color = "#222222")
  s2 <- qc_snapshot_codebook(proj)

  diff <- qc_diff_snapshots(proj, s1$id, s2$id)
  changed <- diff[diff$change_type == "changed" & diff$field == "color", ]
  expect_equal(nrow(changed), 1L)
  expect_equal(changed$old_value[[1L]], "#111111")
  expect_equal(changed$new_value[[1L]], "#222222")
})
