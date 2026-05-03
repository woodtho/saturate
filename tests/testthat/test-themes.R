# ── Shared project (file scope) ──────────────────────────────────────────────
proj <- make_test_project()
withr::defer(qc_close(proj), envir = testthat::teardown_env())

doc <- qc_import_document(proj, content = "Hello world", name = "doc1")

# ── Basic CRUD ────────────────────────────────────────────────────────────────

test_that("qc_add_theme returns 1-row tibble with id and name", {
  t1 <- qc_add_theme(proj, "Resilience")
  expect_equal(nrow(t1), 1L)
  expect_true("id"   %in% names(t1))
  expect_true("name" %in% names(t1))
  expect_equal(t1$name, "Resilience")
})

test_that("qc_list_themes returns themes in the project", {
  qc_add_theme(proj, "Theme A")
  qc_add_theme(proj, "Theme B")
  themes <- qc_list_themes(proj)
  expect_true("Theme A" %in% themes$name)
  expect_true("Theme B" %in% themes$name)
})

test_that("qc_update_theme persists narrative", {
  t1 <- qc_add_theme(proj, "Identity")
  qc_update_theme(proj, t1$id, narrative = "This theme captures identity formation.")

  themes <- qc_list_themes(proj)
  row    <- themes[themes$id == t1$id, ]
  expect_equal(row$narrative[[1L]], "This theme captures identity formation.")
})

test_that("qc_delete_theme soft-deletes the theme", {
  t1 <- qc_add_theme(proj, "Gone")
  qc_delete_theme(proj, t1$id)
  themes <- qc_list_themes(proj)
  expect_false(t1$id %in% themes$id)
})

# ── Link / excerpt tests ──────────────────────────────────────────────────────

test_that("qc_link_theme_codes links codes to a theme", {
  t1 <- qc_add_theme(proj, "T-link")
  c1 <- qc_add_code(proj, "theme-code_a")
  c2 <- qc_add_code(proj, "theme-code_b")

  qc_link_theme_codes(proj, t1$id, c(c1$id, c2$id))

  detail <- qc_get_theme(proj, t1$id)
  expect_equal(nrow(detail$linked_codes), 2L)
  expect_true("theme-code_a" %in% detail$linked_codes$name)
  expect_true("theme-code_b" %in% detail$linked_codes$name)
})

test_that("qc_theme_excerpts returns codings from linked codes", {
  c1 <- qc_add_code(proj, "theme-code_x")
  t1 <- qc_add_theme(proj, "T-excerpt")

  qc_add_coding(proj, doc$id, c1$id, 1L, 5L)
  qc_link_theme_codes(proj, t1$id, c1$id)

  excerpts <- qc_theme_excerpts(proj, t1$id)
  expect_equal(nrow(excerpts), 1L)
  expect_equal(excerpts$seltext[[1L]], "Hello")
})

test_that("qc_theme_excerpts returns empty tibble when no codings", {
  c1 <- qc_add_code(proj, "theme-empty_code")
  t1 <- qc_add_theme(proj, "Empty Theme")
  qc_link_theme_codes(proj, t1$id, c1$id)

  excerpts <- qc_theme_excerpts(proj, t1$id)
  expect_equal(nrow(excerpts), 0L)
})

test_that("qc_link_theme_categories links a category to a theme", {
  t1  <- qc_add_theme(proj, "T with cat")
  cat <- qc_add_category(proj, "ThemeCat1")

  qc_link_theme_categories(proj, t1$id, cat$id)

  detail <- qc_get_theme(proj, t1$id)
  expect_equal(nrow(detail$linked_cats), 1L)
  expect_equal(detail$linked_cats$name[[1L]], "ThemeCat1")
})
