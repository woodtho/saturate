test_that("qc_add_coding snapshots seltext correctly", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "Hello world", name = "d")
  code <- qc_add_code(proj, "greet")

  cod <- qc_add_coding(proj, doc$id, code$id, selfirst = 1L, selast = 5L)
  expect_equal(cod$seltext, "Hello")
})

test_that("qc_list_codings ordered by selfirst", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "ABCDEFGH", name = "d")
  c1   <- qc_add_code(proj, "code1")
  c2   <- qc_add_code(proj, "code2")

  qc_add_coding(proj, doc$id, c2$id, 5L, 8L)
  qc_add_coding(proj, doc$id, c1$id, 1L, 3L)

  codings <- qc_list_codings(proj, doc$id)
  expect_equal(codings$selfirst, c(1L, 5L))
})

test_that("qc_list_codings includes code_name and code_color", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")
  code <- qc_add_code(proj, "theme", color = "#F00000")
  qc_add_coding(proj, doc$id, code$id, 1L, 4L)

  codings <- qc_list_codings(proj, doc$id)
  expect_equal(codings$code_name[[1L]],  "theme")
  expect_equal(codings$code_color[[1L]], "#F00000")
})

test_that("qc_delete_coding removes it from list", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")
  code <- qc_add_code(proj, "c1")
  cod  <- qc_add_coding(proj, doc$id, code$id, 1L, 4L)

  qc_delete_coding(proj, cod$id)
  expect_equal(nrow(qc_list_codings(proj, doc$id)), 0L)
})

test_that("qc_add_coding errors when selfirst < 1", {
  proj <- make_test_project()
  on.exit(qc_close(proj))
  doc  <- qc_import_document(proj, content = "text", name = "d")
  code <- qc_add_code(proj, "c1")
  expect_error(qc_add_coding(proj, doc$id, code$id, 0L, 3L), "selfirst")
})

test_that("build_highlighted_html returns an htmltools tag", {
  doc   <- "Hello world this is a test."
  codings <- tibble::tibble(
    selfirst   = c(1L, 7L),
    selast     = c(5L, 11L),
    code_name  = c("A", "B"),
    code_color = c("#4E79A7", "#F28E2B"),
    id         = c(1L, 2L),
    memo       = c("", "")
  )
  html <- build_highlighted_html(doc, codings)
  expect_s3_class(html, "shiny.tag")
})

test_that("build_highlighted_html with show_line_numbers=TRUE produces qc-line-num", {
  codings <- tibble::tibble(
    selfirst = integer(), selast = integer(),
    code_name = character(), code_color = character(),
    id = integer(), memo = character()
  )
  html <- build_highlighted_html("line one\nline two", codings,
                                  show_line_numbers = TRUE)
  expect_true(grepl("qc-line-num", as.character(html), fixed = TRUE))
})

test_that("build_highlighted_html with show_timestamps=TRUE wraps timestamps", {
  codings <- tibble::tibble(
    selfirst = integer(), selast = integer(),
    code_name = character(), code_color = character(),
    id = integer(), memo = character()
  )
  html <- build_highlighted_html("[00:01:30] Some text", codings,
                                  show_timestamps = TRUE)
  expect_true(grepl("qc-ts-marker", as.character(html), fixed = TRUE))
})

test_that("build_highlighted_html with show_timestamps=TRUE and show_line_numbers=TRUE on timestamp line produces qc-ts-gutter and qc-line-num", {
  codings <- tibble::tibble(
    selfirst = integer(), selast = integer(),
    code_name = character(), code_color = character(),
    id = integer(), memo = character()
  )
  html <- build_highlighted_html("[00:00:05] Hello world", codings,
                                  show_timestamps   = TRUE,
                                  show_line_numbers = TRUE)
  html_str <- as.character(html)
  expect_true(grepl("qc-ts-gutter", html_str, fixed = TRUE))
  expect_true(grepl("qc-line-num",  html_str, fixed = TRUE))
})

test_that("build_highlighted_html with show_timestamps=FALSE produces no qc-ts-marker spans", {
  codings <- tibble::tibble(
    selfirst = integer(), selast = integer(),
    code_name = character(), code_color = character(),
    id = integer(), memo = character()
  )
  html <- build_highlighted_html("[00:01:30] Some text", codings,
                                  show_timestamps = FALSE)
  expect_false(grepl("qc-ts-marker", as.character(html), fixed = TRUE))
})

test_that(".format_ms(0L) returns [00:00:00]", {
  expect_equal(saturate:::.format_ms(0L), "[00:00:00]")
})

test_that(".format_ms(3661000L) returns [01:01:01]", {
  expect_equal(saturate:::.format_ms(3661000L), "[01:01:01]")
})

test_that(".wrap_timestamps wraps [00:01:30] in a span with data-ts", {
  result <- saturate:::.wrap_timestamps("[00:01:30] hello")
  expect_true(grepl('data-ts="00:01:30"', result, fixed = TRUE))
  expect_true(grepl("qc-ts-marker", result, fixed = TRUE))
})

test_that(".add_line_numbers wraps lines in qc-line divs with count matching newlines", {
  text   <- "line one\nline two\nline three"
  result <- saturate:::.add_line_numbers(text, merge_timestamps = FALSE)
  n_divs <- length(gregexpr('<div class="qc-line">', result, fixed = TRUE)[[1L]])
  n_lines <- length(strsplit(text, "\n", fixed = TRUE)[[1L]])
  expect_equal(n_divs, n_lines)
  expect_true(grepl("qc-line-num", result, fixed = TRUE))
})

test_that(".add_line_numbers merge_timestamps=TRUE on timestamp line produces qc-ts-gutter", {
  html_line <- saturate:::.wrap_timestamps("[00:00:05] Hello")
  result    <- saturate:::.add_line_numbers(html_line, merge_timestamps = TRUE)
  expect_true(grepl("qc-ts-gutter", result, fixed = TRUE))
})
