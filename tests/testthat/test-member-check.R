test_that("qc_export_member_check handles return instructions without duplicate columns", {
  skip_on_cran()
  proj <- make_test_project()
  on.exit(qc_close(proj))

  doc  <- qc_import_document(
    proj,
    content = "Participants described needing more follow-up support.",
    name = "interview-1"
  )
  code <- qc_add_code(proj, "support")

  qc_add_coding(proj, doc$id, code$id, 1L, 27L, coder = "tester")

  check <- qc_create_member_check(
    proj,
    source_id           = doc$id,
    participant_label   = "Participant A",
    created_by          = "tester",
    return_by           = "2026-05-01",
    return_to           = "researcher@example.org",
    return_instructions = "Please review and reply by email."
  )

  html <- expect_no_error(
    qc_export_member_check(proj, check$id, format = "html")
  )

  expect_match(html, "Participant A", fixed = TRUE)
  expect_match(html, "2026-05-01", fixed = TRUE)
  expect_match(html, "researcher@example.org", fixed = TRUE)
  expect_match(html, "Please review and reply by email.", fixed = TRUE)
})
