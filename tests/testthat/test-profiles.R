test_that("profiles are stored with settings in the project database", {
  skip_if_not_installed("jsonlite")
  proj <- make_test_project()
  on.exit(qc_close(proj))

  settings <- list(
    colorTheme = "dark",
    showLineNumbers = FALSE,
    showTimestamps = FALSE,
    ttsRate = 1.25
  )

  .db_upsert_profile(proj, "Coder A")
  .db_save_profile_settings(proj, "Coder A", settings)

  profiles <- .db_list_profiles(proj)
  expect_true("Coder A" %in% profiles$name)

  stored <- profiles$settings_json[profiles$name == "Coder A"][[1L]]
  parsed <- jsonlite::fromJSON(stored, simplifyVector = FALSE)
  expect_identical(parsed$colorTheme, "dark")
  expect_false(parsed$showLineNumbers)
  expect_false(parsed$showTimestamps)
  expect_equal(parsed$ttsRate, 1.25)

  public <- qc_list_profiles(proj)
  expect_named(public, c("name", "created_at", "last_used_at"))
})

test_that("saving profile settings creates missing profiles", {
  skip_if_not_installed("jsonlite")
  proj <- make_test_project()
  on.exit(qc_close(proj))

  .db_save_profile_settings(proj, "Coder B", list(colorTheme = "ocean"))

  profiles <- .db_list_profiles(proj)
  expect_true("Coder B" %in% profiles$name)
  parsed <- jsonlite::fromJSON(
    profiles$settings_json[profiles$name == "Coder B"][[1L]],
    simplifyVector = FALSE
  )
  expect_identical(parsed$colorTheme, "ocean")
})

test_that("profiles and settings are restored after reopening a project", {
  skip_if_not_installed("jsonlite")
  proj <- make_test_project()
  path <- proj$path

  .db_save_profile_settings(proj, "Restart Coder", list(
    colorTheme = "contrast",
    showTimestamps = FALSE,
    tableDensity = "compact"
  ))
  qc_close(proj)

  reopened <- qc_open(path)
  on.exit(qc_close(reopened), add = TRUE)

  profiles <- .db_list_profiles(reopened)
  expect_true("Restart Coder" %in% profiles$name)

  parsed <- jsonlite::fromJSON(
    profiles$settings_json[profiles$name == "Restart Coder"][[1L]],
    simplifyVector = FALSE
  )
  expect_identical(parsed$colorTheme, "contrast")
  expect_false(parsed$showTimestamps)
  expect_identical(parsed$tableDensity, "compact")
})
