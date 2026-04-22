#' Update a document's content, preserving the previous version
#'
#' Archives the current content in `source_versions` before writing the new
#' text. All codings for this document are flagged `coding_status = 'needs_review'`
#' because their character offsets may no longer be valid.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Document id.
#' @param content Character. New document text.
#' @param memo Character. Version memo (reason for update).
#'
#' @return The updated one-row tibble from [qc_get_document()].
#' @export
qc_update_document_content <- function(project, id, content, memo = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  if (!is_string(content)) rlang::abort("`content` must be a single string.")
  id <- as.integer(id)

  current <- qc_get_document(project, id)

  # Archive current version
  .exec(project$con,
    "INSERT INTO source_versions
       (source_id, version, content, content_hash, memo)
     VALUES (?, ?, ?, md5(?), ?)",
    list(id, current$doc_version, current$content,
         current$content, memo %||% "")
  )

  content    <- .normalize_content(content)
  word_count <- .count_words(content)
  new_ver    <- current$doc_version + 1L

  .exec(project$con,
    "UPDATE sources
     SET content = ?, doc_version = ?, word_count = ?,
         content_hash = md5(?)
     WHERE id = ? AND status = 1",
    list(content, new_ver, word_count, content, id)
  )

  n_flagged <- .exec(project$con,
    "UPDATE codings SET coding_status = 'needs_review'
     WHERE source_id = ? AND status = 1
       AND coding_status = 'validated'",
    list(id)
  )
  if (n_flagged > 0L)
    cli::cli_warn(c(
      "!" = "{n_flagged} coding{?s} flagged 'needs_review' — offsets may have shifted."
    ))

  cli::cli_alert_success(
    "Updated '{current$name}' to version {new_ver}.")
  qc_get_document(project, id)
}


#' List all saved versions of a document
#'
#' Version 1 is the original; subsequent versions are created by
#' [qc_update_document_content()]. The current live content is always in
#' [qc_get_document()].
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#'
#' @return A tibble: `version`, `content_hash`, `word_count`, `memo`,
#'   `imported_at`.
#' @export
qc_list_versions <- function(project, source_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  source_id <- as.integer(source_id)

  .query(project$con,
    "SELECT version,
            length(content) - length(replace(content, ' ', '')) + 1
              AS word_count,
            content_hash, memo, imported_at
     FROM   source_versions
     WHERE  source_id = ?
     ORDER  BY version",
    list(source_id)
  )
}


#' Retrieve the content of a specific document version
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param version Integer. Version number (as returned by [qc_list_versions()]).
#'
#' @return A one-row tibble: `version`, `content`, `content_hash`, `memo`,
#'   `imported_at`.
#' @export
qc_get_version <- function(project, source_id, version) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  out <- .query(project$con,
    "SELECT version, content, content_hash, memo, imported_at
     FROM   source_versions
     WHERE  source_id = ? AND version = ?",
    list(as.integer(source_id), as.integer(version))
  )
  if (nrow(out) == 0L)
    rlang::abort(paste0("Version ", version, " not found for source_id = ",
                        source_id))
  out
}


#' Restore a document to a previous version
#'
#' Calls [qc_update_document_content()] with the archived content, which
#' archives the current version first, then writes the restored text.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param version Integer. Version to restore.
#'
#' @return The updated document tibble from [qc_get_document()].
#' @export
qc_restore_version <- function(project, source_id, version) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  old <- qc_get_version(project, source_id, version)
  qc_update_document_content(
    project, source_id, old$content,
    memo = paste0("Restored from version ", version)
  )
}
