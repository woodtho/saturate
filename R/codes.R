# Converts a display name to a lowercase slug (a-z, 0-9, _ only).
# Ensures uniqueness within existing_keys by appending _2, _3 as needed.
.make_code_key <- function(name, existing_keys) {
  key    <- tolower(name)
  key    <- gsub("[^a-z0-9]+", "_", key)
  key    <- gsub("^_+|_+$",    "",  key)
  if (nchar(key) == 0L) key <- "code"
  base   <- key
  suffix <- 2L
  while (key %in% existing_keys) {
    key    <- paste0(base, "_", suffix)
    suffix <- suffix + 1L
  }
  key
}

# Internal helper -- append one row to code_history
.log_code_history <- function(con, code_id, operation,
                               field     = NULL,
                               old_value = NULL,
                               new_value = NULL) {
  .exec(con,
    "INSERT INTO code_history
       (code_id, operation, field, old_value, new_value)
     VALUES (?, ?, ?, ?, ?)",
    list(as.integer(code_id),
         as.character(operation),
         field     %||% NA_character_,
         old_value %||% NA_character_,
         new_value %||% NA_character_)
  )
}

#' Add a code to the project codebook
#'
#' @param project A `qc_project` object.
#' @param name Character. Code label. Must be unique.
#' @param color Character. Hex colour (e.g. `"#E15759"`).
#' @param memo Character. Short description / memo.
#' @param parent_id Integer or `NULL`. Parent code id for hierarchical
#'   taxonomies.
#' @param definition Character. Full definition of the code.
#' @param criteria Character. Inclusion/exclusion criteria for coders.
#' @param code_key Character or `NULL`. Stable slug (e.g. `"positive_affect"`).
#'   Auto-generated from `name` when `NULL`. Must be unique; may contain only
#'   lowercase letters, digits, and underscores.
#' @param level Character. Analytic level of the code (e.g. `"descriptive"`,
#'   `"interpretive"`).
#' @param orientation Character. Theoretical orientation of the code.
#' @param weight Numeric or `NULL`. Optional numeric weight for the code.
#' @param weight_description Character. Description of what the weight represents.
#'
#' @return A one-row tibble: `id`, `name`, `color`, `memo`, `created_at`.
#' @export
qc_add_code <- function(project, name, color = "#4E79A7", memo = "",
                         parent_id = NULL, definition = "", criteria = "",
                         code_key = NULL, level = "", orientation = "",
                         weight = NULL, weight_description = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  if (!is_string(name))  rlang::abort("`name` must be a single string.")
  if (!is_string(color)) rlang::abort("`color` must be a single string.")

  existing_keys <- .query(project$con,
    "SELECT code_key FROM codes WHERE status = 1 AND code_key IS NOT NULL"
  )$code_key
  if (is.null(code_key)) {
    final_key <- .make_code_key(name, existing_keys)
  } else {
    final_key <- as.character(code_key)
    if (final_key %in% existing_keys)
      rlang::abort(paste0("code_key '", final_key, "' is already in use."))
  }

  wt <- if (!is.null(weight)) as.double(weight) else NA_real_
  if (is.null(parent_id)) {
    code <- .query(project$con,
      "INSERT INTO codes
         (name, color, memo, definition, criteria, code_key,
          level, orientation, weight, weight_description)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
       RETURNING id, name, color, memo, created_at",
      list(name, color, memo %||% "", definition %||% "", criteria %||% "",
           final_key, level %||% "", orientation %||% "",
           wt, weight_description %||% "")
    )
  } else {
    code <- .query(project$con,
      "INSERT INTO codes
         (name, color, memo, parent_id, definition, criteria, code_key,
          level, orientation, weight, weight_description)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
       RETURNING id, name, color, memo, created_at",
      list(name, color, memo %||% "", as.integer(parent_id),
           definition %||% "", criteria %||% "", final_key,
           level %||% "", orientation %||% "",
           wt, weight_description %||% "")
    )
  }
  .log_code_history(project$con, code$id, "create",
                    new_value = name)
  code
}

#' List all codes
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble with columns `id`, `name`, `color`, `memo`, `parent_id`,
#'   `parent_name`, `definition`, `criteria`, `code_key`, `deprecated`,
#'   `deprecated_reason`, `depth` (0 = root), `n_codings`, `categories`.
#' @export
qc_list_codes <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .query(project$con, "
    WITH RECURSIVE hierarchy AS (
      SELECT id, name, parent_id, 0 AS depth
      FROM   codes
      WHERE  status = 1 AND parent_id IS NULL
      UNION ALL
      SELECT c.id, c.name, c.parent_id, h.depth + 1
      FROM   codes c
      JOIN   hierarchy h ON c.parent_id = h.id
      WHERE  c.status = 1
    )
    SELECT c.id, c.name, c.color, c.memo,
           c.parent_id,
           p.name                                             AS parent_name,
           c.definition, c.criteria,
           c.code_key, c.deprecated, c.deprecated_reason,
           h.depth,
           c.weight, c.weight_description,
           COUNT(DISTINCT cod.id)                            AS n_codings,
           STRING_AGG(DISTINCT cat.name, ', ' ORDER BY cat.name)
                                                             AS categories
    FROM   codes c
    JOIN   hierarchy h    ON h.id = c.id
    LEFT   JOIN codes p   ON p.id = c.parent_id
    LEFT   JOIN codings cod
           ON cod.code_id = c.id AND cod.status = 1
    LEFT   JOIN code_category_links l
           ON l.code_id = c.id AND l.status = 1
    LEFT   JOIN code_categories cat
           ON cat.id = l.category_id AND cat.status = 1
    WHERE  c.status = 1
    GROUP  BY c.id, c.name, c.color, c.memo, c.parent_id,
              p.name, c.definition, c.criteria,
              c.code_key, c.deprecated, c.deprecated_reason, h.depth,
              c.weight, c.weight_description
    ORDER  BY h.depth, c.name
  ")
}

#' Update a code's fields
#'
#' Each changed field is recorded in `code_history` before the update is
#' applied, preserving a complete audit trail.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Code id.
#' @param name,color,memo,definition,criteria Character scalars. Pass
#'   non-`NULL` to update.
#' @param parent_id Integer or `NA`. Pass an integer to set a parent;
#'   `NA` to make the code a root node.
#' @param level Character or `NULL`. Analytic level of the code.
#' @param orientation Character or `NULL`. Theoretical orientation of the code.
#' @param weight Numeric, `NA`, or `NULL`. Pass a number to set; `NA` to clear.
#' @param weight_description Character or `NULL`. Description of what the weight
#'   represents.
#'
#' @return The updated one-row tibble.
#' @export
qc_update_code <- function(project, id,
                            name               = NULL,
                            color              = NULL,
                            memo               = NULL,
                            definition         = NULL,
                            criteria           = NULL,
                            parent_id          = NULL,
                            level              = NULL,
                            orientation        = NULL,
                            weight             = NULL,
                            weight_description = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id <- as.integer(id)

  current <- .query(project$con,
    "SELECT name, color, memo, definition, criteria,
            level, orientation, weight_description FROM codes
     WHERE id = ? AND status = 1",
    list(id)
  )
  if (nrow(current) == 0L)
    rlang::abort(paste0("No active code with id = ", id))

  # Numeric weight handled separately -- it's a DOUBLE, not VARCHAR
  if (!is.null(weight)) {
    wt <- if (is.na(weight)) NA_real_ else as.double(weight)
    .exec(project$con,
      "UPDATE codes SET weight = ? WHERE id = ? AND status = 1",
      list(wt, id))
  }

  str_updates <- list(name = name, color = color, memo = memo,
                      definition = definition, criteria = criteria,
                      level = level, orientation = orientation,
                      weight_description = weight_description)
  for (col in names(str_updates)) {
    val <- str_updates[[col]]
    if (is.null(val)) next
    if (!is_string(val))
      rlang::abort(paste0("`", col, "` must be a single string."))
    old_val <- current[[col]][[1L]]
    if (identical(old_val, val)) next
    .log_code_history(project$con, id, "update",
                      field     = col,
                      old_value = old_val,
                      new_value = val)
    .exec(project$con,
      paste0("UPDATE codes SET ", col, " = ? WHERE id = ? AND status = 1"),
      list(val, id)
    )
  }

  if (!is.null(parent_id)) {
    if (is.na(parent_id)) {
      .exec(project$con,
        "UPDATE codes SET parent_id = NULL WHERE id = ? AND status = 1",
        list(id)
      )
    } else {
      .exec(project$con,
        "UPDATE codes SET parent_id = ? WHERE id = ? AND status = 1",
        list(as.integer(parent_id), id)
      )
    }
  }

  .query(project$con,
    "SELECT id, name, color, memo, definition, criteria,
            parent_id, created_at
     FROM codes WHERE id = ?",
    list(id)
  )
}

#' Delete a code (soft delete)
#'
#' Also soft-deletes all codings that used this code.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Code id.
#'
#' @return Invisibly, the number of codings also soft-deleted.
#' @export
qc_delete_code <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id <- as.integer(id)

  current <- .query(project$con,
    "SELECT name FROM codes WHERE id = ? AND status = 1",
    list(id)
  )
  if (nrow(current) > 0L)
    .log_code_history(project$con, id, "delete",
                      old_value = current$name[[1L]])

  n <- .exec(project$con,
    "UPDATE codings SET status = 0 WHERE code_id = ? AND status = 1",
    list(id)
  )
  .soft_delete(project$con, "codes", "id", id)
  invisible(n)
}

#' Retrieve the change history for one or all codes
#'
#' Returns an append-only audit log. Each row records one mutation:
#' a `create` event when a code is first added, one `update` event per
#' changed field (with before/after values), and a `delete` event when
#' the code is soft-deleted.
#'
#' @param project A `qc_project` object.
#' @param code_id Integer or `NULL`. When supplied, restricts results to
#'   that code. When `NULL`, returns history for all codes.
#'
#' @return A tibble: `id`, `code_id`, `code_name`, `operation`, `field`,
#'   `old_value`, `new_value`, `changed_at`. Ordered newest-first.
#' @export
qc_code_history <- function(project, code_id = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  where <- if (!is.null(code_id))
    paste0("WHERE h.code_id = ", as.integer(code_id))
  else ""
  .query(project$con, paste0("
    SELECT h.id, h.code_id,
           c.name AS code_name,
           h.operation, h.field,
           h.old_value, h.new_value, h.changed_at
    FROM   code_history h
    LEFT   JOIN codes c ON c.id = h.code_id
    ", where, "
    ORDER  BY h.changed_at DESC
  "))
}

#' Add a code category
#'
#' @param project A `qc_project` object.
#' @param name Character. Category name. Must be unique.
#' @param memo Character.
#'
#' @return A one-row tibble: `id`, `name`, `memo`.
#' @export
qc_add_category <- function(project, name, memo = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!is_string(name)) rlang::abort("`name` must be a single string.")
  .query(project$con,
    "INSERT INTO code_categories (name, memo)
     VALUES (?, ?)
     RETURNING id, name, memo",
    list(name, memo %||% "")
  )
}

#' List all categories with their member codes
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble: `category_id`, `category_name`, `code_id`,
#'   `code_name`, `code_color`. Codes not in any category appear with
#'   `NA` category columns.
#' @export
qc_list_categories <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .query(project$con, "
    SELECT cat.id   AS category_id,
           cat.name AS category_name,
           c.id     AS code_id,
           c.name   AS code_name,
           c.color  AS code_color
    FROM   code_categories cat
    LEFT   JOIN code_category_links l
           ON l.category_id = cat.id AND l.status = 1
    LEFT   JOIN codes c
           ON c.id = l.code_id AND c.status = 1
    WHERE  cat.status = 1
    ORDER  BY cat.name, c.name
  ")
}

#' Assign a code to a category
#'
#' @param project A `qc_project` object.
#' @param code_id Integer.
#' @param category_id Integer.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_link_code_category <- function(project, code_id, category_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "INSERT INTO code_category_links (code_id, category_id)
     VALUES (?, ?)
     ON CONFLICT (code_id, category_id) DO UPDATE SET status = 1",
    list(as.integer(code_id), as.integer(category_id))
  )
  invisible(TRUE)
}

#' Remove a code from a category
#'
#' @param project A `qc_project` object.
#' @param code_id Integer.
#' @param category_id Integer.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_unlink_code_category <- function(project, code_id, category_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE code_category_links SET status = 0
     WHERE code_id = ? AND category_id = ?",
    list(as.integer(code_id), as.integer(category_id))
  )
  invisible(TRUE)
}

#' Merge one or more codes into a surviving code
#'
#' All codings from `from_ids` are reassigned to `into_id`. The merged-away
#' codes are soft-deleted. A `'merge'` event is written to `code_history` for
#' each affected code so the operation is fully auditable.
#'
#' @param project A `qc_project` object.
#' @param from_ids Integer vector. Codes to merge away (will be deleted).
#' @param into_id Integer. The surviving code that absorbs all codings.
#'
#' @return Invisibly, a one-row tibble for the surviving code.
#' @export
qc_merge_codes <- function(project, from_ids, into_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  from_ids <- as.integer(from_ids)
  into_id  <- as.integer(into_id)
  if (any(from_ids == into_id))
    rlang::abort("`from_ids` must not contain `into_id`.")

  into <- .query(project$con,
    "SELECT name FROM codes WHERE id = ? AND status = 1", list(into_id))
  if (nrow(into) == 0L)
    rlang::abort(paste0("No active code with id = ", into_id))
  into_name <- into$name[[1L]]

  from_codes <- .query(project$con, paste0(
    "SELECT id, name FROM codes WHERE status = 1",
    .in_clause("id", from_ids)
  ))
  if (nrow(from_codes) == 0L)
    rlang::abort("No active codes found for `from_ids`.")

  for (i in seq_len(nrow(from_codes))) {
    fid   <- from_codes$id[[i]]
    fname <- from_codes$name[[i]]
    .exec(project$con,
      "UPDATE codings SET code_id = ? WHERE code_id = ? AND status = 1",
      list(into_id, fid))
    .log_code_history(project$con, fid, "merge",
                      field     = "merged_into",
                      old_value = fname,
                      new_value = into_name)
    .soft_delete(project$con, "codes", "id", fid)
  }

  .log_code_history(project$con, into_id, "merge",
                    field     = "merged_from",
                    new_value = paste(from_codes$name, collapse = ", "))

  invisible(.query(project$con,
    "SELECT id, name, color, memo FROM codes WHERE id = ?", list(into_id)))
}

#' Split a code into new codes
#'
#' Creates `length(new_names)` new codes and logs the split in `code_history`.
#' The original code is left intact -- use `qc_reassign_coding()` or the
#' Codebook "Review Codings" panel to move passages to the new codes, then
#' delete the original when done.
#'
#' @param project A `qc_project` object.
#' @param code_id Integer. The code to split.
#' @param new_names Character vector (length >= 2). Names for the new codes.
#' @param colors Character vector. Hex colours; recycled or defaulted to
#'   `"#4E79A7"`.
#' @param memos Character vector. Memos; recycled or defaulted to `""`.
#'
#' @return A tibble of the newly created codes.
#' @export
qc_split_code <- function(project, code_id, new_names,
                           colors = NULL, memos = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  code_id   <- as.integer(code_id)
  new_names <- as.character(new_names)
  if (length(new_names) < 2L)
    rlang::abort("`new_names` must have at least 2 elements.")

  current <- .query(project$con,
    "SELECT name FROM codes WHERE id = ? AND status = 1", list(code_id))
  if (nrow(current) == 0L)
    rlang::abort(paste0("No active code with id = ", code_id))

  colors <- colors %||% rep("#4E79A7", length(new_names))
  memos  <- memos  %||% rep("",        length(new_names))

  new_rows <- vector("list", length(new_names))
  for (i in seq_along(new_names)) {
    new_rows[[i]] <- qc_add_code(project, new_names[[i]],
                                  color = colors[[i]],
                                  memo  = memos[[i]])
    .log_code_history(project$con, new_rows[[i]]$id, "split",
                      field     = "split_from",
                      old_value = current$name[[1L]])
  }

  .log_code_history(project$con, code_id, "split",
                    field     = "split_into",
                    new_value = paste(new_names, collapse = ", "))

  do.call(rbind, new_rows)
}

#' Assign a stable key to a code
#'
#' Sets a human-readable slug on a code that stays constant across renames.
#' Keys must be unique across all active codes and may contain only lowercase
#' letters, digits, and underscores.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Code id.
#' @param key Character. The key to assign (e.g. `"positive_affect"`).
#'
#' @return Invisibly, `key`.
#' @export
qc_set_code_key <- function(project, id, key) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id  <- as.integer(id)
  key <- as.character(key)
  if (!grepl("^[a-z0-9_]+$", key))
    rlang::abort("`key` may only contain lowercase letters, digits, and underscores.")

  conflict <- .query(project$con,
    "SELECT id FROM codes WHERE code_key = ? AND id != ? AND status = 1",
    list(key, id))
  if (nrow(conflict) > 0L)
    rlang::abort(paste0("code_key '", key, "' is already used by another code."))

  .exec(project$con,
    "UPDATE codes SET code_key = ? WHERE id = ? AND status = 1",
    list(key, id))
  .log_code_history(project$con, id, "update",
                    field = "code_key", new_value = key)
  invisible(key)
}

#' Mark a code as deprecated
#'
#' Deprecated codes are retained and their historical codings are preserved,
#' but [qc_add_coding()] will reject new codings against them. Reverse with
#' [qc_undeprecate_code()].
#'
#' @param project A `qc_project` object.
#' @param id Integer. Code id.
#' @param reason Character. Optional explanation (stored and shown in exports).
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_deprecate_code <- function(project, id, reason = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id <- as.integer(id)

  current <- .query(project$con,
    "SELECT name, deprecated FROM codes WHERE id = ? AND status = 1",
    list(id))
  if (nrow(current) == 0L)
    rlang::abort(paste0("No active code with id = ", id))
  if (current$deprecated[[1L]] == 1L)
    cli::cli_warn("Code {.val {current$name[[1L]]}} is already deprecated.")

  .exec(project$con,
    "UPDATE codes SET deprecated = 1, deprecated_reason = ?
     WHERE id = ? AND status = 1",
    list(reason %||% "", id))
  .log_code_history(project$con, id, "deprecate",
                    field     = "deprecated",
                    old_value = "0",
                    new_value = if (nchar(reason %||% "") > 0L)
                      paste0("1: ", reason) else "1")
  invisible(TRUE)
}

#' Restore a deprecated code to active status
#'
#' Clears the deprecated flag so new codings can be applied again.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Code id.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_undeprecate_code <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id <- as.integer(id)

  current <- .query(project$con,
    "SELECT name, deprecated FROM codes WHERE id = ? AND status = 1",
    list(id))
  if (nrow(current) == 0L)
    rlang::abort(paste0("No active code with id = ", id))
  if (current$deprecated[[1L]] == 0L)
    cli::cli_warn("Code {.val {current$name[[1L]]}} is not currently deprecated.")

  .exec(project$con,
    "UPDATE codes SET deprecated = 0, deprecated_reason = ''
     WHERE id = ? AND status = 1",
    list(id))
  .log_code_history(project$con, id, "undeprecate",
                    field     = "deprecated",
                    old_value = "1",
                    new_value = "0")
  invisible(TRUE)
}
