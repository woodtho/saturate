#' Create a new analytical theme
#'
#' A theme is a patterned, coherent meaning that addresses the research
#' question. It integrates multiple categories or code clusters and should
#' be expressed as an analytical statement (proposition), not merely a label.
#'
#' @param project A `qc_project` object.
#' @param name Character. Short theme label.
#' @param central_concept Character. The central organizing idea in one sentence.
#' @param narrative Character. Extended analytical justification / proposition.
#' @param definition Character. What counts as belonging to this theme.
#' @param scope Character. Inclusion/exclusion criteria.
#' @param code_ids Integer vector or `NULL`. Direct code links to create.
#' @param category_ids Integer vector or `NULL`. Category links to create.
#' @param created_by Character or `NULL`. Defaults to the current system user.
#'
#' @return A one-row tibble: `id`, `name`, `created_at`.
#' @export
qc_add_theme <- function(project, name,
                          central_concept = "",
                          narrative       = "",
                          definition      = "",
                          scope           = "",
                          code_ids        = NULL,
                          category_ids    = NULL,
                          created_by      = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  if (!is_string(name) || nchar(trimws(name)) == 0L)
    rlang::abort("`name` must be a non-empty string.")

  row <- .query(project$con,
    "INSERT INTO themes (name, central_concept, narrative, definition, scope)
     VALUES (?, ?, ?, ?, ?)
     RETURNING id, name, created_at",
    list(trimws(name),
         as.character(central_concept %||% ""),
         as.character(narrative       %||% ""),
         as.character(definition      %||% ""),
         as.character(scope           %||% ""))
  )

  .exec(project$con,
    "INSERT INTO theme_history (theme_id, operation, changed_by) VALUES (?, 'create', ?)",
    list(row$id, as.character(created_by %||% Sys.info()[["user"]]))
  )

  if (!is.null(code_ids) && length(code_ids) > 0L)
    qc_link_theme_codes(project, row$id, as.integer(code_ids))

  if (!is.null(category_ids) && length(category_ids) > 0L)
    qc_link_theme_categories(project, row$id, as.integer(category_ids))

  cli::cli_alert_success("Theme #{row$id} '{name}' created.")
  row
}

#' List all themes
#'
#' @param project A `qc_project` object.
#'
#' @return A tibble: `id`, `name`, `central_concept`, `narrative`,
#'   `definition`, `scope`, `n_categories`, `n_codes`, `status`, `created_at`.
#' @export
qc_list_themes <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .query(project$con,
    "SELECT t.id,
            t.name,
            t.central_concept,
            t.narrative,
            COALESCE(t.definition, '') AS definition,
            COALESCE(t.scope, '')      AS scope,
            t.status,
            t.created_at,
            COUNT(DISTINCT CASE WHEN l.status   = 1 THEN l.code_id     END) AS n_codes,
            COUNT(DISTINCT CASE WHEN tcl.status = 1 THEN tcl.category_id END) AS n_categories
     FROM   themes t
     LEFT JOIN theme_code_links      l   ON l.theme_id   = t.id
     LEFT JOIN theme_category_links  tcl ON tcl.theme_id = t.id
     WHERE  t.status = 1
     GROUP  BY t.id, t.name, t.central_concept, t.narrative,
               t.definition, t.scope, t.status, t.created_at
     ORDER  BY t.name"
  )
}

#' Get full detail for a single theme
#'
#' Returns the theme row together with its linked categories and directly
#' linked codes.
#'
#' @param project A `qc_project` object.
#' @param theme_id Integer. Theme id.
#'
#' @return A list with elements `theme` (1-row tibble), `linked_cats` (tibble),
#'   and `linked_codes` (tibble).
#' @export
qc_get_theme <- function(project, theme_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  theme_id <- as.integer(theme_id)

  theme <- .query(project$con,
    "SELECT id, name, central_concept, narrative,
            COALESCE(definition, '') AS definition,
            COALESCE(scope, '')      AS scope,
            status, created_at
     FROM   themes WHERE id = ? AND status = 1",
    list(theme_id)
  )
  if (nrow(theme) == 0L)
    rlang::abort(paste0("No theme with id = ", theme_id))

  linked_cats <- .query(project$con,
    "SELECT cc.id, cc.name,
            SUM(CASE WHEN ccl.status = 1 THEN 1 ELSE 0 END) AS n_codes
     FROM   theme_category_links tcl
     JOIN   code_categories cc  ON cc.id  = tcl.category_id AND cc.status = 1
     LEFT JOIN code_category_links ccl ON ccl.category_id = cc.id
     WHERE  tcl.theme_id = ? AND tcl.status = 1
     GROUP  BY cc.id, cc.name
     ORDER  BY cc.name",
    list(theme_id)
  )

  linked_codes <- .query(project$con,
    "SELECT c.id, c.name, c.color,
            SUM(CASE WHEN cod.status = 1 THEN 1 ELSE 0 END) AS n_codings
     FROM   theme_code_links tcl
     JOIN   codes   c   ON c.id  = tcl.code_id AND c.status = 1
     LEFT JOIN codings cod ON cod.code_id = c.id
     WHERE  tcl.theme_id = ? AND tcl.status = 1
     GROUP  BY c.id, c.name, c.color
     ORDER  BY c.name",
    list(theme_id)
  )

  list(theme = theme, linked_cats = linked_cats, linked_codes = linked_codes)
}

#' Update a theme's fields
#'
#' Each changed field is recorded in `theme_history` before the update is
#' applied. Fields not supplied (or `NULL`) are left unchanged.
#'
#' @param project A `qc_project` object.
#' @param id Integer. Theme id.
#' @param name,central_concept,narrative,definition,scope Character or `NULL`.
#' @param changed_by Character or `NULL`. Defaults to the current system user.
#'
#' @return Invisibly `NULL`.
#' @export
qc_update_theme <- function(project, id,
                             name            = NULL,
                             central_concept = NULL,
                             narrative       = NULL,
                             definition      = NULL,
                             scope           = NULL,
                             changed_by      = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id         <- as.integer(id)
  changed_by <- as.character(changed_by %||% Sys.info()[["user"]])

  old <- .query(project$con,
    "SELECT name, central_concept, narrative,
            COALESCE(definition, '') AS definition,
            COALESCE(scope, '')      AS scope
     FROM   themes WHERE id = ? AND status = 1",
    list(id)
  )
  if (nrow(old) == 0L)
    rlang::abort(paste0("No theme with id = ", id))

  updates <- list(name = name, central_concept = central_concept,
                  narrative = narrative, definition = definition, scope = scope)

  for (fld in names(updates)) {
    val <- updates[[fld]]
    if (is.null(val)) next
    old_val <- as.character(old[[fld]])
    new_val <- as.character(val)
    if (identical(old_val, new_val)) next
    .exec(project$con,
      paste0("UPDATE themes SET ", fld, " = ? WHERE id = ? AND status = 1"),
      list(new_val, id)
    )
    .exec(project$con,
      "INSERT INTO theme_history
         (theme_id, operation, field, old_value, new_value, changed_by)
       VALUES (?, 'update', ?, ?, ?, ?)",
      list(id, fld, old_val, new_val, changed_by)
    )
  }
  invisible(NULL)
}

#' Delete a theme (soft delete)
#'
#' @param project A `qc_project` object.
#' @param id Integer. Theme id.
#'
#' @return Invisibly `NULL`.
#' @export
qc_delete_theme <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  id <- as.integer(id)
  .exec(project$con,
    "UPDATE themes SET status = 0 WHERE id = ? AND status = 1",
    list(id))
  .exec(project$con,
    "INSERT INTO theme_history (theme_id, operation, changed_by) VALUES (?, 'delete', ?)",
    list(id, Sys.info()[["user"]])
  )
  invisible(NULL)
}

#' Link codes to a theme
#'
#' Re-linking a previously unlinked code restores the association without
#' creating a duplicate row.
#'
#' @param project A `qc_project` object.
#' @param theme_id Integer. Theme id.
#' @param code_ids Integer vector. One or more code ids.
#'
#' @return Invisibly `NULL`.
#' @export
qc_link_theme_codes <- function(project, theme_id, code_ids) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  theme_id <- as.integer(theme_id)
  for (cid in as.integer(code_ids)) {
    .exec(project$con,
      "INSERT INTO theme_code_links (theme_id, code_id, status)
       VALUES (?, ?, 1)
       ON CONFLICT (theme_id, code_id) DO UPDATE SET status = 1",
      list(theme_id, cid)
    )
  }
  invisible(NULL)
}

#' Unlink a code from a theme
#'
#' @param project A `qc_project` object.
#' @param theme_id Integer. Theme id.
#' @param code_id Integer. Code id.
#'
#' @return Invisibly `NULL`.
#' @export
qc_unlink_theme_code <- function(project, theme_id, code_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE theme_code_links SET status = 0 WHERE theme_id = ? AND code_id = ?",
    list(as.integer(theme_id), as.integer(code_id)))
  invisible(NULL)
}

#' Link categories to a theme
#'
#' All codes in a linked category are included when computing theme excerpts
#' and the structure view. Re-linking a previously unlinked category restores
#' without duplicating.
#'
#' @param project A `qc_project` object.
#' @param theme_id Integer. Theme id.
#' @param category_ids Integer vector. One or more category ids.
#'
#' @return Invisibly `NULL`.
#' @export
qc_link_theme_categories <- function(project, theme_id, category_ids) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  theme_id <- as.integer(theme_id)
  for (cid in as.integer(category_ids)) {
    .exec(project$con,
      "INSERT INTO theme_category_links (theme_id, category_id, status)
       VALUES (?, ?, 1)
       ON CONFLICT (theme_id, category_id) DO UPDATE SET status = 1",
      list(theme_id, cid)
    )
  }
  invisible(NULL)
}

#' Unlink a category from a theme
#'
#' @param project A `qc_project` object.
#' @param theme_id Integer. Theme id.
#' @param category_id Integer. Category id.
#'
#' @return Invisibly `NULL`.
#' @export
qc_unlink_theme_category <- function(project, theme_id, category_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE theme_category_links SET status = 0
     WHERE  theme_id = ? AND category_id = ?",
    list(as.integer(theme_id), as.integer(category_id)))
  invisible(NULL)
}

#' Retrieve all coded excerpts for a theme
#'
#' Returns every active coding whose code belongs to this theme — either via a
#' direct code link or via a linked category. Use this for internal homogeneity
#' checks: all passages should cohere around the theme's central concept.
#'
#' @param project A `qc_project` object.
#' @param theme_id Integer. Theme id.
#'
#' @return A tibble: `id`, `source_id`, `code_id`, `selfirst`, `selast`,
#'   `seltext`, `memo`, `coder`, `code_name`, `code_color`, `doc_name`,
#'   `source_type`.
#' @export
qc_theme_excerpts <- function(project, theme_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  theme_id <- as.integer(theme_id)

  .query(project$con,
    "SELECT cod.id,
            cod.source_id,
            cod.code_id,
            cod.selfirst,
            cod.selast,
            cod.seltext,
            cod.memo,
            cod.coder,
            c.name  AS code_name,
            c.color AS code_color,
            s.name  AS doc_name,
            s.source_type
     FROM   codings cod
     JOIN   codes   c ON c.id  = cod.code_id   AND c.status = 1
     JOIN   sources s ON s.id  = cod.source_id AND s.status = 1
     WHERE  cod.status = 1
       AND  cod.code_id IN (
              SELECT code_id FROM theme_code_links
              WHERE  theme_id = ? AND status = 1
              UNION
              SELECT ccl.code_id
              FROM   code_category_links ccl
              JOIN   theme_category_links tcl
                       ON tcl.category_id = ccl.category_id
                      AND tcl.theme_id    = ? AND tcl.status = 1
              WHERE  ccl.status = 1
            )
     ORDER  BY s.name, cod.selfirst",
    list(theme_id, theme_id)
  )
}
