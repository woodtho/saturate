#' Add a non-hierarchical relationship between two codes
#'
#' Records a directed or symmetric semantic link between codes, e.g.
#' `"related_to"`, `"broader_than"`, `"narrower_than"`, or
#' `"co_occurs_with"`. The relation is stored as a directed edge
#' (`code_id_1 -> code_id_2`), but [qc_list_code_relations()] returns
#' both directions when filtering by a single code.
#'
#' @param project A `qc_project` object.
#' @param code_id_1,code_id_2 Integer. The two codes to link.
#' @param relation_type Character. A short label for the relationship.
#'   Recommended vocabulary: `"related_to"`, `"broader_than"`,
#'   `"narrower_than"`, `"co_occurs_with"`, `"contradicts"`,
#'   `"precedes"`. Any string is accepted.
#' @param note Character. Optional explanation of the relationship.
#'
#' @return A one-row tibble: `id`, `code_id_1`, `name_1`, `code_id_2`,
#'   `name_2`, `relation_type`, `note`, `created_at`.
#' @export
qc_add_code_relation <- function(project, code_id_1, code_id_2,
                                  relation_type, note = "") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  if (!is_string(relation_type))
    rlang::abort("`relation_type` must be a single string.")
  code_id_1 <- as.integer(code_id_1)
  code_id_2 <- as.integer(code_id_2)
  if (code_id_1 == code_id_2)
    rlang::abort("A code cannot be related to itself.")

  row <- .query(project$con,
    "INSERT INTO code_relations (code_id_1, code_id_2, relation_type, note)
     VALUES (?, ?, ?, ?)
     RETURNING id, code_id_1, code_id_2, relation_type, note, created_at",
    list(code_id_1, code_id_2, relation_type, note %||% "")
  )

  names <- .query(project$con,
    "SELECT id, name FROM codes WHERE id IN (?, ?) AND status = 1",
    list(code_id_1, code_id_2)
  )
  row$name_1 <- names$name[names$id == code_id_1]
  row$name_2 <- names$name[names$id == code_id_2]
  row[, c("id", "code_id_1", "name_1", "code_id_2", "name_2",
          "relation_type", "note", "created_at")]
}

#' List code relations
#'
#' @param project A `qc_project` object.
#' @param code_id Integer or `NULL`. When supplied, returns all relations
#'   where this code appears on either side.
#'
#' @return A tibble: `id`, `code_id_1`, `name_1`, `code_id_2`, `name_2`,
#'   `relation_type`, `note`, `created_at`.
#' @export
qc_list_code_relations <- function(project, code_id = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  w_code <- if (!is.null(code_id)) {
    cid <- as.integer(code_id)
    paste0("AND (r.code_id_1 = ", cid, " OR r.code_id_2 = ", cid, ")")
  } else ""

  .query(project$con, paste0("
    SELECT r.id,
           r.code_id_1,  c1.name AS name_1,
           r.code_id_2,  c2.name AS name_2,
           r.relation_type, r.note, r.created_at
    FROM   code_relations r
    JOIN   codes c1 ON c1.id = r.code_id_1
    JOIN   codes c2 ON c2.id = r.code_id_2
    WHERE  r.status = 1 ", w_code, "
    ORDER  BY c1.name, r.relation_type, c2.name
  "))
}

#' Delete a code relation (soft delete)
#'
#' @param project A `qc_project` object.
#' @param id Integer. Relation id.
#'
#' @return Invisibly `1L`.
#' @export
qc_delete_code_relation <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  .soft_delete(project$con, "code_relations", "id", as.integer(id))
  invisible(1L)
}
