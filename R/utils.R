`%||%` <- function(x, y) if (is.null(x)) y else x

assert_class <- function(x, cls, arg = deparse(substitute(x))) {
  if (!inherits(x, cls))
    rlang::abort(paste0("`", arg, "` must be <", cls, ">, not <", class(x)[1L], ">"))
  invisible(x)
}

is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
is_count  <- function(x) is.numeric(x) && length(x) == 1L && x >= 0 && x == floor(x)

assert_con <- function(con) {
  if (!DBI::dbIsValid(con))
    rlang::abort("Database connection is closed. Re-open with `qc_open()`.")
  if (!DBI::dbExistsTable(con, "project_meta"))
    rlang::abort("Connection does not point to a saturate project database.")
  invisible(con)
}

.exec <- function(con, sql, params = NULL) {
  if (is.null(params))
    DBI::dbExecute(con, sql)
  else
    DBI::dbExecute(con, sql, params = params)
}

.query <- function(con, sql, params = NULL) {
  if (is.null(params))
    tibble::as_tibble(DBI::dbGetQuery(con, sql))
  else
    tibble::as_tibble(DBI::dbGetQuery(con, sql, params = params))
}

.soft_delete <- function(con, table, id_col, id_val) {
  .exec(con,
    paste0("UPDATE ", table, " SET status = 0 WHERE ", id_col, " = ?"),
    list(id_val)
  )
}

# Build an IN clause from a validated integer vector.
# Returns a fragment like "AND col IN (1,2,3)" or "" if ids is NULL.
.in_clause <- function(col, ids) {
  if (is.null(ids)) return("")
  ids <- as.integer(ids)
  paste0("AND ", col, " IN (", paste(ids, collapse = ","), ")")
}

# Add a column to a table only if it does not already exist.
# DuckDB does not support ADD COLUMN IF NOT EXISTS; use information_schema.
.add_column_if_missing <- function(con, table, col_name, col_def) {
  exists <- DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) AS n FROM information_schema.columns ",
    "WHERE table_name = '", table, "' AND column_name = '", col_name, "'"
  ))$n
  if (exists == 0L)
    DBI::dbExecute(con,
      paste0("ALTER TABLE ", table, " ADD COLUMN ", col_name, " ", col_def))
  invisible(NULL)
}

# Build INTERSECT sub-query for AND semantics across code_ids.
.must_have_clause <- function(ids) {
  if (is.null(ids) || length(ids) == 0L) return("")
  ids <- as.integer(ids)
  subs <- vapply(ids, function(id) {
    paste0("SELECT source_id FROM codings WHERE code_id = ", id,
           " AND status = 1")
  }, character(1L))
  paste0("AND cod.source_id IN (", paste(subs, collapse = " INTERSECT "), ")")
}
