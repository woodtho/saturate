# Schema DDL — each statement run individually (DuckDB does not support
# multi-statement dbExecute calls).
.ddl <- c(
  # Sequences
  seq_sources    = "CREATE SEQUENCE IF NOT EXISTS sources_id_seq START 1",
  seq_codes      = "CREATE SEQUENCE IF NOT EXISTS codes_id_seq START 1",
  seq_codecats   = "CREATE SEQUENCE IF NOT EXISTS code_categories_id_seq START 1",
  seq_codings    = "CREATE SEQUENCE IF NOT EXISTS codings_id_seq START 1",
  seq_doccats    = "CREATE SEQUENCE IF NOT EXISTS document_categories_id_seq START 1",
  seq_cases      = "CREATE SEQUENCE IF NOT EXISTS cases_id_seq START 1",
  seq_caseattrs  = "CREATE SEQUENCE IF NOT EXISTS case_attributes_id_seq START 1",
  seq_srcattrs   = "CREATE SEQUENCE IF NOT EXISTS source_attributes_id_seq START 1",
  seq_annots        = "CREATE SEQUENCE IF NOT EXISTS annotations_id_seq START 1",
  seq_code_history  = "CREATE SEQUENCE IF NOT EXISTS code_history_id_seq START 1",
  seq_snapshots     = "CREATE SEQUENCE IF NOT EXISTS codebook_snapshots_id_seq START 1",
  seq_autorules     = "CREATE SEQUENCE IF NOT EXISTS auto_coding_rules_id_seq START 1",

  # project_meta — single-row KV store
  tbl_meta = "
    CREATE TABLE IF NOT EXISTS project_meta (
      key   VARCHAR PRIMARY KEY,
      value VARCHAR NOT NULL DEFAULT ''
    )
  ",

  # sources
  tbl_sources = "
    CREATE TABLE IF NOT EXISTS sources (
      id          BIGINT PRIMARY KEY DEFAULT nextval('sources_id_seq'),
      name        VARCHAR NOT NULL,
      content     VARCHAR NOT NULL DEFAULT '',
      memo        VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1,
      created_at  TIMESTAMPTZ DEFAULT now()
    )
  ",

  # codes
  tbl_codes = "
    CREATE TABLE IF NOT EXISTS codes (
      id          BIGINT PRIMARY KEY DEFAULT nextval('codes_id_seq'),
      name        VARCHAR NOT NULL UNIQUE,
      color       VARCHAR NOT NULL DEFAULT '#4E79A7',
      memo        VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1,
      created_at  TIMESTAMPTZ DEFAULT now()
    )
  ",

  # code_categories
  tbl_codecats = "
    CREATE TABLE IF NOT EXISTS code_categories (
      id          BIGINT PRIMARY KEY DEFAULT nextval('code_categories_id_seq'),
      name        VARCHAR NOT NULL UNIQUE,
      memo        VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1,
      created_at  TIMESTAMPTZ DEFAULT now()
    )
  ",

  # code_category_links
  tbl_code_cat_links = "
    CREATE TABLE IF NOT EXISTS code_category_links (
      code_id     BIGINT NOT NULL,
      category_id BIGINT NOT NULL,
      status      INTEGER NOT NULL DEFAULT 1,
      PRIMARY KEY (code_id, category_id)
    )
  ",

  # codings — selfirst/selast are 1-based, both inclusive
  tbl_codings = "
    CREATE TABLE IF NOT EXISTS codings (
      id          BIGINT PRIMARY KEY DEFAULT nextval('codings_id_seq'),
      source_id   BIGINT NOT NULL,
      code_id     BIGINT NOT NULL,
      selfirst    INTEGER NOT NULL,
      selast      INTEGER NOT NULL,
      seltext     VARCHAR NOT NULL DEFAULT '',
      memo        VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1,
      created_at  TIMESTAMPTZ DEFAULT now()
    )
  ",
  idx_codings_src  = "CREATE INDEX IF NOT EXISTS idx_codings_source ON codings(source_id)",
  idx_codings_code = "CREATE INDEX IF NOT EXISTS idx_codings_code   ON codings(code_id)",

  # document_categories
  tbl_doccats = "
    CREATE TABLE IF NOT EXISTS document_categories (
      id          BIGINT PRIMARY KEY DEFAULT nextval('document_categories_id_seq'),
      name        VARCHAR NOT NULL UNIQUE,
      memo        VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1
    )
  ",

  # document_category_links
  tbl_doc_cat_links = "
    CREATE TABLE IF NOT EXISTS document_category_links (
      source_id   BIGINT NOT NULL,
      category_id BIGINT NOT NULL,
      status      INTEGER NOT NULL DEFAULT 1,
      PRIMARY KEY (source_id, category_id)
    )
  ",

  # cases
  tbl_cases = "
    CREATE TABLE IF NOT EXISTS cases (
      id          BIGINT PRIMARY KEY DEFAULT nextval('cases_id_seq'),
      name        VARCHAR NOT NULL UNIQUE,
      memo        VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1,
      created_at  TIMESTAMPTZ DEFAULT now()
    )
  ",

  # case_source_links
  tbl_case_src_links = "
    CREATE TABLE IF NOT EXISTS case_source_links (
      case_id     BIGINT NOT NULL,
      source_id   BIGINT NOT NULL,
      status      INTEGER NOT NULL DEFAULT 1,
      PRIMARY KEY (case_id, source_id)
    )
  ",

  # case_attributes — EAV; UNIQUE on (case_id, variable) for upsert
  tbl_caseattrs = "
    CREATE TABLE IF NOT EXISTS case_attributes (
      id          BIGINT PRIMARY KEY DEFAULT nextval('case_attributes_id_seq'),
      case_id     BIGINT NOT NULL,
      variable    VARCHAR NOT NULL,
      value       VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1,
      UNIQUE (case_id, variable)
    )
  ",

  # source_attributes — EAV
  tbl_srcattrs = "
    CREATE TABLE IF NOT EXISTS source_attributes (
      id          BIGINT PRIMARY KEY DEFAULT nextval('source_attributes_id_seq'),
      source_id   BIGINT NOT NULL,
      variable    VARCHAR NOT NULL,
      value       VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1,
      UNIQUE (source_id, variable)
    )
  ",

  # annotations — optional character-offset memos on a document
  tbl_annots = "
    CREATE TABLE IF NOT EXISTS annotations (
      id          BIGINT PRIMARY KEY DEFAULT nextval('annotations_id_seq'),
      source_id   BIGINT NOT NULL,
      position    INTEGER,
      annotation  VARCHAR NOT NULL,
      status      INTEGER NOT NULL DEFAULT 1,
      created_at  TIMESTAMPTZ DEFAULT now()
    )
  ",

  # code_history — append-only audit log for code mutations
  tbl_code_history = "
    CREATE TABLE IF NOT EXISTS code_history (
      id          BIGINT PRIMARY KEY
                    DEFAULT nextval('code_history_id_seq'),
      code_id     BIGINT NOT NULL,
      operation   VARCHAR NOT NULL,
      field       VARCHAR,
      old_value   VARCHAR,
      new_value   VARCHAR,
      changed_at  TIMESTAMPTZ DEFAULT now()
    )
  ",

  # codebook_snapshots — point-in-time codebook JSON snapshots
  tbl_snapshots = "
    CREATE TABLE IF NOT EXISTS codebook_snapshots (
      id            BIGINT PRIMARY KEY
                      DEFAULT nextval('codebook_snapshots_id_seq'),
      label         VARCHAR NOT NULL DEFAULT '',
      snapshot_json VARCHAR NOT NULL,
      created_at    TIMESTAMPTZ DEFAULT now()
    )
  ",

  # auto_coding_rules — stored regex rules for automatic coding
  tbl_autorules = "
    CREATE TABLE IF NOT EXISTS auto_coding_rules (
      id          BIGINT PRIMARY KEY
                    DEFAULT nextval('auto_coding_rules_id_seq'),
      name        VARCHAR NOT NULL,
      pattern     VARCHAR NOT NULL,
      code_id     BIGINT NOT NULL,
      ignore_case INTEGER NOT NULL DEFAULT 1,
      status      INTEGER NOT NULL DEFAULT 1,
      created_at  TIMESTAMPTZ DEFAULT now()
    )
  "
)

.bootstrap_schema <- function(con) {
  for (sql in .ddl) {
    DBI::dbExecute(con, sql)
  }

  # Idempotent column additions for projects created before these fields existed
  .add_column_if_missing(con, "codes",   "parent_id",   "BIGINT")
  .add_column_if_missing(con, "codes",   "definition",  "VARCHAR NOT NULL DEFAULT ''")
  .add_column_if_missing(con, "codes",   "criteria",    "VARCHAR NOT NULL DEFAULT ''")
  .add_column_if_missing(con, "codings", "coder",
                         "VARCHAR NOT NULL DEFAULT 'default'")
  .add_column_if_missing(con, "codings", "coding_source",
                         "VARCHAR NOT NULL DEFAULT 'manual'")
  .add_column_if_missing(con, "codings", "coding_status",
                         "VARCHAR NOT NULL DEFAULT 'validated'")

  # Seed project_meta if empty
  existing <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM project_meta")$n
  if (existing == 0L) {
    DBI::dbExecute(con,
      "INSERT INTO project_meta (key, value) VALUES
         ('name',       'Untitled Project'),
         ('owner',      ''),
         ('memo',       ''),
         ('created_at', CAST(now() AS VARCHAR))"
    )
  }
  invisible(con)
}

#' Create a new qualcoder project
#'
#' Initialises a DuckDB database at `path` and bootstraps the schema.
#'
#' @param path Character. Path to the `.duckdb` file to create.
#' @param name Character. Project name stored in metadata.
#' @param owner Character. Owner name.
#' @param overwrite Logical. Overwrite an existing file if `TRUE`.
#'
#' @return A `qc_project` object (invisibly).
#' @export
qc_new <- function(path,
                   name      = fs::path_ext_remove(fs::path_file(path)),
                   owner     = Sys.info()[["user"]],
                   overwrite = FALSE) {
  if (!is_string(path)) rlang::abort("`path` must be a single string.")
  path <- fs::path_abs(path)
  if (fs::file_exists(path)) {
    if (!overwrite) rlang::abort(paste0("File already exists: ", path,
                                        "\nUse `overwrite = TRUE` to replace it."))
    fs::file_delete(path)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  .bootstrap_schema(con)
  .exec(con,
    "UPDATE project_meta SET value = ? WHERE key = 'name'",  list(name))
  .exec(con,
    "UPDATE project_meta SET value = ? WHERE key = 'owner'", list(owner))
  proj <- .make_project(con, path)
  cli::cli_alert_success("Created project {.file {path}}")
  invisible(proj)
}

#' Open an existing qualcoder project
#'
#' @param path Character. Path to an existing `.duckdb` file.
#' @param read_only Logical. Open in read-only mode.
#'
#' @return A `qc_project` object.
#' @export
qc_open <- function(path, read_only = FALSE) {
  if (!is_string(path)) rlang::abort("`path` must be a single string.")
  path <- fs::path_abs(path)
  if (!fs::file_exists(path))
    rlang::abort(paste0("File not found: ", path))
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
  if (!DBI::dbExistsTable(con, "project_meta")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    rlang::abort("Not a qualcoder project (project_meta table missing).")
  }
  proj <- .make_project(con, path)
  cli::cli_alert_success("Opened project {.file {path}}")
  invisible(proj)
}

#' Close a qualcoder project connection
#'
#' @param project A `qc_project` object.
#' @export
qc_close <- function(project) {
  assert_class(project, "qc_project")
  DBI::dbDisconnect(project$con, shutdown = TRUE)
  cli::cli_alert_info("Project closed.")
  invisible(NULL)
}

#' Retrieve or update project-level metadata
#'
#' @param project A `qc_project` object.
#' @param name,owner,memo Character scalars. Pass non-`NULL` to update.
#'
#' @return A one-row tibble: `name`, `owner`, `memo`, `created_at`.
#' @export
qc_project_info <- function(project, name = NULL, owner = NULL, memo = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  updates <- list(name = name, owner = owner, memo = memo)
  for (key in names(updates)) {
    val <- updates[[key]]
    if (!is.null(val)) {
      if (!is_string(val)) rlang::abort(paste0("`", key, "` must be a single string."))
      .exec(project$con,
        "UPDATE project_meta SET value = ? WHERE key = ?",
        list(val, key)
      )
    }
  }
  raw <- .query(project$con, "SELECT key, value FROM project_meta")
  tibble::tibble(
    name       = raw$value[raw$key == "name"],
    owner      = raw$value[raw$key == "owner"],
    memo       = raw$value[raw$key == "memo"],
    created_at = raw$value[raw$key == "created_at"]
  )
}

.make_project <- function(con, path) {
  structure(list(con = con, path = path), class = "qc_project")
}

#' @export
print.qc_project <- function(x, ...) {
  if (!DBI::dbIsValid(x$con)) {
    cli::cli_text("<qc_project> [closed] {.file {x$path}}")
    return(invisible(x))
  }
  n_docs  <- DBI::dbGetQuery(x$con,
    "SELECT COUNT(*) AS n FROM sources WHERE status = 1")$n
  n_codes <- DBI::dbGetQuery(x$con,
    "SELECT COUNT(*) AS n FROM codes WHERE status = 1")$n
  info    <- .query(x$con, "SELECT key, value FROM project_meta WHERE key IN ('name','owner')")
  proj_name  <- info$value[info$key == "name"]
  proj_owner <- info$value[info$key == "owner"]
  cli::cli_text("<qc_project> {.strong {proj_name}} [{proj_owner}]")
  cli::cli_text("  Path:      {.file {x$path}}")
  cli::cli_text("  Documents: {n_docs}  |  Codes: {n_codes}")
  invisible(x)
}
