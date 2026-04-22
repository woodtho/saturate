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
  seq_srcvers       = "CREATE SEQUENCE IF NOT EXISTS source_versions_id_seq START 1",
  seq_code_rels     = "CREATE SEQUENCE IF NOT EXISTS code_relations_id_seq START 1",
  seq_coding_audit      = "CREATE SEQUENCE IF NOT EXISTS coding_audit_id_seq START 1",
  seq_member_checks     = "CREATE SEQUENCE IF NOT EXISTS member_checks_id_seq START 1",
  seq_member_check_items = "CREATE SEQUENCE IF NOT EXISTS member_check_items_id_seq START 1",

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
      id            BIGINT PRIMARY KEY DEFAULT nextval('sources_id_seq'),
      name          VARCHAR NOT NULL,
      content       VARCHAR NOT NULL DEFAULT '',
      memo          VARCHAR NOT NULL DEFAULT '',
      status        INTEGER NOT NULL DEFAULT 1,
      created_at    TIMESTAMPTZ DEFAULT now(),
      filename      VARCHAR DEFAULT '',
      source_system VARCHAR DEFAULT 'manual',
      language      VARCHAR DEFAULT '',
      doc_version   INTEGER DEFAULT 1,
      content_hash  VARCHAR DEFAULT '',
      word_count    INTEGER DEFAULT 0,
      parent_id     BIGINT,
      source_type   VARCHAR DEFAULT ''
    )
  ",

  # source_versions — append-only content history
  tbl_srcvers = "
    CREATE TABLE IF NOT EXISTS source_versions (
      id           BIGINT PRIMARY KEY DEFAULT nextval('source_versions_id_seq'),
      source_id    BIGINT NOT NULL,
      version      INTEGER NOT NULL,
      content      VARCHAR NOT NULL,
      content_hash VARCHAR NOT NULL DEFAULT '',
      memo         VARCHAR NOT NULL DEFAULT '',
      imported_at  TIMESTAMPTZ DEFAULT now(),
      UNIQUE (source_id, version)
    )
  ",
  idx_srcvers = "CREATE INDEX IF NOT EXISTS idx_src_versions_src ON source_versions(source_id)",

  # codes
  tbl_codes = "
    CREATE TABLE IF NOT EXISTS codes (
      id          BIGINT PRIMARY KEY DEFAULT nextval('codes_id_seq'),
      name        VARCHAR NOT NULL UNIQUE,
      color       VARCHAR NOT NULL DEFAULT '#4E79A7',
      memo        VARCHAR NOT NULL DEFAULT '',
      status      INTEGER NOT NULL DEFAULT 1,
      created_at  TIMESTAMPTZ DEFAULT now(),
      parent_id         BIGINT,
      definition        VARCHAR DEFAULT '',
      criteria          VARCHAR DEFAULT '',
      code_key          VARCHAR,
      deprecated        INTEGER NOT NULL DEFAULT 0,
      deprecated_reason VARCHAR NOT NULL DEFAULT '',
      weight            DOUBLE,
      weight_description VARCHAR NOT NULL DEFAULT ''
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
      id             BIGINT PRIMARY KEY DEFAULT nextval('codings_id_seq'),
      source_id      BIGINT NOT NULL,
      code_id        BIGINT NOT NULL,
      selfirst       INTEGER NOT NULL,
      selast         INTEGER NOT NULL,
      seltext        VARCHAR NOT NULL DEFAULT '',
      memo           VARCHAR NOT NULL DEFAULT '',
      status         INTEGER NOT NULL DEFAULT 1,
      created_at     TIMESTAMPTZ DEFAULT now(),
      coder          VARCHAR DEFAULT 'default',
      coding_source  VARCHAR DEFAULT 'manual',
      coding_status  VARCHAR DEFAULT 'validated',
      confidence     INTEGER
    )
  ",

  # code_relations — non-hierarchical relationships between codes
  tbl_code_rels = "
    CREATE TABLE IF NOT EXISTS code_relations (
      id            BIGINT PRIMARY KEY
                      DEFAULT nextval('code_relations_id_seq'),
      code_id_1     BIGINT NOT NULL,
      code_id_2     BIGINT NOT NULL,
      relation_type VARCHAR NOT NULL,
      note          VARCHAR NOT NULL DEFAULT '',
      status        INTEGER NOT NULL DEFAULT 1,
      created_at    TIMESTAMPTZ DEFAULT now()
    )
  ",
  idx_codings_src      = "CREATE INDEX IF NOT EXISTS idx_codings_source    ON codings(source_id)",
  idx_codings_code     = "CREATE INDEX IF NOT EXISTS idx_codings_code      ON codings(code_id)",
  idx_codings_src_stat = "CREATE INDEX IF NOT EXISTS idx_codings_src_stat  ON codings(source_id, status)",
  idx_codings_cod_stat = "CREATE INDEX IF NOT EXISTS idx_codings_cod_stat  ON codings(code_id,   status)",
  idx_codings_cdr_stat = "CREATE INDEX IF NOT EXISTS idx_codings_cdr_stat  ON codings(coder,     status)",
  idx_codes_status     = "CREATE INDEX IF NOT EXISTS idx_codes_status       ON codes(status)",
  idx_sources_status   = "CREATE INDEX IF NOT EXISTS idx_sources_status     ON sources(status)",

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
  idx_code_hist_code = "CREATE INDEX IF NOT EXISTS idx_code_hist_code ON code_history(code_id)",

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
  ",

  # coding_audit — append-only log of every coding operation
  tbl_coding_audit = "
    CREATE TABLE IF NOT EXISTS coding_audit (
      id          BIGINT PRIMARY KEY
                    DEFAULT nextval('coding_audit_id_seq'),
      coding_id   BIGINT NOT NULL,
      source_id   BIGINT NOT NULL,
      code_id     BIGINT NOT NULL,
      operation   VARCHAR NOT NULL,
      field       VARCHAR,
      old_value   VARCHAR,
      new_value   VARCHAR,
      selfirst    INTEGER,
      selast      INTEGER,
      seltext     VARCHAR,
      coder       VARCHAR,
      changed_by  VARCHAR,
      changed_at  TIMESTAMPTZ DEFAULT now()
    )
  ",
  idx_coding_audit_coding = "CREATE INDEX IF NOT EXISTS idx_coding_audit_coding ON coding_audit(coding_id)",
  idx_coding_audit_src    = "CREATE INDEX IF NOT EXISTS idx_coding_audit_src    ON coding_audit(source_id)",
  idx_coding_audit_at     = "CREATE INDEX IF NOT EXISTS idx_coding_audit_at     ON coding_audit(changed_at)",

  # member_checks — per-participant review records
  tbl_member_checks = "
    CREATE TABLE IF NOT EXISTS member_checks (
      id                BIGINT PRIMARY KEY DEFAULT nextval('member_checks_id_seq'),
      source_id         BIGINT NOT NULL,
      participant_label VARCHAR NOT NULL DEFAULT '',
      code_ids_filter   VARCHAR NOT NULL DEFAULT '',
      sent_at           TIMESTAMPTZ DEFAULT now(),
      response_at       TIMESTAMPTZ,
      status            VARCHAR NOT NULL DEFAULT 'pending',
      notes             VARCHAR NOT NULL DEFAULT '',
      created_by        VARCHAR NOT NULL DEFAULT ''
    )
  ",
  tbl_member_check_items = "
    CREATE TABLE IF NOT EXISTS member_check_items (
      id                   BIGINT PRIMARY KEY DEFAULT nextval('member_check_items_id_seq'),
      check_id             BIGINT NOT NULL,
      coding_id            BIGINT NOT NULL,
      participant_response VARCHAR NOT NULL DEFAULT '',
      item_status          VARCHAR NOT NULL DEFAULT 'pending',
      response_at          TIMESTAMPTZ
    )
  ",
  idx_mc_source = "CREATE INDEX IF NOT EXISTS idx_mc_source ON member_checks(source_id)",
  idx_mci_check = "CREATE INDEX IF NOT EXISTS idx_mci_check ON member_check_items(check_id)",

  # excerpts — reader passages separate from codings, with optional memo
  seq_excerpts = "CREATE SEQUENCE IF NOT EXISTS excerpts_id_seq START 1",

  tbl_excerpts = "
    CREATE TABLE IF NOT EXISTS excerpts (
      id         BIGINT PRIMARY KEY DEFAULT nextval('excerpts_id_seq'),
      source_id  BIGINT NOT NULL,
      selfirst   INTEGER NOT NULL,
      selast     INTEGER NOT NULL,
      seltext    VARCHAR NOT NULL DEFAULT '',
      memo       VARCHAR NOT NULL DEFAULT '',
      coder      VARCHAR NOT NULL DEFAULT 'default',
      status     INTEGER NOT NULL DEFAULT 1,
      created_at TIMESTAMPTZ DEFAULT now()
    )
  ",
  idx_excerpts_src = "CREATE INDEX IF NOT EXISTS idx_excerpts_source ON excerpts(source_id)",

  # project_memos — append-only analytical / reflexivity journal
  seq_pmemos = "CREATE SEQUENCE IF NOT EXISTS project_memos_id_seq START 1",

  tbl_project_memos = "
    CREATE TABLE IF NOT EXISTS project_memos (
      id         BIGINT PRIMARY KEY DEFAULT nextval('project_memos_id_seq'),
      content    VARCHAR NOT NULL,
      memo_type  VARCHAR NOT NULL DEFAULT 'analytical',
      created_by VARCHAR NOT NULL DEFAULT '',
      status     INTEGER NOT NULL DEFAULT 1,
      created_at TIMESTAMPTZ DEFAULT now()
    )
  ",

  # themes — analytical theme objects (Braun & Clarke reflexive TA)
  seq_themes          = "CREATE SEQUENCE IF NOT EXISTS themes_id_seq START 1",

  tbl_themes = "
    CREATE TABLE IF NOT EXISTS themes (
      id              BIGINT PRIMARY KEY DEFAULT nextval('themes_id_seq'),
      name            VARCHAR NOT NULL,
      central_concept VARCHAR NOT NULL DEFAULT '',
      narrative       VARCHAR NOT NULL DEFAULT '',
      status          INTEGER NOT NULL DEFAULT 1,
      created_at      TIMESTAMPTZ DEFAULT now()
    )
  ",
  tbl_theme_code_links = "
    CREATE TABLE IF NOT EXISTS theme_code_links (
      theme_id   BIGINT NOT NULL,
      code_id    BIGINT NOT NULL,
      status     INTEGER NOT NULL DEFAULT 1,
      PRIMARY KEY (theme_id, code_id)
    )
  ",
  idx_theme_code = "CREATE INDEX IF NOT EXISTS idx_theme_code ON theme_code_links(theme_id)"
)

.bootstrap_schema <- function(con) {
  for (sql in .ddl) {
    DBI::dbExecute(con, sql)
  }

  # Idempotent column additions for projects created before these fields existed.
  # DuckDB ALTER TABLE ADD COLUMN does not support NOT NULL constraints.
  .add_column_if_missing(con, "codes",   "parent_id",      "BIGINT")
  .add_column_if_missing(con, "codes",   "definition",     "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "codes",   "criteria",       "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "codings", "coder",          "VARCHAR DEFAULT 'default'")
  .add_column_if_missing(con, "codings", "coding_source",  "VARCHAR DEFAULT 'manual'")
  .add_column_if_missing(con, "codings", "coding_status",  "VARCHAR DEFAULT 'validated'")
  .add_column_if_missing(con, "codings", "confidence",     "INTEGER")
  .add_column_if_missing(con, "annotations", "coder",      "VARCHAR DEFAULT 'default'")
  .add_column_if_missing(con, "sources", "filename",       "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "sources", "source_system",  "VARCHAR DEFAULT 'manual'")
  .add_column_if_missing(con, "sources", "language",       "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "sources", "doc_version",    "INTEGER DEFAULT 1")
  .add_column_if_missing(con, "sources", "content_hash",   "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "sources", "word_count",          "INTEGER DEFAULT 0")
  .add_column_if_missing(con, "sources", "parent_id",           "BIGINT")
  .add_column_if_missing(con, "sources", "source_type",         "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "codes",   "code_key",            "VARCHAR")
  .add_column_if_missing(con, "codes",   "deprecated",          "INTEGER DEFAULT 0")
  .add_column_if_missing(con, "codes",   "deprecated_reason",   "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "codes",   "level",               "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "codes",   "orientation",         "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "codes",   "weight",              "DOUBLE")
  .add_column_if_missing(con, "codes",   "weight_description",  "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "member_checks", "return_by",           "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "member_checks", "return_to",           "VARCHAR DEFAULT ''")
  .add_column_if_missing(con, "member_checks", "return_instructions",  "VARCHAR DEFAULT ''")

  # Seed project_meta if empty
  existing <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM project_meta")$n
  if (existing == 0L) {
    DBI::dbExecute(con,
      "INSERT INTO project_meta (key, value) VALUES
         ('name',         'Untitled Project'),
         ('owner',        ''),
         ('memo',         ''),
         ('locked',       '0'),
         ('blind_coding', '0'),
         ('created_at',   CAST(now() AS VARCHAR))"
    )
  } else {
    # Idempotent: add locked key to projects created before this field existed
    DBI::dbExecute(con,
      "INSERT INTO project_meta (key, value)
       VALUES ('locked', '0'), ('blind_coding', '0')
       ON CONFLICT (key) DO NOTHING"
    )
  }
  invisible(con)
}

#' Create a new saturate project
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

#' Open an existing saturate project
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
    rlang::abort("Not a saturate project (project_meta table missing).")
  }
  if (!read_only) .bootstrap_schema(con)
  proj <- .make_project(con, path)
  cli::cli_alert_success("Opened project {.file {path}}")
  invisible(proj)
}

#' Close a saturate project connection
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
#' @return A one-row tibble: `name`, `owner`, `memo`, `created_at`, `locked`.
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
    created_at = raw$value[raw$key == "created_at"],
    locked     = identical(raw$value[raw$key == "locked"], "1")
  )
}

#' Lock or unlock a project against further edits
#'
#' A locked project rejects all write operations (`qc_add_coding`,
#' `qc_import_document`, `qc_add_code`, etc.). Use this to freeze a dataset
#' after finalising coding so downstream exports are reproducible.
#'
#' @param project A `qc_project` object.
#'
#' @return Invisibly `TRUE` (locked) or `FALSE` (unlocked).
#' @export
qc_lock_project <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE project_meta SET value = '1' WHERE key = 'locked'")
  cli::cli_alert_success(
    "Project locked. No edits permitted until `qc_unlock_project()` is called.")
  invisible(TRUE)
}

#' @rdname qc_lock_project
#' @export
qc_unlock_project <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .exec(project$con,
    "UPDATE project_meta SET value = '0' WHERE key = 'locked'")
  cli::cli_alert_info("Project unlocked.")
  invisible(FALSE)
}

#' @rdname qc_lock_project
#' @export
qc_is_locked <- function(project) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  val <- DBI::dbGetQuery(project$con,
    "SELECT value FROM project_meta WHERE key = 'locked'")$value
  identical(val, "1")
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
  info    <- .query(x$con,
    "SELECT key, value FROM project_meta WHERE key IN ('name','owner','locked')")
  proj_name  <- info$value[info$key == "name"]
  proj_owner <- info$value[info$key == "owner"]
  locked     <- identical(info$value[info$key == "locked"], "1")
  cli::cli_text("<qc_project> {.strong {proj_name}} [{proj_owner}]{if (locked) ' \U0001F512' else ''}")
  cli::cli_text("  Path:      {.file {x$path}}")
  cli::cli_text("  Documents: {n_docs}  |  Codes: {n_codes}{if (locked) '  |  LOCKED' else ''}")
  invisible(x)
}
