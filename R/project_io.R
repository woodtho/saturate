# -- Internal helpers -----------------------------------------------------------

# Insert a row into `table` via RETURNING id; returns the new integer id.
# `cols` is a character vector of column names; `vals` the matching param list.
.ins_ret <- function(con, table, cols, vals) {
  sql <- paste0(
    "INSERT INTO ", table, " (", paste(cols, collapse = ", "), ") VALUES (",
    paste(rep("?", length(cols)), collapse = ", "),
    ") RETURNING id"
  )
  .query(con, sql, vals)$id[[1L]]
}

# Build a named integer vector: source_id (as string key) -> dest_id.
.make_id_map <- function(src_ids) {
  v <- integer(length(src_ids))
  names(v) <- as.character(src_ids)
  v
}

# Remap a column of IDs using a named integer map.
# Returns an integer vector; entries not found in map become NA.
.remap <- function(ids, id_map) {
  unname(id_map[as.character(ids)])
}

# -- Split ----------------------------------------------------------------------

#' Create a coder copy of a project
#'
#' Copies the codebook, cases, themes, and a subset (or all) of documents into
#' a new standalone `.duckdb` file. Codings are excluded by default so each
#' coder starts fresh. IDs are NOT preserved -- the copy gets fresh sequences --
#' so the files can be merged back later by name / content-hash matching.
#'
#' @param project A `qc_project` object.
#' @param path Character. Destination file path (`.duckdb`).
#' @param source_ids Integer vector of source IDs to include, or `NULL` for all.
#' @param include_codings Logical. Copy existing codings into the split file.
#' @param overwrite Logical. Overwrite `path` if it already exists.
#'
#' @return The new `qc_project` (invisibly). Caller must call [qc_close()].
#' @export
qc_split_project <- function(project,
                              path,
                              source_ids      = NULL,
                              include_codings = FALSE,
                              overwrite       = FALSE) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  if (!is_string(path)) rlang::abort("`path` must be a single string.")

  dest  <- qc_new(path, overwrite = overwrite)
  con_s <- project$con
  con_d <- dest$con

  # -- Codes ------------------------------------------------------------------
  codes_s  <- .query(con_s, "SELECT * FROM codes WHERE status = 1")
  code_map <- .make_id_map(codes_s$id)

  for (i in seq_len(nrow(codes_s))) {
    r  <- codes_s[i, , drop = FALSE]
    ni <- .ins_ret(con_d, "codes",
      c("name","color","memo","definition","criteria","code_key",
        "deprecated","deprecated_reason","weight","weight_description"),
      list(r$name, r$color %||% "#4E79A7", r$memo %||% "",
           r$definition %||% "", r$criteria %||% "",
           if (length(r$code_key)==1L && is.na(r$code_key)) NA_character_ else r$code_key,
           as.integer(r$deprecated %||% 0L), r$deprecated_reason %||% "",
           if (length(r$weight)==1L && is.na(r$weight)) NA_real_ else as.double(r$weight),
           r$weight_description %||% ""))
    code_map[[as.character(r$id)]] <- as.integer(ni)
  }

  # Fix codes.parent_id (self-referential)
  for (i in seq_len(nrow(codes_s))) {
    pid <- codes_s$parent_id[[i]]
    if (!is.null(pid) && !is.na(pid)) {
      new_pid <- code_map[as.character(pid)]
      if (!is.null(new_pid) && !is.na(new_pid))
        .exec(con_d, "UPDATE codes SET parent_id = ? WHERE id = ?",
              list(as.integer(new_pid), code_map[[as.character(codes_s$id[[i]])]]))
    }
  }

  # -- Code categories ---------------------------------------------------------
  cats_s  <- .query(con_s, "SELECT * FROM code_categories WHERE status = 1")
  cat_map <- .make_id_map(cats_s$id)

  for (i in seq_len(nrow(cats_s))) {
    r  <- cats_s[i, , drop = FALSE]
    ni <- .ins_ret(con_d, "code_categories", c("name","memo"),
                   list(r$name, r$memo %||% ""))
    cat_map[[as.character(r$id)]] <- as.integer(ni)
  }

  if (nrow(codes_s) > 0L && nrow(cats_s) > 0L) {
    ccl <- .query(con_s, "SELECT * FROM code_category_links WHERE status = 1")
    if (nrow(ccl) > 0L) {
      ccl$code_id     <- .remap(ccl$code_id,     code_map)
      ccl$category_id <- .remap(ccl$category_id, cat_map)
      ok <- ccl[!is.na(ccl$code_id) & !is.na(ccl$category_id), , drop = FALSE]
      if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "code_category_links", ok, append = TRUE)
    }

    rels <- .query(con_s, "SELECT * FROM code_relations WHERE status = 1")
    if (nrow(rels) > 0L) {
      rno_id <- rels[, setdiff(names(rels), "id"), drop = FALSE]
      rno_id$code_id_1 <- .remap(rels$code_id_1, code_map)
      rno_id$code_id_2 <- .remap(rels$code_id_2, code_map)
      ok <- rno_id[!is.na(rno_id$code_id_1) & !is.na(rno_id$code_id_2), , drop = FALSE]
      if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "code_relations", ok, append = TRUE)
    }
  }

  # -- Document categories ------------------------------------------------------
  doccats_s  <- .query(con_s, "SELECT * FROM document_categories WHERE status = 1")
  doccat_map <- .make_id_map(doccats_s$id)

  for (i in seq_len(nrow(doccats_s))) {
    r  <- doccats_s[i, , drop = FALSE]
    ni <- .ins_ret(con_d, "document_categories", c("name","memo"),
                   list(r$name, r$memo %||% ""))
    doccat_map[[as.character(r$id)]] <- as.integer(ni)
  }

  # -- Cases --------------------------------------------------------------------
  cases_s  <- .query(con_s, "SELECT * FROM cases WHERE status = 1")
  case_map <- .make_id_map(cases_s$id)

  for (i in seq_len(nrow(cases_s))) {
    r  <- cases_s[i, , drop = FALSE]
    ni <- .ins_ret(con_d, "cases", c("name","memo"),
                   list(r$name, r$memo %||% ""))
    case_map[[as.character(r$id)]] <- as.integer(ni)
  }

  if (nrow(cases_s) > 0L) {
    ca <- .query(con_s, paste0(
      "SELECT * FROM case_attributes WHERE status = 1 AND case_id IN (",
      paste(cases_s$id, collapse = ","), ")"))
    if (nrow(ca) > 0L) {
      ca_no_id <- ca[, setdiff(names(ca), "id"), drop = FALSE]
      ca_no_id$case_id <- .remap(ca$case_id, case_map)
      ok <- ca_no_id[!is.na(ca_no_id$case_id), , drop = FALSE]
      if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "case_attributes", ok, append = TRUE)
    }
  }

  # -- Sources ------------------------------------------------------------------
  src_sql <- if (is.null(source_ids)) {
    "SELECT * FROM sources WHERE status = 1"
  } else {
    paste0("SELECT * FROM sources WHERE status = 1 AND id IN (",
           paste(as.integer(source_ids), collapse = ","), ")")
  }
  srcs_s  <- .query(con_s, src_sql)
  src_map <- .make_id_map(srcs_s$id)

  for (i in seq_len(nrow(srcs_s))) {
    r  <- srcs_s[i, , drop = FALSE]
    ni <- .ins_ret(con_d, "sources",
      c("name","content","memo","filename","source_system","language",
        "doc_version","content_hash","word_count","source_type"),
      list(r$name, r$content, r$memo %||% "", r$filename %||% "",
           r$source_system %||% "manual", r$language %||% "",
           as.integer(r$doc_version %||% 1L), r$content_hash %||% "",
           as.integer(r$word_count %||% 0L), r$source_type %||% ""))
    src_map[[as.character(r$id)]] <- as.integer(ni)
  }

  # Fix sources.parent_id (self-referential)
  for (i in seq_len(nrow(srcs_s))) {
    pid <- srcs_s$parent_id[[i]]
    if (!is.null(pid) && !is.na(pid)) {
      new_pid <- src_map[as.character(pid)]
      if (!is.null(new_pid) && !is.na(new_pid))
        .exec(con_d, "UPDATE sources SET parent_id = ? WHERE id = ?",
              list(as.integer(new_pid), src_map[[as.character(srcs_s$id[[i]])]]))
    }
  }

  if (nrow(srcs_s) > 0L) {
    in_s <- paste(srcs_s$id, collapse = ",")

    sv <- .query(con_s, paste0("SELECT * FROM source_versions WHERE source_id IN (", in_s, ")"))
    if (nrow(sv) > 0L) {
      sv_no_id <- sv[, setdiff(names(sv), "id"), drop = FALSE]
      sv_no_id$source_id <- .remap(sv$source_id, src_map)
      ok <- sv_no_id[!is.na(sv_no_id$source_id), , drop = FALSE]
      if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "source_versions", ok, append = TRUE)
    }

    sa <- .query(con_s, paste0(
      "SELECT * FROM source_attributes WHERE status = 1 AND source_id IN (", in_s, ")"))
    if (nrow(sa) > 0L) {
      sa_no_id <- sa[, setdiff(names(sa), "id"), drop = FALSE]
      sa_no_id$source_id <- .remap(sa$source_id, src_map)
      ok <- sa_no_id[!is.na(sa_no_id$source_id), , drop = FALSE]
      if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "source_attributes", ok, append = TRUE)
    }

    ann <- .query(con_s, paste0(
      "SELECT * FROM annotations WHERE status = 1 AND source_id IN (", in_s, ")"))
    if (nrow(ann) > 0L) {
      ann_no_id <- ann[, setdiff(names(ann), "id"), drop = FALSE]
      ann_no_id$source_id <- .remap(ann$source_id, src_map)
      ok <- ann_no_id[!is.na(ann_no_id$source_id), , drop = FALSE]
      if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "annotations", ok, append = TRUE)
    }

    if (nrow(doccats_s) > 0L) {
      dcl <- .query(con_s, paste0(
        "SELECT * FROM document_category_links WHERE status = 1 AND source_id IN (", in_s, ")"))
      if (nrow(dcl) > 0L) {
        dcl$source_id   <- .remap(dcl$source_id,   src_map)
        dcl$category_id <- .remap(dcl$category_id, doccat_map)
        ok <- dcl[!is.na(dcl$source_id) & !is.na(dcl$category_id), , drop = FALSE]
        if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "document_category_links", ok, append = TRUE)
      }
    }

    if (nrow(cases_s) > 0L) {
      csl <- .query(con_s, paste0(
        "SELECT * FROM case_source_links WHERE status = 1 AND source_id IN (", in_s, ")"))
      if (nrow(csl) > 0L) {
        csl$source_id <- .remap(csl$source_id, src_map)
        csl$case_id   <- .remap(csl$case_id,   case_map)
        ok <- csl[!is.na(csl$source_id) & !is.na(csl$case_id), , drop = FALSE]
        if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "case_source_links", ok, append = TRUE)
      }
    }

    # -- Codings (optional) -------------------------------------------------
    if (include_codings && nrow(codes_s) > 0L) {
      cod_s   <- .query(con_s, paste0(
        "SELECT * FROM codings WHERE status = 1 AND source_id IN (", in_s, ")"))
      cod_map <- .make_id_map(cod_s$id)

      for (i in seq_len(nrow(cod_s))) {
        r          <- cod_s[i, , drop = FALSE]
        new_src_id  <- src_map[[as.character(r$source_id)]]
        new_code_id <- code_map[[as.character(r$code_id)]]
        if (is.null(new_src_id)  || is.na(new_src_id) ||
            is.null(new_code_id) || is.na(new_code_id)) next
        ni <- .ins_ret(con_d, "codings",
          c("source_id","code_id","selfirst","selast","seltext","memo",
            "coder","coding_source","coding_status","confidence"),
          list(as.integer(new_src_id), as.integer(new_code_id),
               as.integer(r$selfirst), as.integer(r$selast),
               r$seltext %||% "", r$memo %||% "",
               r$coder %||% "default", r$coding_source %||% "manual",
               r$coding_status %||% "validated",
               if (is.null(r$confidence)||is.na(r$confidence)) NA_integer_
               else as.integer(r$confidence)))
        cod_map[[as.character(r$id)]] <- as.integer(ni)
      }

      if (length(cod_map) > 0L) {
        aud <- .query(con_s, paste0(
          "SELECT * FROM coding_audit WHERE source_id IN (", in_s, ")"))
        if (nrow(aud) > 0L) {
          aud_no_id <- aud[, setdiff(names(aud), "id"), drop = FALSE]
          aud_no_id$source_id <- .remap(aud$source_id, src_map)
          aud_no_id$code_id   <- .remap(aud$code_id,   code_map)
          aud_no_id$coding_id <- .remap(aud$coding_id, cod_map)
          ok <- aud_no_id[!is.na(aud_no_id$source_id) & !is.na(aud_no_id$coding_id), , drop = FALSE]
          if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "coding_audit", ok, append = TRUE)
        }
      }
    }
  }

  # -- Themes -------------------------------------------------------------------
  themes_s   <- .query(con_s, "SELECT * FROM themes WHERE status = 1")
  theme_map  <- .make_id_map(themes_s$id)

  for (i in seq_len(nrow(themes_s))) {
    r  <- themes_s[i, , drop = FALSE]
    ni <- .ins_ret(con_d, "themes",
      c("name","central_concept","narrative","definition","scope"),
      list(r$name, r$central_concept %||% "", r$narrative %||% "",
           r$definition %||% "", r$scope %||% ""))
    theme_map[[as.character(r$id)]] <- as.integer(ni)
  }

  if (nrow(themes_s) > 0L) {
    in_t <- paste(themes_s$id, collapse = ",")

    if (nrow(codes_s) > 0L) {
      tcl <- .query(con_s, paste0(
        "SELECT * FROM theme_code_links WHERE status = 1 AND theme_id IN (", in_t, ")"))
      if (nrow(tcl) > 0L) {
        tcl$theme_id <- .remap(tcl$theme_id, theme_map)
        tcl$code_id  <- .remap(tcl$code_id,  code_map)
        ok <- tcl[!is.na(tcl$theme_id) & !is.na(tcl$code_id), , drop = FALSE]
        if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "theme_code_links", ok, append = TRUE)
      }
    }

    if (nrow(cats_s) > 0L) {
      tkcl <- .query(con_s, paste0(
        "SELECT * FROM theme_category_links WHERE status = 1 AND theme_id IN (", in_t, ")"))
      if (nrow(tkcl) > 0L) {
        tkcl$theme_id    <- .remap(tkcl$theme_id,    theme_map)
        tkcl$category_id <- .remap(tkcl$category_id, cat_map)
        ok <- tkcl[!is.na(tkcl$theme_id) & !is.na(tkcl$category_id), , drop = FALSE]
        if (nrow(ok) > 0L) DBI::dbWriteTable(con_d, "theme_category_links", ok, append = TRUE)
      }
    }
  }

  # -- project_meta -------------------------------------------------------------
  meta <- .query(con_s, "SELECT * FROM project_meta")
  for (i in seq_len(nrow(meta))) {
    key <- meta$key[[i]]
    val <- if (key == "locked") "0" else meta$value[[i]]
    .exec(con_d,
      "INSERT INTO project_meta (key, value) VALUES (?, ?)
       ON CONFLICT (key) DO UPDATE SET value = excluded.value",
      list(key, val))
  }

  cli::cli_alert_success("Split project created: {.file {path}}")
  invisible(dest)
}

# -- Merge ----------------------------------------------------------------------

#' Merge a contributor project into a master project
#'
#' Reads codes, documents, codings, themes, and memos from a contributor
#' `.duckdb` file and inserts any new items into `master`. Items that already
#' exist (matched by name / content hash) are skipped or replaced.
#'
#' @param master A `qc_project` object (write target).
#' @param contributor_path Character. Path to the contributor `.duckdb` file.
#' @param on_conflict `"skip"` (default) leaves existing codings untouched.
#'   `"replace"` soft-deletes the existing coding and re-inserts.
#' @param coders Character vector. If provided, only import codings by these
#'   coder names.
#'
#' @return Invisibly, a named list: `codes_added`, `sources_added`,
#'   `codings_added`, `codings_skip`, `themes_added`, `memos_added`.
#' @export
qc_merge_project <- function(master,
                              contributor_path,
                              on_conflict = c("skip", "replace"),
                              coders      = NULL) {
  assert_class(master, "qc_project")
  assert_con(master$con)
  .assert_unlocked(master)
  if (!is_string(contributor_path)) rlang::abort("`contributor_path` must be a string.")
  contributor_path <- fs::path_abs(contributor_path)
  if (!fs::file_exists(contributor_path))
    rlang::abort(paste0("Contributor file not found: ", contributor_path))
  on_conflict <- match.arg(on_conflict)

  con_m <- master$con
  con_b <- DBI::dbConnect(duckdb::duckdb(), dbdir = contributor_path, read_only = TRUE)
  on.exit(try(DBI::dbDisconnect(con_b, shutdown = TRUE), silent = TRUE), add = TRUE)

  result <- list(codes_added   = 0L, sources_added = 0L,
                 codings_added = 0L, codings_skip  = 0L,
                 themes_added  = 0L, memos_added   = 0L)

  # -- Master lookup maps --------------------------------------------------------
  m_codes  <- .query(con_m, "SELECT id, lower(name) AS nlc FROM codes WHERE status = 1")
  m_cats   <- .query(con_m, "SELECT id, lower(name) AS nlc FROM code_categories WHERE status = 1")
  m_srcs   <- .query(con_m,
    "SELECT id, lower(name) AS nlc, content_hash FROM sources WHERE status = 1")
  m_themes <- .query(con_m, "SELECT id, lower(name) AS nlc FROM themes WHERE status = 1")

  code_nlc_to_id  <- setNames(m_codes$id,  m_codes$nlc)
  cat_nlc_to_id   <- setNames(m_cats$id,   m_cats$nlc)
  theme_nlc_to_id <- setNames(m_themes$id, m_themes$nlc)
  src_hash_to_id  <- setNames(m_srcs$id,   m_srcs$content_hash)
  src_nlc_to_id   <- setNames(m_srcs$id,   m_srcs$nlc)

  # -- Merge codes ---------------------------------------------------------------
  b_codes  <- .query(con_b, "SELECT * FROM codes WHERE status = 1")
  code_map <- .make_id_map(b_codes$id)

  for (i in seq_len(nrow(b_codes))) {
    r   <- b_codes[i, , drop = FALSE]
    nlc <- tolower(r$name)
    mid <- code_nlc_to_id[nlc]
    if (!is.null(mid) && !is.na(mid)) {
      code_map[[as.character(r$id)]] <- as.integer(mid)
    } else {
      ni <- .ins_ret(con_m, "codes",
        c("name","color","memo","definition","criteria","code_key",
          "deprecated","deprecated_reason","weight","weight_description"),
        list(r$name, r$color %||% "#4E79A7", r$memo %||% "",
             r$definition %||% "", r$criteria %||% "",
             if (length(r$code_key)==1L && is.na(r$code_key)) NA_character_ else r$code_key,
             as.integer(r$deprecated %||% 0L), r$deprecated_reason %||% "",
             if (length(r$weight)==1L && is.na(r$weight)) NA_real_ else as.double(r$weight),
             r$weight_description %||% ""))
      code_map[[as.character(r$id)]] <- as.integer(ni)
      code_nlc_to_id[[nlc]] <- as.integer(ni)
      result$codes_added <- result$codes_added + 1L
    }
  }

  # -- Merge code_categories -----------------------------------------------------
  b_cats  <- .query(con_b, "SELECT * FROM code_categories WHERE status = 1")
  cat_map <- .make_id_map(b_cats$id)

  for (i in seq_len(nrow(b_cats))) {
    r   <- b_cats[i, , drop = FALSE]
    nlc <- tolower(r$name)
    mid <- cat_nlc_to_id[nlc]
    if (!is.null(mid) && !is.na(mid)) {
      cat_map[[as.character(r$id)]] <- as.integer(mid)
    } else {
      ni <- .ins_ret(con_m, "code_categories", c("name","memo"),
                     list(r$name, r$memo %||% ""))
      cat_map[[as.character(r$id)]] <- as.integer(ni)
      cat_nlc_to_id[[nlc]] <- as.integer(ni)
    }
  }

  # Re-link codes to categories
  b_ccl <- .query(con_b, "SELECT * FROM code_category_links WHERE status = 1")
  for (i in seq_len(nrow(b_ccl))) {
    cid   <- code_map[as.character(b_ccl$code_id[[i]])]
    catid <- cat_map[as.character(b_ccl$category_id[[i]])]
    if (!is.null(cid) && !is.na(cid) && !is.null(catid) && !is.na(catid)) {
      .exec(con_m,
        "INSERT INTO code_category_links (code_id, category_id, status) VALUES (?, ?, 1)
         ON CONFLICT (code_id, category_id) DO UPDATE SET status = 1",
        list(as.integer(cid), as.integer(catid)))
    }
  }

  # -- Merge sources -------------------------------------------------------------
  b_srcs  <- .query(con_b, "SELECT * FROM sources WHERE status = 1")
  src_map <- .make_id_map(b_srcs$id)

  for (i in seq_len(nrow(b_srcs))) {
    r    <- b_srcs[i, , drop = FALSE]
    hash <- r$content_hash %||% ""
    nlc  <- tolower(r$name)
    mid  <- src_hash_to_id[hash]
    if (is.null(mid) || is.na(mid)) mid <- src_nlc_to_id[nlc]

    if (!is.null(mid) && !is.na(mid)) {
      src_map[[as.character(r$id)]] <- as.integer(mid)
    } else {
      ni <- .ins_ret(con_m, "sources",
        c("name","content","memo","filename","source_system","language",
          "doc_version","content_hash","word_count","source_type"),
        list(r$name, r$content, r$memo %||% "", r$filename %||% "",
             r$source_system %||% "manual", r$language %||% "",
             as.integer(r$doc_version %||% 1L), r$content_hash %||% "",
             as.integer(r$word_count  %||% 0L), r$source_type %||% ""))
      src_map[[as.character(r$id)]] <- as.integer(ni)
      src_hash_to_id[[hash]] <- as.integer(ni)
      src_nlc_to_id[[nlc]]   <- as.integer(ni)
      result$sources_added <- result$sources_added + 1L
    }
  }

  # -- Merge themes -------------------------------------------------------------
  b_themes  <- .query(con_b, "SELECT * FROM themes WHERE status = 1")
  theme_map <- .make_id_map(b_themes$id)

  for (i in seq_len(nrow(b_themes))) {
    r   <- b_themes[i, , drop = FALSE]
    nlc <- tolower(r$name)
    mid <- theme_nlc_to_id[nlc]
    if (!is.null(mid) && !is.na(mid)) {
      theme_map[[as.character(r$id)]] <- as.integer(mid)
    } else {
      ni <- .ins_ret(con_m, "themes",
        c("name","central_concept","narrative","definition","scope"),
        list(r$name, r$central_concept %||% "", r$narrative %||% "",
             r$definition %||% "", r$scope %||% ""))
      theme_map[[as.character(r$id)]] <- as.integer(ni)
      theme_nlc_to_id[[nlc]] <- as.integer(ni)
      result$themes_added <- result$themes_added + 1L

      in_tid <- as.integer(r$id)
      b_tcl <- .query(con_b, paste0(
        "SELECT * FROM theme_code_links WHERE status = 1 AND theme_id = ", in_tid))
      for (j in seq_len(nrow(b_tcl))) {
        cid <- code_map[as.character(b_tcl$code_id[[j]])]
        if (!is.null(cid) && !is.na(cid))
          .exec(con_m,
            "INSERT INTO theme_code_links (theme_id, code_id, status) VALUES (?, ?, 1)
             ON CONFLICT (theme_id, code_id) DO UPDATE SET status = 1",
            list(as.integer(ni), as.integer(cid)))
      }

      b_tkcl <- .query(con_b, paste0(
        "SELECT * FROM theme_category_links WHERE status = 1 AND theme_id = ", in_tid))
      for (j in seq_len(nrow(b_tkcl))) {
        catid <- cat_map[as.character(b_tkcl$category_id[[j]])]
        if (!is.null(catid) && !is.na(catid))
          .exec(con_m,
            "INSERT INTO theme_category_links (theme_id, category_id, status) VALUES (?, ?, 1)
             ON CONFLICT (theme_id, category_id) DO UPDATE SET status = 1",
            list(as.integer(ni), as.integer(catid)))
      }
    }
  }

  # -- Merge codings -------------------------------------------------------------
  coder_sql <- if (!is.null(coders) && length(coders) > 0L)
    paste0(" AND coder IN (", paste(shQuote(coders), collapse = ","), ")")
  else ""

  b_codings <- .query(con_b, paste0(
    "SELECT * FROM codings WHERE status = 1", coder_sql))

  if (nrow(b_codings) > 0L) {
    b_codings$m_src  <- .remap(b_codings$source_id, src_map)
    b_codings$m_code <- .remap(b_codings$code_id,   code_map)
    b_codings <- b_codings[!is.na(b_codings$m_src) & !is.na(b_codings$m_code), , drop = FALSE]

    existing <- .query(con_m,
      "SELECT source_id, code_id, selfirst, selast, coder FROM codings WHERE status = 1")
    exist_keys <- if (nrow(existing) > 0L)
      paste(existing$source_id, existing$code_id,
            existing$selfirst, existing$selast, existing$coder, sep = "\x1f")
    else character(0L)

    changed_by <- Sys.info()[["user"]] %||% "merge"

    for (i in seq_len(nrow(b_codings))) {
      r   <- b_codings[i, , drop = FALSE]
      sid <- r$m_src
      cid <- r$m_code
      key <- paste(sid, cid, r$selfirst, r$selast, r$coder, sep = "\x1f")

      if (key %in% exist_keys) {
        if (on_conflict == "skip") {
          result$codings_skip <- result$codings_skip + 1L
          next
        }
        .exec(con_m,
          "UPDATE codings SET status = 0
           WHERE source_id = ? AND code_id = ? AND selfirst = ? AND selast = ?
             AND coder = ? AND status = 1",
          list(sid, cid, as.integer(r$selfirst), as.integer(r$selast), r$coder))
      }

      ni <- .ins_ret(con_m, "codings",
        c("source_id","code_id","selfirst","selast","seltext","memo",
          "coder","coding_source","coding_status","confidence"),
        list(as.integer(sid), as.integer(cid),
             as.integer(r$selfirst), as.integer(r$selast),
             r$seltext %||% "", r$memo %||% "",
             r$coder %||% "default", r$coding_source %||% "manual",
             r$coding_status %||% "validated",
             if (is.null(r$confidence)||is.na(r$confidence)) NA_integer_
             else as.integer(r$confidence)))

      .exec(con_m,
        "INSERT INTO coding_audit
           (coding_id, source_id, code_id, operation, coder, changed_by)
         VALUES (?, ?, ?, 'create', ?, ?)",
        list(as.integer(ni), as.integer(sid), as.integer(cid),
             r$coder %||% "default", changed_by))

      exist_keys <- c(exist_keys, key)
      result$codings_added <- result$codings_added + 1L
    }
  }

  # -- Merge project_memos -------------------------------------------------------
  b_memos <- .query(con_b, "SELECT * FROM project_memos WHERE status = 1")
  if (nrow(b_memos) > 0L) {
    mm <- .query(con_m,
      "SELECT lower(content) AS clc, created_by, memo_type
       FROM project_memos WHERE status = 1")
    exist_memo_keys <- if (nrow(mm) > 0L)
      paste(mm$clc, mm$created_by %||% "", mm$memo_type, sep = "\x1f")
    else character(0L)

    for (i in seq_len(nrow(b_memos))) {
      r   <- b_memos[i, , drop = FALSE]
      key <- paste(tolower(r$content),
                   r$created_by %||% "",
                   r$memo_type  %||% "analytical", sep = "\x1f")
      if (key %in% exist_memo_keys) next
      .exec(con_m,
        "INSERT INTO project_memos (content, memo_type, created_by) VALUES (?, ?, ?)",
        list(r$content, r$memo_type %||% "analytical", r$created_by %||% ""))
      exist_memo_keys <- c(exist_memo_keys, key)
      result$memos_added <- result$memos_added + 1L
    }
  }

  cli::cli_alert_success(paste0(
    "Merge complete \u2014 ",
    result$codings_added, " coding(s), ",
    result$codes_added,   " code(s), ",
    result$sources_added, " document(s) added; ",
    result$codings_skip,  " duplicate(s) skipped."
  ))
  invisible(result)
}
