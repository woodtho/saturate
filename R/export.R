# -- Export: themes report, full codebook, raw project data --------------------

# -- Themes analytical report --------------------------------------------------

#' Export an analytical themes report
#'
#' Generates a formatted document containing each theme's proposition,
#' narrative, definition, scope, linked codes/categories, and supporting
#' excerpts. Suitable for sharing with supervisors or writing up methods.
#'
#' @param project A `qc_project` object.
#' @param format One of `"docx"`, `"html"`, `"txt"`, `"json"`.
#' @param theme_ids Integer vector or `NULL` (all themes).
#' @param include_excerpts Logical. Include coded excerpts under each theme.
#' @param include_narrative Logical. Include narrative and definition fields.
#' @param output_path File path or `NULL` (returns a temp file path).
#'
#' @return Path to the generated file (invisibly).
#' @export
qc_export_themes_report <- function(project,
                                    format           = c("docx", "html", "txt", "json"),
                                    theme_ids        = NULL,
                                    include_excerpts  = TRUE,
                                    include_narrative = TRUE,
                                    output_path      = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  format <- match.arg(format)

  themes <- qc_list_themes(project)
  if (!is.null(theme_ids))
    themes <- themes[themes$id %in% as.integer(theme_ids), ]
  if (nrow(themes) == 0L)
    rlang::abort("No themes to export.")

  path <- switch(format,
    docx = .export_themes_docx(project, themes, include_excerpts, include_narrative),
    html = .export_themes_html(project, themes, include_excerpts, include_narrative),
    txt  = .export_themes_txt(project,  themes, include_excerpts, include_narrative),
    json = .export_themes_json(project,  themes, include_excerpts)
  )

  if (!is.null(output_path)) {
    file.copy(path, output_path, overwrite = TRUE)
    return(invisible(output_path))
  }
  invisible(path)
}

# -- Full codebook export ------------------------------------------------------

#' Export a rich codebook
#'
#' Extends [qc_export_codebook()] with optional example excerpts and more
#' output formats including Word and Excel.
#'
#' @param project A `qc_project` object.
#' @param format One of `"docx"`, `"xlsx"`, `"csv"`, `"json"`, `"html"`.
#' @param include_definitions Logical. Include code definitions.
#' @param include_criteria Logical. Include inclusion/exclusion criteria.
#' @param include_memo Logical. Include code memos.
#' @param include_examples Logical. Include example excerpts per code.
#' @param n_examples Integer. Maximum excerpts per code when `include_examples`.
#' @param output_path File path or `NULL`.
#'
#' @return Path to the generated file (invisibly).
#' @export
qc_export_codebook_full <- function(project,
                                    format              = c("docx", "xlsx", "csv", "json", "html"),
                                    include_definitions = TRUE,
                                    include_criteria    = TRUE,
                                    include_memo        = FALSE,
                                    include_examples    = FALSE,
                                    n_examples          = 2L,
                                    output_path         = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  format <- match.arg(format)

  codes <- qc_list_codes(project)
  cats  <- .query(project$con,
    "SELECT cc.id AS cat_id, cc.name AS cat_name, ccl.code_id
     FROM code_categories cc
     JOIN code_category_links ccl ON ccl.category_id = cc.id AND ccl.status = 1
     WHERE cc.status = 1")

  code_cats <- lapply(codes$id, function(cid) {
    matched <- cats[cats$code_id == cid, "cat_name", drop = TRUE]
    paste(matched, collapse = "; ")
  })
  codes$categories <- unlist(code_cats)

  path <- switch(format,
    docx = .export_codebook_docx(project, codes, include_definitions,
                                  include_criteria, include_memo,
                                  include_examples, n_examples),
    xlsx = .export_codebook_xlsx(project, codes, include_definitions,
                                  include_criteria, include_memo,
                                  include_examples, n_examples),
    csv  = .export_codebook_csv(codes, include_definitions, include_criteria, include_memo),
    json = .export_codebook_json(project, codes, include_examples, n_examples),
    html = .export_codebook_html(project, codes, include_definitions,
                                  include_criteria, include_memo,
                                  include_examples, n_examples)
  )

  if (!is.null(output_path)) {
    file.copy(path, output_path, overwrite = TRUE)
    return(invisible(output_path))
  }
  invisible(path)
}

# -- Raw table export ----------------------------------------------------------

#' Export a raw project database table
#'
#' Exports any major project table as-is for archival, secondary analysis, or
#' transfer between tools.
#'
#' @param project A `qc_project` object.
#' @param table_name One of the supported table names (see Details).
#' @param format One of `"csv"`, `"json"`, `"xlsx"`.
#' @param output_path File path or `NULL`.
#'
#' @details
#' Supported table names: `"documents"`, `"codes"`, `"codings"`,
#' `"categories"`, `"category_links"`, `"themes"`, `"cases"`,
#' `"case_attributes"`, `"annotations"`, `"memos"`, `"coding_audit"`,
#' `"code_history"`.
#'
#' @return Path to the generated file (invisibly).
#' @export
qc_export_project_data <- function(project,
                                   table_name  = "codings",
                                   format      = c("csv", "json", "xlsx"),
                                   output_path = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  format <- match.arg(format)

  sql <- switch(table_name,
    documents     = "SELECT id, name, filename, source_type, language,
                            word_count, doc_version, memo, created_at
                     FROM sources WHERE status = 1 ORDER BY name",
    codes         = "SELECT id, name, color, definition, criteria, memo,
                            code_key, weight, deprecated, parent_id, status, created_at
                     FROM codes ORDER BY name",
    codings       = "SELECT cod.id, s.name AS document, c.name AS code,
                            cod.selfirst, cod.selast, cod.seltext, cod.memo,
                            cod.coder, cod.coding_status, cod.confidence,
                            cod.coding_source, cod.created_at
                     FROM codings cod
                     JOIN sources s ON s.id = cod.source_id
                     JOIN codes   c ON c.id = cod.code_id
                     WHERE cod.status = 1 AND s.status = 1 AND c.status = 1
                     ORDER BY s.name, cod.selfirst",
    categories    = "SELECT id, name, memo, created_at FROM code_categories WHERE status = 1 ORDER BY name",
    category_links= "SELECT cc.name AS category, c.name AS code, c.id AS code_id
                     FROM code_category_links ccl
                     JOIN code_categories cc ON cc.id = ccl.category_id
                     JOIN codes c ON c.id = ccl.code_id
                     WHERE ccl.status = 1 AND cc.status = 1 AND c.status = 1
                     ORDER BY cc.name, c.name",
    themes        = "SELECT id, name, central_concept, narrative, definition,
                            scope, status, created_at
                     FROM themes WHERE status = 1 ORDER BY name",
    cases         = "SELECT id, name, memo, created_at FROM cases WHERE status = 1 ORDER BY name",
    case_attributes = "SELECT ca.case_id, c.name AS case_name, ca.variable, ca.value
                       FROM case_attributes ca
                       JOIN cases c ON c.id = ca.case_id
                       WHERE ca.status = 1 AND c.status = 1
                       ORDER BY c.name, ca.variable",
    annotations   = "SELECT a.id, s.name AS document, a.position,
                            a.annotation, a.created_at
                     FROM annotations a
                     JOIN sources s ON s.id = a.source_id
                     WHERE a.status = 1 AND s.status = 1
                     ORDER BY s.name, a.position",
    memos         = "SELECT id, content, memo_type, created_by, created_at
                     FROM project_memos WHERE status = 1 ORDER BY created_at",
    coding_audit  = "SELECT ca.id, s.name AS document, c.name AS code,
                            ca.operation, ca.field, ca.old_value, ca.new_value,
                            ca.selfirst, ca.selast, ca.coder, ca.changed_by, ca.changed_at
                     FROM coding_audit ca
                     LEFT JOIN sources s ON s.id = ca.source_id
                     LEFT JOIN codes   c ON c.id = ca.code_id
                     ORDER BY ca.changed_at DESC",
    code_history  = "SELECT ch.id, c.name AS code, ch.operation, ch.field,
                            ch.old_value, ch.new_value, ch.changed_at
                     FROM code_history ch
                     LEFT JOIN codes c ON c.id = ch.code_id
                     ORDER BY ch.changed_at DESC",
    rlang::abort(paste0("Unknown table_name '", table_name, "'."))
  )

  df <- .query(project$con, sql)

  ext  <- c(csv = ".csv", json = ".json", xlsx = ".xlsx")[[format]]
  path <- tempfile(fileext = ext)

  if (format == "csv") {
    utils::write.csv(df, path, row.names = FALSE)
  } else if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE))
      rlang::abort("Install the 'jsonlite' package to export JSON.")
    writeLines(jsonlite::toJSON(df, auto_unbox = TRUE, pretty = TRUE), path)
  } else if (format == "xlsx") {
    .write_xlsx(list(data = df), path)
  }

  if (!is.null(output_path)) {
    file.copy(path, output_path, overwrite = TRUE)
    return(invisible(output_path))
  }
  invisible(path)
}

# -- Private helpers -----------------------------------------------------------

.write_xlsx <- function(sheets, path) {
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    wb <- openxlsx2::wb_workbook()
    for (nm in names(sheets)) {
      wb <- openxlsx2::wb_add_worksheet(wb, nm)
      wb <- openxlsx2::wb_add_data(wb, nm, sheets[[nm]])
    }
    openxlsx2::wb_save(wb, path)
  } else if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    for (nm in names(sheets)) {
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeData(wb, nm, sheets[[nm]])
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  } else if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(sheets, path)
  } else {
    rlang::abort("Install 'openxlsx2', 'openxlsx', or 'writexl' to export Excel files.")
  }
}

# --- Themes DOCX -------------------------------------------------------------

.export_themes_docx <- function(project, themes, include_excerpts, include_narrative) {
  if (!requireNamespace("officer", quietly = TRUE))
    rlang::abort("Install the 'officer' package to export Word documents.")

  info <- qc_project_info(project)
  doc  <- officer::read_docx()

  doc <- officer::body_add_par(doc, paste0(info$name, " \u2014 Analytical Themes Report"),
                                style = "heading 1")
  doc <- officer::body_add_par(doc, format(Sys.Date(), "%B %d, %Y"), style = "Normal")
  doc <- officer::body_add_par(doc, paste0(nrow(themes), " themes"), style = "Normal")
  doc <- officer::body_add_break(doc)

  for (i in seq_len(nrow(themes))) {
    th <- themes[i, ]
    td <- qc_get_theme(project, th$id)

    doc <- officer::body_add_par(doc, as.character(th$name), style = "heading 1")

    if (nchar(trimws(th$central_concept %||% "")) > 0)
      doc <- officer::body_add_par(doc,
        paste0("Proposition: ", th$central_concept), style = "heading 2")

    if (include_narrative) {
      if (nchar(trimws(th$narrative %||% "")) > 0)
        doc <- officer::body_add_par(doc, as.character(th$narrative), style = "Normal")

      if (nchar(trimws(th$definition %||% "")) > 0) {
        doc <- officer::body_add_par(doc, "Definition", style = "heading 3")
        doc <- officer::body_add_par(doc, as.character(th$definition), style = "Normal")
      }
      if (nchar(trimws(th$scope %||% "")) > 0) {
        doc <- officer::body_add_par(doc, "Scope", style = "heading 3")
        doc <- officer::body_add_par(doc, as.character(th$scope), style = "Normal")
      }
    }

    if (nrow(td$linked_cats) > 0 || nrow(td$linked_codes) > 0) {
      doc <- officer::body_add_par(doc, "Supporting Codes", style = "heading 3")
      for (j in seq_len(nrow(td$linked_cats))) {
        cat_row <- td$linked_cats[j, ]
        doc <- officer::body_add_par(doc,
          paste0("[Category] ", cat_row$name,
                 " (", cat_row$n_codes, " codes)"),
          style = "List Paragraph")
      }
      for (j in seq_len(nrow(td$linked_codes))) {
        cr <- td$linked_codes[j, ]
        doc <- officer::body_add_par(doc,
          paste0(cr$name, " \u2014 ", cr$n_codings, " coding(s)"),
          style = "List Paragraph")
      }
    }

    if (include_excerpts) {
      excerpts <- tryCatch(qc_theme_excerpts(project, th$id),
                            error = function(e) NULL)
      if (!is.null(excerpts) && nrow(excerpts) > 0L) {
        doc <- officer::body_add_par(doc, "Excerpts", style = "heading 3")
        for (dn in unique(excerpts$doc_name)) {
          de <- excerpts[excerpts$doc_name == dn, ]
          doc <- officer::body_add_par(doc, as.character(dn), style = "heading 4")
          for (k in seq_len(nrow(de))) {
            ex  <- de[k, ]
            txt <- trimws(as.character(ex$seltext))
            doc <- officer::body_add_fpar(doc,
              officer::fpar(
                officer::ftext(paste0("\u201c", txt, "\u201d"),
                  prop = officer::fp_text(italic = TRUE, font.size = 11)),
                fp_p = officer::fp_par(
                  padding.left = 30, padding.right = 15,
                  padding.top = 4,   padding.bottom = 2
                )
              )
            )
            doc <- officer::body_add_fpar(doc,
              officer::fpar(
                officer::ftext(
                  paste0("\u2014 ", ex$code_name, "  |  coder: ", ex$coder),
                  prop = officer::fp_text(font.size = 9, color = "#6c757d")),
                fp_p = officer::fp_par(
                  padding.left = 30, padding.bottom = 8)
              )
            )
          }
        }
      }
    }

    if (i < nrow(themes))
      doc <- officer::body_add_break(doc)
  }

  path <- tempfile(fileext = ".docx")
  print(doc, target = path)
  path
}

# --- Themes HTML -------------------------------------------------------------

.export_themes_html <- function(project, themes, include_excerpts, include_narrative) {
  info   <- qc_project_info(project)
  pieces <- character(0)

  css <- "
    body{font-family:Georgia,'Times New Roman',serif;max-width:860px;margin:2rem auto;
         padding:0 1.5rem;color:#1a1a1a;line-height:1.7}
    h1{font-size:1.9rem;border-bottom:3px solid #2c3e50;padding-bottom:.5rem;color:#2c3e50;margin-top:2.5rem}
    h2{font-size:1.2rem;color:#374151;font-style:italic;font-weight:normal;margin-top:.25rem}
    h3{font-size:1rem;font-weight:700;color:#374151;margin:1.2rem 0 .35rem;
       text-transform:uppercase;letter-spacing:.06em;font-size:.8rem}
    h4{font-size:.9rem;font-weight:700;color:#1a1a1a;margin:1rem 0 .25rem}
    .narrative{margin:.75rem 0;color:#1a1a1a}
    .codes-list{margin:.25rem 0 .5rem;padding-left:1.5rem;color:#374151;font-size:.9rem}
    .excerpt{border-left:3px solid #4E79A7;padding:.5rem 1rem;margin:.5rem 0;
             font-style:italic;color:#1a1a1a;background:#f8f9fa;border-radius:0 4px 4px 0}
    .excerpt-meta{font-size:.78rem;color:#4b5563;font-style:normal;margin-top:.2rem}
    .theme-section{margin-bottom:3rem;padding-bottom:2rem;border-bottom:1px solid #e9ecef}
    .meta{color:#4b5563;font-size:.85rem;margin:.5rem 0 2rem}
    .tag{display:inline-block;background:#e2e8f0;border-radius:3px;padding:2px 8px;
         font-size:.75rem;margin-right:.25rem;color:#374151}
  "

  pieces <- c(pieces, sprintf(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n
<title>%s \u2014 Analytical Themes Report</title>\n<style>%s</style>\n</head>\n<body>",
    htmltools::htmlEscape(info$name), css
  ))
  pieces <- c(pieces, sprintf(
    "<h1 style=\"border-top:5px solid #2c3e50;margin-top:0;padding-top:1.5rem\">%s</h1>
<p class=\"meta\">Analytical Themes Report &bull; %s &bull; %d themes</p>",
    htmltools::htmlEscape(info$name),
    format(Sys.Date(), "%B %d, %Y"),
    nrow(themes)
  ))

  for (i in seq_len(nrow(themes))) {
    th <- themes[i, ]
    td <- qc_get_theme(project, th$id)

    pieces <- c(pieces, sprintf('<section class="theme-section">'))
    pieces <- c(pieces, sprintf('<h1>%s</h1>', htmltools::htmlEscape(th$name)))

    if (nchar(trimws(th$central_concept %||% "")) > 0)
      pieces <- c(pieces, sprintf('<h2>%s</h2>',
        htmltools::htmlEscape(th$central_concept)))

    if (include_narrative) {
      if (nchar(trimws(th$narrative %||% "")) > 0)
        pieces <- c(pieces, sprintf('<p class="narrative">%s</p>',
          htmltools::htmlEscape(th$narrative)))

      if (nchar(trimws(th$definition %||% "")) > 0)
        pieces <- c(pieces, sprintf('<h3>Definition</h3><p>%s</p>',
          htmltools::htmlEscape(th$definition)))

      if (nchar(trimws(th$scope %||% "")) > 0)
        pieces <- c(pieces, sprintf('<h3>Scope</h3><p>%s</p>',
          htmltools::htmlEscape(th$scope)))
    }

    if (nrow(td$linked_cats) > 0 || nrow(td$linked_codes) > 0) {
      pieces <- c(pieces, '<h3>Supporting Codes</h3><ul class="codes-list">')
      for (j in seq_len(nrow(td$linked_cats))) {
        cr <- td$linked_cats[j, ]
        pieces <- c(pieces, sprintf(
          '<li><span class="tag">category</span> %s (%d codes)</li>',
          htmltools::htmlEscape(cr$name), cr$n_codes
        ))
      }
      for (j in seq_len(nrow(td$linked_codes))) {
        cr <- td$linked_codes[j, ]
        pieces <- c(pieces, sprintf(
          '<li><span style="display:inline-block;width:10px;height:10px;
            border-radius:2px;background:%s;margin-right:5px"></span>%s &mdash; %d coding(s)</li>',
          htmltools::htmlEscape(cr$color %||% "#999"),
          htmltools::htmlEscape(cr$name),
          cr$n_codings
        ))
      }
      pieces <- c(pieces, '</ul>')
    }

    if (include_excerpts) {
      excerpts <- tryCatch(qc_theme_excerpts(project, th$id),
                            error = function(e) NULL)
      if (!is.null(excerpts) && nrow(excerpts) > 0L) {
        pieces <- c(pieces, '<h3>Excerpts</h3>')
        for (dn in unique(excerpts$doc_name)) {
          de <- excerpts[excerpts$doc_name == dn, ]
          pieces <- c(pieces, sprintf('<h4>%s</h4>', htmltools::htmlEscape(dn)))
          for (k in seq_len(nrow(de))) {
            ex <- de[k, ]
            pieces <- c(pieces, sprintf(
              '<div class="excerpt">\u201c%s\u201d
               <div class="excerpt-meta">%s &mdash; coder: %s</div></div>',
              htmltools::htmlEscape(trimws(as.character(ex$seltext))),
              htmltools::htmlEscape(as.character(ex$code_name)),
              htmltools::htmlEscape(as.character(ex$coder))
            ))
          }
        }
      }
    }

    pieces <- c(pieces, '</section>')
  }

  pieces <- c(pieces, '</body></html>')

  path <- tempfile(fileext = ".html")
  writeLines(paste(pieces, collapse = "\n"), path, useBytes = FALSE)
  path
}

# --- Themes TXT --------------------------------------------------------------

.export_themes_txt <- function(project, themes, include_excerpts, include_narrative) {
  info  <- qc_project_info(project)
  lines <- character(0)
  sep80 <- strrep("=", 80)
  sep40 <- strrep("-", 40)

  lines <- c(lines,
    sep80,
    paste0(info$name, " \u2014 ANALYTICAL THEMES REPORT"),
    paste0("Generated: ", format(Sys.Date(), "%B %d, %Y")),
    paste0("Themes: ", nrow(themes)),
    sep80, ""
  )

  for (i in seq_len(nrow(themes))) {
    th <- themes[i, ]
    td <- qc_get_theme(project, th$id)

    lines <- c(lines, sep80)
    lines <- c(lines, paste0("THEME ", i, ": ", toupper(th$name)))
    if (nchar(trimws(th$central_concept %||% "")) > 0)
      lines <- c(lines, paste0("Proposition: ", th$central_concept))
    lines <- c(lines, sep80, "")

    if (include_narrative) {
      if (nchar(trimws(th$narrative %||% "")) > 0) {
        lines <- c(lines, "NARRATIVE", sep40)
        lines <- c(lines, strwrap(th$narrative, width = 78), "")
      }
      if (nchar(trimws(th$definition %||% "")) > 0) {
        lines <- c(lines, "DEFINITION", sep40)
        lines <- c(lines, strwrap(th$definition, width = 78), "")
      }
      if (nchar(trimws(th$scope %||% "")) > 0) {
        lines <- c(lines, "SCOPE", sep40)
        lines <- c(lines, strwrap(th$scope, width = 78), "")
      }
    }

    if (nrow(td$linked_cats) > 0 || nrow(td$linked_codes) > 0) {
      lines <- c(lines, "SUPPORTING CODES", sep40)
      for (j in seq_len(nrow(td$linked_cats))) {
        cr <- td$linked_cats[j, ]
        lines <- c(lines, paste0("  [Category] ", cr$name, " (", cr$n_codes, " codes)"))
      }
      for (j in seq_len(nrow(td$linked_codes))) {
        cr <- td$linked_codes[j, ]
        lines <- c(lines, paste0("  * ", cr$name, " (", cr$n_codings, " codings)"))
      }
      lines <- c(lines, "")
    }

    if (include_excerpts) {
      excerpts <- tryCatch(qc_theme_excerpts(project, th$id),
                            error = function(e) NULL)
      if (!is.null(excerpts) && nrow(excerpts) > 0L) {
        lines <- c(lines, "EXCERPTS", sep40)
        for (dn in unique(excerpts$doc_name)) {
          lines <- c(lines, paste0("  ", dn))
          de <- excerpts[excerpts$doc_name == dn, ]
          for (k in seq_len(nrow(de))) {
            ex    <- de[k, ]
            wtext <- strwrap(trimws(as.character(ex$seltext)), width = 72,
                              prefix = "    ", initial = "    \u201c")
            wtext[length(wtext)] <- paste0(wtext[length(wtext)], "\u201d")
            lines  <- c(lines, wtext)
            lines  <- c(lines, paste0("    \u2014 ", ex$code_name,
                                       " | coder: ", ex$coder), "")
          }
        }
      }
    }

    lines <- c(lines, "")
  }

  path <- tempfile(fileext = ".txt")
  writeLines(lines, path, useBytes = FALSE)
  path
}

# --- Themes JSON -------------------------------------------------------------

.export_themes_json <- function(project, themes, include_excerpts) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    rlang::abort("Install the 'jsonlite' package to export JSON.")

  info <- qc_project_info(project)

  out <- list(
    project     = info$name,
    exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    themes = lapply(seq_len(nrow(themes)), function(i) {
      th <- themes[i, ]
      td <- qc_get_theme(project, th$id)

      entry <- list(
        id              = th$id,
        name            = th$name,
        central_concept = th$central_concept,
        narrative       = th$narrative,
        definition      = th$definition,
        scope           = th$scope,
        n_categories    = th$n_categories,
        n_codes         = th$n_codes,
        linked_categories = as.list(td$linked_cats$name),
        linked_codes      = lapply(seq_len(nrow(td$linked_codes)), function(j) {
          list(name = td$linked_codes$name[j], n_codings = td$linked_codes$n_codings[j])
        })
      )

      if (include_excerpts) {
        excerpts <- tryCatch(qc_theme_excerpts(project, th$id),
                              error = function(e) NULL)
        if (!is.null(excerpts) && nrow(excerpts) > 0L) {
          entry$excerpts <- lapply(seq_len(nrow(excerpts)), function(k) {
            ex <- excerpts[k, ]
            list(
              document = ex$doc_name,
              code     = ex$code_name,
              coder    = ex$coder,
              text     = trimws(as.character(ex$seltext)),
              start    = ex$selfirst,
              end      = ex$selast
            )
          })
        }
      }
      entry
    })
  )

  path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, na = "null"), path)
  path
}

# --- Codebook helpers ---------------------------------------------------------

.get_code_examples <- function(project, code_id, n) {
  .query(project$con,
    paste0("SELECT seltext FROM codings WHERE code_id = ? AND status = 1 LIMIT ", n),
    list(as.integer(code_id))
  )$seltext
}

.export_codebook_csv <- function(codes, include_definitions, include_criteria, include_memo) {
  keep <- c("id", "name", "color", "n_codings", "categories")
  if (include_definitions) keep <- c(keep, "definition")
  if (include_criteria)    keep <- c(keep, "criteria")
  if (include_memo)        keep <- c(keep, "memo")
  keep <- intersect(keep, names(codes))

  path <- tempfile(fileext = ".csv")
  utils::write.csv(codes[, keep, drop = FALSE], path, row.names = FALSE)
  path
}

.export_codebook_json <- function(project, codes, include_examples, n_examples) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    rlang::abort("Install 'jsonlite' to export JSON.")
  info <- qc_project_info(project)
  out <- list(
    project     = info$name,
    exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    codes = lapply(seq_len(nrow(codes)), function(i) {
      r <- as.list(codes[i, ])
      if (include_examples)
        r$examples <- .get_code_examples(project, r$id, n_examples)
      r
    })
  )
  path <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, na = "null"), path)
  path
}

.export_codebook_xlsx <- function(project, codes, include_definitions,
                                   include_criteria, include_memo,
                                   include_examples, n_examples) {
  info    <- qc_project_info(project)
  keep    <- c("id", "name", "color", "n_codings", "categories")
  if (include_definitions) keep <- c(keep, "definition")
  if (include_criteria)    keep <- c(keep, "criteria")
  if (include_memo)        keep <- c(keep, "memo")
  keep    <- intersect(keep, names(codes))
  codes_sheet <- codes[, keep, drop = FALSE]

  cats_sheet <- .query(project$con,
    "SELECT cc.name AS category, c.name AS code, c.definition
     FROM code_category_links ccl
     JOIN code_categories cc ON cc.id = ccl.category_id
     JOIN codes c ON c.id = ccl.code_id
     WHERE ccl.status = 1 AND cc.status = 1 AND c.status = 1
     ORDER BY cc.name, c.name")

  sheets <- list(Codes = codes_sheet, Categories = cats_sheet)

  if (include_examples) {
    ex_rows <- lapply(seq_len(nrow(codes)), function(i) {
      exs <- .get_code_examples(project, codes$id[i], n_examples)
      if (length(exs) == 0L) return(NULL)
      tibble::tibble(code = codes$name[i], example = exs)
    })
    ex_df <- do.call(rbind, Filter(Negate(is.null), ex_rows))
    if (!is.null(ex_df) && nrow(ex_df) > 0L)
      sheets$Examples <- ex_df
  }

  path <- tempfile(fileext = ".xlsx")
  .write_xlsx(sheets, path)
  path
}

.export_codebook_docx <- function(project, codes, include_definitions,
                                   include_criteria, include_memo,
                                   include_examples, n_examples) {
  if (!requireNamespace("officer", quietly = TRUE))
    rlang::abort("Install 'officer' to export Word documents.")

  info <- qc_project_info(project)
  doc  <- officer::read_docx()

  doc <- officer::body_add_par(doc, paste0(info$name, " \u2014 Codebook"),
                                style = "heading 1")
  doc <- officer::body_add_par(doc,
    paste0(format(Sys.Date(), "%B %d, %Y"), " \u2022 ", nrow(codes), " codes"),
    style = "Normal")
  doc <- officer::body_add_break(doc)

  for (i in seq_len(nrow(codes))) {
    r <- codes[i, ]
    doc <- officer::body_add_par(doc, as.character(r$name), style = "heading 2")
    doc <- officer::body_add_par(doc,
      paste0("Codings: ", r$n_codings,
             if (nchar(trimws(r$categories)) > 0)
               paste0("  |  Categories: ", r$categories) else ""),
      style = "Normal")

    if (include_definitions && nchar(trimws(r$definition %||% "")) > 0) {
      doc <- officer::body_add_par(doc, "Definition", style = "heading 3")
      doc <- officer::body_add_par(doc, as.character(r$definition), style = "Normal")
    }
    if (include_criteria && nchar(trimws(r$criteria %||% "")) > 0) {
      doc <- officer::body_add_par(doc, "Inclusion / Exclusion Criteria", style = "heading 3")
      doc <- officer::body_add_par(doc, as.character(r$criteria), style = "Normal")
    }
    if (include_memo && nchar(trimws(r$memo %||% "")) > 0) {
      doc <- officer::body_add_par(doc, "Memo", style = "heading 3")
      doc <- officer::body_add_par(doc, as.character(r$memo), style = "Normal")
    }

    if (include_examples) {
      exs <- .get_code_examples(project, r$id, n_examples)
      if (length(exs) > 0L) {
        doc <- officer::body_add_par(doc, "Example Excerpts", style = "heading 3")
        for (ex in exs) {
          doc <- officer::body_add_fpar(doc,
            officer::fpar(
              officer::ftext(paste0("\u201c", trimws(ex), "\u201d"),
                prop = officer::fp_text(italic = TRUE, font.size = 11)),
              fp_p = officer::fp_par(padding.left = 24, padding.bottom = 6)
            )
          )
        }
      }
    }
  }

  path <- tempfile(fileext = ".docx")
  print(doc, target = path)
  path
}

.export_codebook_html <- function(project, codes, include_definitions,
                                   include_criteria, include_memo,
                                   include_examples, n_examples) {
  info <- qc_project_info(project)
  pieces <- character(0)

  css <- "
    body{font-family:system-ui,-apple-system,sans-serif;max-width:900px;margin:2rem auto;
         padding:0 1.5rem;color:#1a1a1a;line-height:1.65}
    h1{font-size:1.6rem;color:#2c3e50;border-bottom:3px solid #2c3e50;padding-bottom:.4rem}
    h2{font-size:1.1rem;font-weight:700;margin:1.5rem 0 .2rem;display:flex;
       align-items:center;gap:.5rem}
    h3{font-size:.8rem;font-weight:700;text-transform:uppercase;letter-spacing:.06em;
       color:#4b5563;margin:1rem 0 .2rem}
    .swatch{display:inline-block;width:14px;height:14px;border-radius:3px;
            border:1px solid rgba(0,0,0,.15);flex-shrink:0}
    .meta{color:#4b5563;font-size:.82rem;margin-bottom:.5rem}
    .excerpt{border-left:3px solid #e2e8f0;padding:.35rem .75rem;font-style:italic;
             font-size:.9rem;color:#374151;margin:.25rem 0;background:#f8f9fa}
    .code-card{border:1px solid #e2e8f0;border-radius:6px;padding:1rem 1.25rem;
               margin-bottom:1rem}
    .meta span{color:#4b5563}
  "

  pieces <- c(pieces, sprintf(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n
<title>%s \u2014 Codebook</title>\n<style>%s</style>\n</head>\n<body>",
    htmltools::htmlEscape(info$name), css
  ))
  pieces <- c(pieces, sprintf(
    "<h1>%s \u2014 Codebook</h1><p class=\"meta\">%s &bull; %d codes</p>",
    htmltools::htmlEscape(info$name),
    format(Sys.Date(), "%B %d, %Y"),
    nrow(codes)
  ))

  for (i in seq_len(nrow(codes))) {
    r <- codes[i, ]
    color <- htmltools::htmlEscape(r$color %||% "#adb5bd")
    pieces <- c(pieces, sprintf('<div class="code-card">'))
    pieces <- c(pieces, sprintf(
      '<h2><span class="swatch" style="background:%s"></span>%s</h2>
       <p class="meta"><span>%d codings</span>%s</p>',
      color,
      htmltools::htmlEscape(r$name),
      r$n_codings,
      if (nchar(trimws(r$categories)) > 0)
        paste0(" &bull; <span>", htmltools::htmlEscape(r$categories), "</span>")
      else ""
    ))

    if (include_definitions && nchar(trimws(r$definition %||% "")) > 0)
      pieces <- c(pieces, sprintf('<h3>Definition</h3><p>%s</p>',
        htmltools::htmlEscape(r$definition)))
    if (include_criteria && nchar(trimws(r$criteria %||% "")) > 0)
      pieces <- c(pieces, sprintf('<h3>Criteria</h3><p>%s</p>',
        htmltools::htmlEscape(r$criteria)))
    if (include_memo && nchar(trimws(r$memo %||% "")) > 0)
      pieces <- c(pieces, sprintf('<h3>Memo</h3><p>%s</p>',
        htmltools::htmlEscape(r$memo)))

    if (include_examples) {
      exs <- .get_code_examples(project, r$id, n_examples)
      if (length(exs) > 0L) {
        pieces <- c(pieces, '<h3>Examples</h3>')
        for (ex in exs)
          pieces <- c(pieces, sprintf('<div class="excerpt">\u201c%s\u201d</div>',
            htmltools::htmlEscape(trimws(ex))))
      }
    }

    pieces <- c(pieces, '</div>')
  }

  pieces <- c(pieces, '</body></html>')

  path <- tempfile(fileext = ".html")
  writeLines(paste(pieces, collapse = "\n"), path, useBytes = FALSE)
  path
}
