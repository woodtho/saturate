#' saturate: Qualitative Text Coding with DuckDB and Shiny
#'
#' @description
#' A modern replacement for RQDA. Manage coding projects in DuckDB, annotate
#' text passages programmatically or via a Shiny GUI, and retrieve coded
#' segments as tidy tibbles.
#'
#' ## Core workflow
#' 1. Create or open a project with [qc_new()] / [qc_open()].
#' 2. Import documents with [qc_import_document()].
#' 3. Define codes with [qc_add_code()].
#' 4. Code passages with [qc_add_coding()] or interactively via [shiny_saturate()].
#' 5. Retrieve results with [qc_get_coded_segments()].
#' 6. Close the project with [qc_close()].
#'
#' @docType package
#' @name saturate-package
#' @aliases saturate
"_PACKAGE"

utils::globalVariables(c(
  "id", "name", "status", "source_id", "code_id",
  "case_id", "case_name", "category_id",
  "selfirst", "selast", "seltext", "memo", "content", "color",
  "code_name", "code_color", "category_name", "source_name", "created_at",
  "n_codings", "n_segments", "n_documents", "variable", "value",
  "coding_id", "category_names", "categories",
  # hierarchy / definitions
  "parent_id", "parent_name", "definition", "criteria", "depth",
  # coder tracking
  "coder", "coding_source", "coding_status",
  # query results
  "code1_id", "code1_name", "code2_id", "code2_name",
  "attribute_value", "n_documents",
  "c1_start", "c1_end", "c1_text", "c2_start", "c2_end", "c2_text", "gap",
  "must_have", "must_not",
  # search
  "match_start", "match_end", "match_text", "context", "match_n",
  # reliability
  "kappa", "pct_agree", "n_agree", "n_docs", "n11", "n10", "n01", "n00",
  # snapshots
  "label", "snapshot_json", "snapshot_id", "n_codes",
  # graph
  "from_id", "to_id", "shared", "shared_codes",
  "from_name", "to_name", "from_color", "to_color",
  "group", "shape"
))
