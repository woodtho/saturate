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
  "coder", "coding_source", "coding_status", "confidence",
  # annotations
  "annotation", "position",
  # code relations
  "relation_type", "note", "name_1", "name_2",
  # unit coding
  "unit_n",
  # query results
  "code1_id", "code1_name", "code2_id", "code2_name",
  "attribute_value", "n_documents",
  "c1_start", "c1_end", "c1_text", "c2_start", "c2_end", "c2_text", "gap",
  "must_have", "must_not",
  # search
  "match_start", "match_end", "match_text", "context", "match_n",
  # reliability
  "kappa", "pct_agree", "n_agree", "n_docs", "n11", "n10", "n01", "n00",
  # code history / audit
  "operation", "field", "old_value", "new_value", "changed_at", "changed_by",
  "event_type",
  # document metadata
  "filename", "source_system", "language", "doc_version",
  "content_hash", "word_count",
  # duplicate detection
  "source_id_1", "name_1", "source_id_2", "name_2", "similarity", "type",
  # version history
  "version", "imported_at", "segment_n",
  # snapshots
  "label", "snapshot_json", "snapshot_id", "n_codes",
  # graph
  "from_id", "to_id", "shared", "shared_codes",
  "from_name", "to_name", "from_color", "to_color",
  "group", "shape",
  # codebook management
  "code_key", "deprecated", "deprecated_reason",
  "issue_type", "severity",
  "change_type",
  # compare module
  "coder_label", "left_label", "right_label",
  # navigation
  "start", "end",
  # saturation curve
  "doc_index", "doc_name", "new_codes", "cumulative_codes",
  # triangulation
  "source_type", "total",
  # member checks
  "participant_label", "item_status", "participant_response",
  "check_id", "n_items", "n_confirmed", "n_disputed",
  "code_color",
  # excerpts
  "excerpt_id",
  # project memos / journal
  "memo_type", "created_by",
  # themes
  "central_concept", "narrative", "theme_id",
  # coding orientation
  "level", "orientation",
  # word cloud
  "word", "freq",
  # code weights
  "weight", "weight_description"
))
