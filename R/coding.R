#' Add a coded segment
#'
#' Records that the passage from character offset `selfirst` to `selast`
#' (1-based, both inclusive, matching `substr(content, selfirst, selast)`) in
#' document `source_id` is tagged with `code_id`. The passage text is
#' snapshotted into `seltext` at write time.
#'
#' @param project A `qc_project` object.
#' @param source_id Integer. Document id.
#' @param code_id Integer. Code id.
#' @param selfirst Integer. 1-based start character position.
#' @param selast Integer. 1-based end character position (inclusive).
#' @param memo Character. Optional per-segment memo.
#' @param coder Character. Coder identifier (username or label).
#' @param coding_source One of `"manual"` or `"auto"`.
#' @param coding_status One of `"draft"` or `"validated"`.
#'
#' @return A one-row tibble: `id`, `source_id`, `code_id`, `selfirst`,
#'   `selast`, `seltext`, `memo`, `coder`, `coding_source`,
#'   `coding_status`, `created_at`.
#' @export
qc_add_coding <- function(project, source_id, code_id,
                          selfirst, selast, memo = "",
                          coder         = "default",
                          coding_source = "manual",
                          coding_status = "validated") {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  source_id <- as.integer(source_id)
  code_id   <- as.integer(code_id)
  selfirst  <- as.integer(selfirst)
  selast    <- as.integer(selast)
  if (selfirst < 1L) rlang::abort("`selfirst` must be >= 1.")
  if (selast < selfirst) rlang::abort("`selast` must be >= `selfirst`.")

  doc     <- qc_get_document(project, source_id)
  seltext <- substr(doc$content, selfirst, selast)

  .query(project$con,
    "INSERT INTO codings
       (source_id, code_id, selfirst, selast, seltext, memo,
        coder, coding_source, coding_status)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
     RETURNING id, source_id, code_id, selfirst, selast, seltext, memo,
               coder, coding_source, coding_status, created_at",
    list(source_id, code_id, selfirst, selast, seltext, memo %||% "",
         coder %||% "default",
         coding_source %||% "manual",
         coding_status %||% "validated")
  )
}

#' List codings, optionally filtered by document and/or code
#'
#' @param project A `qc_project` object.
#' @param source_id Integer or `NULL`. Restrict to a single document.
#' @param code_id Integer or `NULL`. Restrict to a single code.
#'
#' @return A tibble: `id`, `source_id`, `code_id`, `code_name`,
#'   `code_color`, `selfirst`, `selast`, `seltext`, `memo`, `created_at`.
#'   Ordered by `selfirst`.
#' @export
qc_list_codings <- function(project, source_id = NULL, code_id = NULL) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  conds  <- "cod.status = 1"
  params <- list()
  if (!is.null(source_id)) {
    conds  <- paste0(conds, " AND cod.source_id = ?")
    params <- c(params, list(as.integer(source_id)))
  }
  if (!is.null(code_id)) {
    conds  <- paste0(conds, " AND cod.code_id = ?")
    params <- c(params, list(as.integer(code_id)))
  }
  .query(project$con, paste0("
    SELECT cod.id, cod.source_id, cod.code_id,
           c.name  AS code_name,
           c.color AS code_color,
           cod.selfirst, cod.selast, cod.seltext, cod.memo,
           cod.created_at
    FROM   codings cod
    JOIN   codes c ON c.id = cod.code_id
    WHERE  ", conds, "
    ORDER  BY cod.selfirst
  "), params)
}

#' Delete a single coding (soft delete)
#'
#' @param project A `qc_project` object.
#' @param id Integer. The coding id.
#'
#' @return Invisibly, `1L`.
#' @export
qc_delete_coding <- function(project, id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  .soft_delete(project$con, "codings", "id", as.integer(id))
  invisible(1L)
}

#' Reassign a coding to a different code
#'
#' Moves a single coding from its current code to `new_code_id`. Useful after
#' splitting a code to redistribute existing passages.
#'
#' @param project A `qc_project` object.
#' @param coding_id Integer. The coding to reassign.
#' @param new_code_id Integer. The target code.
#'
#' @return Invisibly, `TRUE`.
#' @export
qc_reassign_coding <- function(project, coding_id, new_code_id) {
  assert_class(project, "qc_project")
  assert_con(project$con)
  .assert_unlocked(project)
  .exec(project$con,
    "UPDATE codings SET code_id = ? WHERE id = ? AND status = 1",
    list(as.integer(new_code_id), as.integer(coding_id))
  )
  invisible(TRUE)
}

# Build highlighted HTML from document content and a codings tibble.
# Used by the Shiny coding panel.
build_highlighted_html <- function(content, codings) {
  n <- nchar(content)
  if (nrow(codings) == 0L || n == 0L) {
    return(htmltools::div(
      content,
      class = "qc-text-display",
      style = .text_display_style()
    ))
  }

  # All segment boundaries: character positions where highlighting changes
  breaks <- sort(unique(c(1L, codings$selfirst, codings$selast + 1L, n + 1L)))
  breaks <- breaks[breaks >= 1L & breaks <= n + 1L]

  parts <- vector("list", length(breaks) - 1L)
  for (i in seq_along(parts)) {
    seg_start <- breaks[i]
    seg_end   <- breaks[i + 1L] - 1L
    seg_text  <- substr(content, seg_start, seg_end)

    active <- codings[
      codings$selfirst <= seg_start & codings$selast >= seg_start, ]

    if (nrow(active) == 0L) {
      parts[[i]] <- seg_text
    } else {
      col <- active$code_color[[1L]]
      tip <- paste(active$code_name, collapse = ", ")
      # Append "55" hex (~33 % opacity) to a 6-digit hex colour
      bg  <- if (grepl("^#[0-9A-Fa-f]{6}$", col)) paste0(col, "55") else col
      parts[[i]] <- htmltools::tags$mark(
        seg_text,
        style = paste0("background-color:", bg, "; cursor:pointer;"),
        title = tip,
        `data-selfirst` = seg_start,
        `data-selast`   = seg_end
      )
    }
  }

  htmltools::div(
    class = "qc-text-display",
    style = .text_display_style(),
    htmltools::tagList(parts)
  )
}

.text_display_style <- function() {
  paste0(
    "white-space: pre-wrap; font-family: Georgia, serif; line-height: 1.9; ",
    "padding: 1.2rem; height: 68vh; overflow-y: scroll; ",
    "font-size: 0.95rem; border: 1px solid #dee2e6; border-radius: 4px; ",
    "user-select: text;"
  )
}
