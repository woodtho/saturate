qc_help_note <- function(...) {
  shiny::div(
    class = "qc-help-note",
    shiny::icon("info-circle", class = "qc-help-icon"),
    shiny::div(class = "qc-help-note-body", ...)
  )
}

qc_help_details <- function(title, ..., open = FALSE) {
  attrs <- list(class = "qc-help-details")
  if (isTRUE(open)) attrs$open <- NA

  do.call(
    shiny::tags$details,
    c(
      attrs,
      list(
        shiny::tags$summary(
          shiny::icon("question-circle", class = "qc-help-icon"),
          shiny::span(title)
        ),
        shiny::div(class = "qc-help-body", ...)
      )
    )
  )
}

qc_help_list <- function(items) {
  shiny::tags$ul(
    class = "qc-help-list",
    lapply(items, shiny::tags$li)
  )
}

mod_help_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "p-3 qc-help-page",
    shiny::div(
      class = "qc-help-hero",
      shiny::h4("Help"),
      shiny::p(
        "Use this guide as a project map: import documents, build a codebook, ",
        "code passages, compare interpretations, develop themes, then export ",
        "the evidence trail."
      ),
      shiny::uiOutput(ns("project_summary"))
    ),

    bslib::navset_card_underline(
      id = ns("help_tabs"),

      bslib::nav_panel(
        "Start",
        shiny::div(
          class = "qc-help-section",
          shiny::h5("Recommended workflow"),
          shiny::tags$ol(
            class = "qc-help-steps",
            shiny::tags$li("Import documents in Documents."),
            shiny::tags$li("Create a small starter codebook in Codebook."),
            shiny::tags$li("Open a document from Documents and apply codes in Coding."),
            shiny::tags$li("Review coder agreement or document differences in Compare."),
            shiny::tags$li("Use Query, Graph, and Themes to develop analysis."),
            shiny::tags$li("Export reports, codebooks, and raw data from Export.")
          ),
          qc_help_note(
            shiny::strong("Tip: "),
            "Start with a few broad codes, then refine definitions and criteria ",
            "as patterns become clearer."
          )
        )
      ),

      bslib::nav_panel(
        "Documents",
        shiny::div(
          class = "qc-help-section",
          shiny::h5("Documents"),
          qc_help_list(c(
            "Import text, Word, PDF, Markdown, or CSV files; pasted text is also supported.",
            "Use source types such as interview, focus_group, survey, observation, or document for later triangulation.",
            "Editing a document creates a new version and flags existing codings for review."
          )),
          shiny::h5("Coding"),
          qc_help_list(c(
            "Select text in the document pane, choose a code, then apply it.",
            "Use confidence and segment memos when the interpretation is provisional.",
            "Blind mode hides other coders' work so coding decisions can be made independently.",
            "The Previous, Next, and Disputed controls help move through uncoded or contested passages."
          ))
        )
      ),

      bslib::nav_panel(
        "Codebook",
        shiny::div(
          class = "qc-help-section",
          shiny::h5("Codebook"),
          qc_help_list(c(
            "Definitions say what the code means; criteria say what belongs in or out.",
            "Categories group related codes and make filtering easier in Coding, Query, and Themes.",
            "History records codebook edits so changes can be audited later."
          )),
          shiny::h5("Themes"),
          qc_help_list(c(
            "Themes are analytical claims, not just topic labels.",
            "Link themes to categories or individual codes to collect supporting excerpts.",
            "Use the analytical statement field for the claim you expect to report."
          ))
        )
      ),

      bslib::nav_panel(
        "Analysis",
        shiny::div(
          class = "qc-help-section",
          shiny::h5("Query and visual tools"),
          qc_help_list(c(
            "OR filters return passages with any selected code; AND filters require additional codes.",
            "NOT filters exclude passages containing selected codes.",
            "Co-occurrence, saturation, triangulation, and cross-tab views reuse the same filter context.",
            "Graph views are useful for seeing shared codes across documents or code relationships."
          )),
          shiny::h5("Compare"),
          qc_help_list(c(
            "Compare two documents to inspect code distribution differences.",
            "Compare two coders on the same document to find unique, agreed, and conflicting interpretations."
          ))
        )
      ),

      bslib::nav_panel(
        "Review",
        shiny::div(
          class = "qc-help-section",
          shiny::h5("Member checks"),
          qc_help_list(c(
            "Create a check for one document and optionally limit it to selected codes.",
            "Export the check for participant review, then record confirmed, disputed, or other responses.",
            "Participant responses become part of the project evidence trail."
          )),
          shiny::h5("Audit and export"),
          qc_help_list(c(
            "Audit shows code and coding changes with filters by operation, document, and date.",
            "Export can produce analytical reports, codebooks, or raw project tables.",
            "Use raw exports when you need to inspect or archive project data outside the app."
          ))
        )
      ),

      bslib::nav_panel(
        "Shortcuts",
        shiny::div(
          class = "qc-help-section",
          shiny::h5("Coding shortcuts"),
          shiny::tags$dl(
            class = "qc-help-shortcuts",
            shiny::tags$dt(shiny::tags$kbd("1"), " - ", shiny::tags$kbd("9")),
            shiny::tags$dd("Apply the Nth visible code to the selected text."),
            shiny::tags$dt(shiny::tags$kbd("Enter")),
            shiny::tags$dd("Apply the current code to the selected text."),
            shiny::tags$dt(shiny::tags$kbd("Esc")),
            shiny::tags$dd("Clear the current selection display."),
            shiny::tags$dt(shiny::tags$kbd("/")),
            shiny::tags$dd("Focus the code selector."),
            shiny::tags$dt(shiny::tags$kbd("n"), " / ", shiny::tags$kbd("p")),
            shiny::tags$dd("Jump to the next or previous uncoded segment."),
            shiny::tags$dt(shiny::tags$kbd("d")),
            shiny::tags$dd("Jump to the next disputed or draft segment."),
            shiny::tags$dt(shiny::tags$kbd("?")),
            shiny::tags$dd("Open the shortcuts dialog.")
          )
        )
      )
    )
  )
}

mod_help_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    output$project_summary <- shiny::renderUI({
      if (is.null(rv) || is.null(rv$project)) return(NULL)

      n_docs <- tryCatch(nrow(qc_list_documents(rv$project)), error = function(e) 0L)
      n_codes <- tryCatch(nrow(qc_list_codes(rv$project)), error = function(e) 0L)
      n_codings <- tryCatch(
        .query(rv$project$con, "SELECT COUNT(*) AS n FROM codings WHERE status = 1")$n[[1]],
        error = function(e) 0L
      )
      n_themes <- tryCatch(nrow(qc_list_themes(rv$project)), error = function(e) 0L)

      shiny::div(
        class = "qc-help-stats",
        .qc_help_stat(n_docs, "documents"),
        .qc_help_stat(n_codes, "codes"),
        .qc_help_stat(n_codings, "codings"),
        .qc_help_stat(n_themes, "themes")
      )
    })
  })
}

.qc_help_stat <- function(value, label) {
  shiny::div(
    class = "qc-help-stat",
    shiny::div(class = "qc-help-stat-value", format(value, big.mark = ",")),
    shiny::div(class = "qc-help-stat-label", label)
  )
}
