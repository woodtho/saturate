# Development smoke/demo project for the current saturate app.
#
# Run with:
#   source("test_app.R")
#
# The script rebuilds a local .satdb project with enough data to exercise the
# main UI paths: document import, timestamped transcripts, line/timestamp
# rendering, coding, codebook metadata, categories, code relations, cases,
# memos, annotations, excerpts, themes, member checks, triangulation, and
# saturation. It launches the app when run interactively.

devtools::load_all()

project_path <- file.path(tempdir(), "saturate_demo.satdb")

# -- Helpers -----------------------------------------------------------------

span_for <- function(text, pattern, occurrence = 1L) {
  hits <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (length(hits) == 0L || hits[[1L]] == -1L || length(hits) < occurrence) {
    stop("Could not find occurrence ", occurrence, " of: ", pattern, call. = FALSE)
  }
  start <- hits[[occurrence]]
  c(selfirst = start, selast = start + nchar(pattern) - 1L)
}

doc_span <- function(project, source_id, pattern, occurrence = 1L) {
  span_for(qc_get_document(project, source_id)$content, pattern, occurrence)
}

add_phrase_coding <- function(project, source_id, code_id, pattern,
                              coder = "coder_a", occurrence = 1L,
                              memo = "", confidence = NA_real_) {
  sp <- doc_span(project, source_id, pattern, occurrence)
  conf <- if (is.na(confidence)) NULL else {
    if (confidence <= 1) round(confidence * 100) else round(confidence)
  }
  qc_add_coding(
    project, source_id, code_id,
    selfirst = sp[["selfirst"]],
    selast   = sp[["selast"]],
    coder    = coder,
    memo     = memo,
    confidence = conf
  )
}

id_for <- function(df, name_col = "name") {
  force(df)
  force(name_col)
  function(name) {
    out <- df$id[df[[name_col]] == name]
    if (!length(out)) stop("No id for: ", name, call. = FALSE)
    out[[1L]]
  }
}

cat_section <- function(label) {
  cat("\n--- ", label, " ", strrep("-", max(1, 68 - nchar(label))), "\n", sep = "")
}

# -- 1. Fresh project ---------------------------------------------------------
if(!file.exists(project_path)){
proj <- qc_new(
  project_path,
  name = "Current App Smoke Study",
  owner = "Researcher",
  overwrite = TRUE
)

# -- 2. Documents, including a timestamped transcript -------------------------

docs_seed <- list(
  list(
    name = "Small Tech Firm",
    source_type = "interview",
    memo = "Semi-structured interview with founder.",
    content = paste(
      "We've been trying to scale our software platform for the past two years,",
      "but hiring has been a persistent bottleneck.",
      "It's not just about finding developers; it's finding people who understand",
      "both the technical side and the regulatory requirements in our sector.",
      "We've had to turn down projects because we could not staff them properly.",
      "At the same time, our R&D spending has increased, especially on adapting",
      "our product for international markets, but the return has not been immediate.",
      "Government funding applications take months, and by the time we hear back,",
      "the opportunity has often passed.",
      "Collaboration with universities has helped, but it depends heavily on",
      "individual relationships.",
      sep = "\n"
    )
  ),
  list(
    name = "Manufacturing Firm",
    source_type = "interview",
    memo = "Operations manager interview.",
    content = paste(
      "In manufacturing, innovation does not always look like new products.",
      "For us, it is about improving processes and reducing waste.",
      "Over the last year, we invested in automation equipment, which improved",
      "efficiency, but the upfront costs were significant.",
      "Energy prices have also been volatile, which affects our margins directly.",
      "We considered expanding into new markets, but logistics and supply chain",
      "disruptions have made that risky.",
      "Hiring skilled tradespeople is another challenge; we often train internally,",
      "which takes time and resources.",
      sep = "\n"
    )
  ),
  list(
    name = "Life Sciences Company",
    source_type = "interview",
    memo = "Interview with R&D director.",
    content = paste(
      "Our business is heavily driven by R&D, and timelines are long.",
      "It can take years before a product reaches the market, and even then,",
      "regulatory approval is uncertain.",
      "We rely on a mix of private investment and public funding to sustain",
      "operations during that period.",
      "Recently, we have seen increased competition for talent, particularly",
      "in specialized research roles.",
      "Collaboration is critical; we work closely with academic institutions and",
      "hospitals, but coordinating across organizations can slow things down.",
      sep = "\n"
    )
  ),
  list(
    name = "Service Firm",
    source_type = "focus_group",
    memo = "Leadership team focus group.",
    content = paste(
      "We accelerated our digital transformation during the pandemic, moving many",
      "of our services online.",
      "That improved accessibility for clients, but it required retraining staff",
      "and investing in new systems.",
      "Some employees adapted quickly, but others struggled, especially those less",
      "familiar with digital tools.",
      "Cybersecurity has become a bigger concern as well, adding cost and complexity.",
      sep = "\n"
    )
  ),
  list(
    name = "Large Multinational",
    source_type = "document",
    memo = "Strategy memo excerpt imported as a document source.",
    content = paste(
      "As a large organization operating in multiple countries, coordination is",
      "one of our biggest challenges.",
      "Decisions made at headquarters do not always translate well to local contexts,",
      "particularly when regulatory environments differ.",
      "We invest significantly in R&D, but aligning those investments with market",
      "needs requires constant adjustment.",
      "Our scale allows us to absorb risks that smaller firms cannot, and we benefit",
      "from established relationships with governments and partners.",
      "Recently, we have focused more on sustainability initiatives.",
      sep = "\n"
    )
  ),
  list(
    name = "Transcript with Timestamps",
    source_type = "interview",
    memo = "Simulates output from Record & Transcribe with timestamps enabled.",
    content = paste(
      "[00:00:00] Interviewer: Can you describe the biggest barrier to growth?",
      "[00:00:07] Participant: Hiring is still the bottleneck. We can find generalists, but not people with regulated product experience.",
      "[00:00:19] Participant: The playback review helped us match the transcript to the original recording when we checked this section.",
      "[00:00:31] Interviewer: How do funding programs affect timing?",
      "[00:00:36] Participant: Grants help, but the approval cycle is too slow for market windows.",
      "[00:00:48] Participant: University partners help us test ideas, although the relationship depends on specific people.",
      sep = "\n"
    )
  )
)

for (doc in docs_seed) {
  qc_import_document(
    proj,
    name = doc$name,
    content = doc$content,
    source_type = doc$source_type,
    memo = doc$memo
  )
}

docs <- qc_list_documents(proj, include_content = TRUE)
doc_id <- id_for(docs)

# -- 3. Codebook with metadata, hierarchy, categories, and relations -----------

parent_people <- qc_add_code(
  proj, "people_and_capacity",
  color = "#6C757D",
  definition = "Parent code for people, skills, and capacity constraints.",
  criteria = "Use child codes for specific mechanisms.",
  level = "organizing",
  orientation = "capacity"
)$id

qc_add_code(proj, "talent_constraints",
  color = "#E15759",
  parent_id = parent_people,
  definition = "Difficulties recruiting, retaining, or developing skilled workers.",
  criteria = "Include hiring bottlenecks, skills gaps, and training burden.",
  level = "interpretive",
  orientation = "barrier",
  weight = -0.8,
  weight_description = "Negative pressure on innovation capacity.")

qc_add_code(proj, "rd_investment",
  color = "#4E79A7",
  definition = "Spending on research, development, product, or process innovation.",
  criteria = "Include R&D spend, automation capex, product development.",
  level = "descriptive",
  orientation = "enabler",
  weight = 0.6,
  weight_description = "Investment that may support innovation.")

qc_add_code(proj, "regulatory_environment",
  color = "#F28E2B",
  definition = "Regulatory requirements, approvals, or compliance burdens.",
  criteria = "Include sector regulations, approval processes, compliance costs.",
  level = "contextual",
  orientation = "constraint",
  weight = -0.5)

qc_add_code(proj, "government_programs",
  color = "#76B7B2",
  definition = "Public funding, incentive programs, or government-supported initiatives.",
  criteria = "Include grants, tax incentives, and program timing.",
  level = "contextual",
  orientation = "mixed",
  weight = 0.2)

qc_add_code(proj, "collaboration",
  color = "#59A14F",
  definition = "Partnerships with external organizations for knowledge or resource access.",
  criteria = "Include university links, clusters, hospitals, and partner networks.",
  level = "interpretive",
  orientation = "enabler",
  weight = 0.7)

qc_add_code(proj, "digital_transformation",
  color = "#B07AA1",
  definition = "Adoption of digital technologies to change service delivery or operations.",
  criteria = "Include online services, automation, digital tools, and cybersecurity.",
  level = "descriptive",
  orientation = "change process")

qc_add_code(proj, "supply_chain_disruption",
  color = "#FF9DA7",
  definition = "Disruptions to logistics, materials, or external supply.",
  criteria = "Include supply delays, logistics risk, materials volatility.",
  level = "descriptive",
  orientation = "barrier",
  weight = -0.6)

qc_add_code(proj, "scaling_challenges",
  color = "#9C755F",
  definition = "Barriers to growth related to capacity, capital, coordination, or timing.",
  criteria = "Include inability to take on projects or coordination failures.",
  level = "interpretive",
  orientation = "barrier",
  weight = -0.7)

codes <- qc_list_codes(proj)
code_id <- id_for(codes)

qc_add_category(proj, "People & Skills")
qc_add_category(proj, "Innovation & Investment")
qc_add_category(proj, "External Environment")

cats <- .query(proj$con, "SELECT id, name FROM code_categories WHERE status = 1")
cat_id <- id_for(cats)

qc_link_code_category(proj, code_id("people_and_capacity"), cat_id("People & Skills"))
qc_link_code_category(proj, code_id("talent_constraints"), cat_id("People & Skills"))
qc_link_code_category(proj, code_id("digital_transformation"), cat_id("People & Skills"))
qc_link_code_category(proj, code_id("rd_investment"), cat_id("Innovation & Investment"))
qc_link_code_category(proj, code_id("scaling_challenges"), cat_id("Innovation & Investment"))
qc_link_code_category(proj, code_id("collaboration"), cat_id("Innovation & Investment"))
qc_link_code_category(proj, code_id("regulatory_environment"), cat_id("External Environment"))
qc_link_code_category(proj, code_id("government_programs"), cat_id("External Environment"))
qc_link_code_category(proj, code_id("supply_chain_disruption"), cat_id("External Environment"))

qc_add_code_relation(
  proj, code_id("government_programs"), code_id("scaling_challenges"),
  relation_type = "precedes",
  note = "Slow program decisions can cause missed market windows."
)
qc_add_code_relation(
  proj, code_id("talent_constraints"), code_id("scaling_challenges"),
  relation_type = "co_occurs_with",
  note = "Capacity constraints are often described as growth constraints."
)

qc_snapshot_codebook(proj, "Initial current-app demo codebook")

# -- 4. Programmatic codings across source types and timestamped lines ---------

add_phrase_coding(proj, doc_id("Small Tech Firm"), code_id("talent_constraints"),
  "hiring has been a persistent bottleneck",
  memo = "Hiring bottleneck explicitly named as the main growth constraint.",
  confidence = 0.95)
add_phrase_coding(proj, doc_id("Small Tech Firm"), code_id("regulatory_environment"),
  "regulatory requirements in our sector")
add_phrase_coding(proj, doc_id("Small Tech Firm"), code_id("scaling_challenges"),
  "turn down projects because we could not staff them properly")
add_phrase_coding(proj, doc_id("Small Tech Firm"), code_id("rd_investment"),
  "R&D spending has increased")
add_phrase_coding(proj, doc_id("Small Tech Firm"), code_id("government_programs"),
  "Government funding applications take months")
add_phrase_coding(proj, doc_id("Small Tech Firm"), code_id("collaboration"),
  "Collaboration with universities has helped")

add_phrase_coding(proj, doc_id("Manufacturing Firm"), code_id("rd_investment"),
  "invested in automation equipment")
add_phrase_coding(proj, doc_id("Manufacturing Firm"), code_id("supply_chain_disruption"),
  "logistics and supply chain")
add_phrase_coding(proj, doc_id("Manufacturing Firm"), code_id("talent_constraints"),
  "Hiring skilled tradespeople is another challenge")

add_phrase_coding(proj, doc_id("Life Sciences Company"), code_id("rd_investment"),
  "heavily driven by R&D")
add_phrase_coding(proj, doc_id("Life Sciences Company"), code_id("regulatory_environment"),
  "regulatory approval is uncertain")
add_phrase_coding(proj, doc_id("Life Sciences Company"), code_id("government_programs"),
  "public funding to sustain")
add_phrase_coding(proj, doc_id("Life Sciences Company"), code_id("talent_constraints"),
  "increased competition for talent")
add_phrase_coding(proj, doc_id("Life Sciences Company"), code_id("collaboration"),
  "Collaboration is critical")

add_phrase_coding(proj, doc_id("Service Firm"), code_id("digital_transformation"),
  "digital transformation during the pandemic")
add_phrase_coding(proj, doc_id("Service Firm"), code_id("talent_constraints"),
  "required retraining staff")
add_phrase_coding(proj, doc_id("Service Firm"), code_id("digital_transformation"),
  "Cybersecurity has become a bigger concern")

add_phrase_coding(proj, doc_id("Large Multinational"), code_id("scaling_challenges"),
  "one of our biggest challenges")
add_phrase_coding(proj, doc_id("Large Multinational"), code_id("regulatory_environment"),
  "regulatory environments differ")
add_phrase_coding(proj, doc_id("Large Multinational"), code_id("rd_investment"),
  "invest significantly in R&D")
add_phrase_coding(proj, doc_id("Large Multinational"), code_id("collaboration"),
  "relationships with governments and partners")

timestamp_doc <- doc_id("Transcript with Timestamps")
add_phrase_coding(proj, timestamp_doc, code_id("talent_constraints"),
  "Hiring is still the bottleneck",
  memo = "Timestamped transcript line should render with timestamp gutter.",
  confidence = 0.9)
add_phrase_coding(proj, timestamp_doc, code_id("scaling_challenges"),
  "match the transcript to the original recording")
add_phrase_coding(proj, timestamp_doc, code_id("government_programs"),
  "approval cycle is too slow for market windows")
add_phrase_coding(proj, timestamp_doc, code_id("collaboration"),
  "University partners help us test ideas")

# Second coder for blind-mode and reliability UI.
add_phrase_coding(proj, timestamp_doc, code_id("talent_constraints"),
  "not people with regulated product experience",
  coder = "coder_b",
  memo = "Second coder sees this as a specialized skills constraint.",
  confidence = 0.8)
add_phrase_coding(proj, doc_id("Service Firm"), code_id("digital_transformation"),
  "services online",
  coder = "coder_b",
  confidence = 0.85)

# -- 5. Cases, attributes, annotations, excerpts, and journal ------------------

for (nm in c("Small Tech Firm", "Manufacturing Firm", "Life Sciences Company",
             "Service Firm", "Large Multinational", "Transcript Participant")) {
  qc_add_case(proj, nm)
}

cases <- qc_list_cases(proj)
case_id <- id_for(cases)

case_doc_links <- c(
  "Small Tech Firm" = "Small Tech Firm",
  "Manufacturing Firm" = "Manufacturing Firm",
  "Life Sciences Company" = "Life Sciences Company",
  "Service Firm" = "Service Firm",
  "Large Multinational" = "Large Multinational",
  "Transcript Participant" = "Transcript with Timestamps"
)
for (nm in names(case_doc_links)) {
  qc_link_case_source(proj, case_id(nm), doc_id(case_doc_links[[nm]]))
}

attrs <- list(
  "Small Tech Firm" = list(sector = "technology", size = "small", rd_intensity = "high", method = "interview"),
  "Manufacturing Firm" = list(sector = "manufacturing", size = "medium", rd_intensity = "medium", method = "interview"),
  "Life Sciences Company" = list(sector = "life_sciences", size = "small", rd_intensity = "high", method = "interview"),
  "Service Firm" = list(sector = "services", size = "medium", rd_intensity = "low", method = "focus_group"),
  "Large Multinational" = list(sector = "technology", size = "large", rd_intensity = "high", method = "document"),
  "Transcript Participant" = list(sector = "technology", size = "small", rd_intensity = "high", method = "recorded_interview")
)
for (nm in names(attrs)) {
  for (var in names(attrs[[nm]])) {
    qc_set_case_attribute(proj, case_id(nm), var, attrs[[nm]][[var]])
  }
}

qc_add_project_memo(
  proj,
  "Decision: retain transcript timestamps during coding so playback review can be matched to coded lines.",
  type = "decision",
  created_by = "Researcher"
)
qc_add_project_memo(
  proj,
  "Reflexivity note: growth barriers may be overrepresented because the interview guide foregrounds constraints.",
  type = "reflexivity",
  created_by = "Researcher"
)

ann_pos <- doc_span(proj, timestamp_doc, "playback review")[["selfirst"]]
qc_add_annotation(
  proj,
  timestamp_doc,
  "Check whether timestamp display and TTS timestamp skipping remain readable here.",
  position = ann_pos,
  coder = "coder_a"
)

excerpt_span <- doc_span(proj, timestamp_doc, "approval cycle is too slow for market windows")
qc_add_excerpt(
  proj,
  timestamp_doc,
  selfirst = excerpt_span[["selfirst"]],
  selast = excerpt_span[["selast"]],
  memo = "Good excerpt for showing playback/timestamp alignment in member checks.",
  coder = "coder_a"
)

# -- 6. Themes and member check -----------------------------------------------

qc_add_theme(
  proj,
  "Capacity constraints shape innovation timing",
  central_concept = "Organizations describe innovation as dependent on people, timing, and external coordination.",
  narrative = paste(
    "Across interviews, investment and collaboration appear useful only when",
    "firms can align skilled labour, funding cycles, and regulatory timing."
  ),
  definition = "Includes passages linking internal capacity to delayed or constrained innovation.",
  scope = "Exclude generic descriptions of innovation without a timing or capacity mechanism.",
  code_ids = c(code_id("talent_constraints"), code_id("scaling_challenges"),
               code_id("government_programs"), code_id("collaboration")),
  category_ids = c(cat_id("People & Skills"), cat_id("Innovation & Investment"))
)

mc <- qc_create_member_check(
  proj,
  timestamp_doc,
  participant_label = "Transcript Participant",
  code_ids = c(code_id("talent_constraints"), code_id("government_programs")),
  created_by = "Researcher",
  return_by = as.character(Sys.Date() + 14L),
  return_to = "researcher@example.org",
  return_instructions = "Review the timestamped excerpts against the recording and reply with corrections.",
  notes = "Demo member check for timestamped transcript workflow."
)

# Build exports in temp files to exercise handlers without polluting the repo.
qc_export_member_check(proj, mc$id, path = tempfile(fileext = ".html"), format = "html")
qc_export_member_check(proj, mc$id, path = tempfile(fileext = ".txt"), format = "txt")

}
# -- 7. Sanity checks for current app features ---------------------------------

cat_section("Documents")
print(qc_list_documents(proj)[, c("id", "name", "source_type", "word_count", "n_codings")])

cat_section("Codes")
codes_report <- qc_list_codes(proj)
code_cols <- intersect(
  c("name", "code_key", "definition", "weight", "n_codings", "categories"),
  names(codes_report)
)
print(codes_report[, code_cols])

cat_section("Timestamped transcript rendering smoke check")
timestamp_content <- qc_get_document(proj, timestamp_doc)$content
timestamp_html <- as.character(build_highlighted_html(
  timestamp_content,
  qc_list_codings(proj, timestamp_doc),
  excerpts = qc_list_excerpts(proj, timestamp_doc),
  show_line_numbers = TRUE,
  show_timestamps = TRUE
))
stopifnot(
  grepl("qc-timestamps-on", timestamp_html, fixed = TRUE),
  grepl("qc-line-numbers-on", timestamp_html, fixed = TRUE),
  grepl("qc-ts", timestamp_html, fixed = TRUE),
  grepl("[00:00:36]", timestamp_content, fixed = TRUE)
)
cat("Timestamped rendering classes found.\n")

cat_section("Segments: talent constraints")
print(qc_get_coded_segments(proj, code_ids = code_id("talent_constraints"))[
  , c("source_name", "coder", "seltext", "memo")
])

cat_section("Triangulation by source type")
print(qc_triangulate(proj))

cat_section("Cross-tab: code x sector")
print(qc_cross_tabulate(proj, attribute = "sector"))

cat_section("Saturation curve")
print(qc_saturation_curve(proj))

cat_section("Themes")
print(qc_list_themes(proj)[, c("id", "name", "n_codes", "n_categories")])

cat_section("Member checks")
print(qc_list_member_checks(proj))

cat_section("Annotations and excerpts")
print(qc_list_annotations(proj, timestamp_doc))
print(qc_list_excerpts(proj, timestamp_doc))

cat("\nDemo project created at:\n", project_path, "\n", sep = "")

# -- 8. Launch GUI -------------------------------------------------------------
devtools::load_all()
proj <- qc_open(project_path)
if (interactive()) {
  shiny_saturate(proj)
}


shiny_saturate()
