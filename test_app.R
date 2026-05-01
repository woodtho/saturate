# library(saturate)
devtools::load_all()
# ── 1. Fresh project ──────────────────────────────────────────────────────────

proj <- qc_new("test_study.duckdb",
               name  = "Firm Innovation Study",
               owner = "Researcher",
               overwrite = TRUE)
qc_close(proj)
proj <- qc_open("test_study.duckdb")


# ── 2. Documents ──────────────────────────────────────────────────────────────

qc_import_document(proj, name = "Small Tech Firm", content =
"We've been trying to scale our software platform for the past two years, but
hiring has been a persistent bottleneck. It's not just about finding
developers—it's finding people who understand both the technical side and the
regulatory requirements in our sector. We've had to turn down projects because
we couldn't staff them properly. At the same time, our R&D spending has
increased, especially on adapting our product for international markets, but
the return hasn't been immediate. There's also uncertainty around government
programs—some of the funding applications take months, and by the time we hear
back, the opportunity has often passed. Collaboration with universities has
helped, but it's inconsistent and depends heavily on individual relationships.")

qc_import_document(proj, name = "Manufacturing Firm", content =
"In manufacturing, innovation doesn't always look like new products. For us,
it's about improving processes and reducing waste. Over the last year, we
invested in automation equipment, which has improved efficiency, but the
upfront costs were significant. Energy prices have also been volatile, which
affects our margins directly. We've considered expanding into new markets, but
logistics and supply chain disruptions have made that risky. Hiring skilled
tradespeople is another challenge—we often end up training internally, which
takes time and resources. Government incentives are helpful, but the criteria
don't always align with what we're trying to do on the ground.")

qc_import_document(proj, name = "Life Sciences Company", content =
"Our business is heavily driven by R&D, and timelines are long. It can take
years before a product reaches the market, and even then, regulatory approval
is uncertain. We rely on a mix of private investment and public funding to
sustain operations during that period. Recently, we've seen increased
competition for talent, particularly in specialized research roles.
Collaboration is critical—we work closely with academic institutions and
hospitals—but coordinating across organizations can slow things down. On the
positive side, being part of a regional cluster has improved access to
expertise and infrastructure, which we wouldn't have on our own.")

qc_import_document(proj, name = "Service Firm", content =
"We accelerated our digital transformation during the pandemic, moving many of
our services online. While that improved accessibility for clients, it also
required retraining staff and investing in new systems. Some employees adapted
quickly, but others struggled, especially those who were less familiar with
digital tools. We've also had to rethink how we engage with clients—there's
less face-to-face interaction, which changes the nature of the service.
Cybersecurity has become a bigger concern as well, adding another layer of
cost and complexity. Overall, the shift has been positive, but it's still an
ongoing process.")

qc_import_document(proj, name = "Large Multinational", content =
"As a large organization operating in multiple countries, coordination is one
of our biggest challenges. Decisions made at headquarters don't always
translate well to local contexts, particularly when regulatory environments
differ. We invest significantly in R&D, but aligning those investments with
market needs requires constant adjustment. There's also internal competition
for resources between divisions, which can slow down decision-making. On the
other hand, our scale allows us to absorb risks that smaller firms cannot, and
we benefit from established relationships with governments and partners.
Recently, we've been focusing more on sustainability initiatives, which are
increasingly important to both regulators and customers.")

docs  <- qc_list_documents(proj)
doc_id <- function(name) docs$id[docs$name == name]

# ── 3. Codes ──────────────────────────────────────────────────────────────────

qc_add_code(proj, "talent_constraints",
  color      = "#E15759",
  definition = "Difficulties recruiting, retaining, or developing skilled workers.",
  criteria   = "Include: hiring bottlenecks, skills gaps, internal training burden. Exclude: general labour costs.")

qc_add_code(proj, "rd_investment",
  color      = "#4E79A7",
  definition = "Spending on research, development, or product/process innovation.",
  criteria   = "Include: R&D spend, automation capex, new product development. Exclude: routine maintenance.")

qc_add_code(proj, "regulatory_environment",
  color      = "#F28E2B",
  definition = "Regulatory requirements, approvals, or compliance burdens.",
  criteria   = "Include: sector regulations, approval processes, compliance costs. Exclude: internal policy.")

qc_add_code(proj, "government_programs",
  color      = "#76B7B2",
  definition = "Public funding, incentive programs, or government-supported initiatives.",
  criteria   = "Include: grants, tax incentives, funding applications. Exclude: general tax references.")

qc_add_code(proj, "collaboration",
  color      = "#59A14F",
  definition = "Partnerships with external organisations for shared knowledge or resource access.",
  criteria   = "Include: university links, cluster membership, inter-firm partnerships.")

qc_add_code(proj, "digital_transformation",
  color      = "#B07AA1",
  definition = "Adoption of digital technologies to change service delivery or operations.",
  criteria   = "Include: online services, automation, digital tools, cybersecurity. Exclude: routine IT.")

qc_add_code(proj, "supply_chain_disruption",
  color      = "#FF9DA7",
  definition = "Disruptions to logistics, materials, or external supply.",
  criteria   = "Include: supply delays, logistics risk, materials volatility.")

qc_add_code(proj, "scaling_challenges",
  color      = "#9C755F",
  definition = "Barriers to growing the firm — capacity, capital, or coordination.",
  criteria   = "Include: inability to take on projects, resource competition, coordination failure.")

codes  <- qc_list_codes(proj)
code_id <- function(name) codes$id[codes$name == name]

# ── 4. Categories ─────────────────────────────────────────────────────────────

qc_add_category(proj, "People & Skills")
qc_add_category(proj, "Innovation & Investment")
qc_add_category(proj, "External Environment")

cats <- .query(proj$con,
  "SELECT id, name FROM code_categories WHERE status = 1")
cat_id <- function(name) cats$id[cats$name == name]

qc_link_code_category(proj, code_id("talent_constraints"),    cat_id("People & Skills"))
qc_link_code_category(proj, code_id("digital_transformation"),cat_id("People & Skills"))
qc_link_code_category(proj, code_id("rd_investment"),         cat_id("Innovation & Investment"))
qc_link_code_category(proj, code_id("scaling_challenges"),    cat_id("Innovation & Investment"))
qc_link_code_category(proj, code_id("collaboration"),         cat_id("Innovation & Investment"))
qc_link_code_category(proj, code_id("regulatory_environment"),cat_id("External Environment"))
qc_link_code_category(proj, code_id("government_programs"),   cat_id("External Environment"))
qc_link_code_category(proj, code_id("supply_chain_disruption"),cat_id("External Environment"))

# ── 5. Programmatic codings (coder = "coder_a") ───────────────────────────────

# Small Tech Firm
d <- doc_id("Small Tech Firm")
qc_add_coding(proj, d, code_id("talent_constraints"),   27, 175, coder = "coder_a",
  memo = "Hiring bottleneck explicitly named as the main constraint on growth")
qc_add_coding(proj, d, code_id("rd_investment"),       266, 382, coder = "coder_a")
qc_add_coding(proj, d, code_id("government_programs"), 384, 498, coder = "coder_a",
  memo = "Long lag on funding decisions makes program timing useless")
qc_add_coding(proj, d, code_id("collaboration"),       499, 584, coder = "coder_a")
qc_add_coding(proj, d, code_id("scaling_challenges"),   27, 226, coder = "coder_a")

# Manufacturing Firm
d <- doc_id("Manufacturing Firm")
qc_add_coding(proj, d, code_id("rd_investment"),         59, 175, coder = "coder_a",
  memo = "Automation capex as process innovation — not product innovation")
qc_add_coding(proj, d, code_id("supply_chain_disruption"),245, 319, coder = "coder_a")
qc_add_coding(proj, d, code_id("talent_constraints"),    320, 410, coder = "coder_a")
qc_add_coding(proj, d, code_id("government_programs"),   411, 497, coder = "coder_a")

# Life Sciences
d <- doc_id("Life Sciences Company")
qc_add_coding(proj, d, code_id("rd_investment"),          1,  87, coder = "coder_a")
qc_add_coding(proj, d, code_id("regulatory_environment"), 88, 183, coder = "coder_a",
  memo = "Approval uncertainty named alongside long R&D timelines")
qc_add_coding(proj, d, code_id("talent_constraints"),    254, 342, coder = "coder_a")
qc_add_coding(proj, d, code_id("collaboration"),         343, 447, coder = "coder_a")

# Service Firm
d <- doc_id("Service Firm")
qc_add_coding(proj, d, code_id("digital_transformation"),  1, 119, coder = "coder_a")
qc_add_coding(proj, d, code_id("talent_constraints"),    120, 249, coder = "coder_a",
  memo = "Workforce adaptation framed as retraining burden, not hiring")
qc_add_coding(proj, d, code_id("scaling_challenges"),    390, 450, coder = "coder_a")

# Large Multinational
d <- doc_id("Large Multinational")
qc_add_coding(proj, d, code_id("scaling_challenges"),      1, 143, coder = "coder_a",
  memo = "Coordination failure as a scaling problem specific to multinationals")
qc_add_coding(proj, d, code_id("regulatory_environment"), 144, 246, coder = "coder_a")
qc_add_coding(proj, d, code_id("rd_investment"),          247, 338, coder = "coder_a")
qc_add_coding(proj, d, code_id("collaboration"),          450, 534, coder = "coder_a")

# ── 6. Cases with attributes ──────────────────────────────────────────────────

qc_add_case(proj, "Small Tech Firm")
qc_add_case(proj, "Manufacturing Firm")
qc_add_case(proj, "Life Sciences Company")
qc_add_case(proj, "Service Firm")
qc_add_case(proj, "Large Multinational")

cases <- qc_list_cases(proj)
case_id <- function(name) cases$id[cases$name == name]

for (nm in cases$name) {
  qc_link_case_source(proj, case_id(nm), doc_id(nm))
}

attrs <- list(
  "Small Tech Firm"       = list(sector = "technology",    size = "small",  rd_intensity = "high"),
  "Manufacturing Firm"    = list(sector = "manufacturing", size = "medium", rd_intensity = "medium"),
  "Life Sciences Company" = list(sector = "life_sciences", size = "small",  rd_intensity = "high"),
  "Service Firm"          = list(sector = "services",      size = "medium", rd_intensity = "low"),
  "Large Multinational"   = list(sector = "technology",    size = "large",  rd_intensity = "high")
)
for (nm in names(attrs)) {
  for (var in names(attrs[[nm]])) {
    qc_set_case_attribute(proj, case_id(nm), var, attrs[[nm]][[var]])
  }
}

# ── 7. Quick sanity checks ────────────────────────────────────────────────────

cat("\n--- Documents ---\n"); print(qc_list_documents(proj))
cat("\n--- Codes ---\n");     print(qc_list_codes(proj)[, c("name","n_codings","categories")])
cat("\n--- Segments (rd_investment) ---\n")
print(qc_get_coded_segments(proj,
  code_ids = code_id("rd_investment"))[, c("source_name","seltext")])
cat("\n--- Cross-tab: code x sector ---\n")
print(qc_cross_tabulate(proj, attribute = "sector"))
cat("\n--- Summary ---\n"); print(qc_code_summary(proj))

# ── 8. Launch GUI ─────────────────────────────────────────────────────────────

proj <- qc_open("test_study.duckdb")
proj <- qc_open("C:/Users/aThom/Downloads/Firm_Innovation_Study_split_2026-04-23.duckdb")
devtools::load_all()
shiny_saturate(proj)
shiny_saturate()
