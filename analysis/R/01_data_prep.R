# ---------------------------------------------------------------------------
# 01_data_prep.R
# Eligibility, derivation of analysis variables and descriptive tables.
# ---------------------------------------------------------------------------

source("analysis/R/00_setup.R")

read_out <- function(file) {
  readr::read_csv(file.path(root, "output", file), show_col_types = FALSE, progress = FALSE)
}

studies   <- read_out("studies.csv")
incidence <- read_out("incidence.csv")
amr       <- read_out("amr.csv")
quality   <- read_out("quality.csv")
screening <- read_out("screening.csv")
outcomes  <- read_out("outcomes.csv")
drivers   <- read_out("drivers.csv")

# Normalise logical columns written by the Python build
as_yes <- function(x) tolower(trimws(as.character(x))) %in% c("yes", "true", "1")
studies <- studies %>%
  mutate(
    has_incidence = as_yes(has_incidence),
    has_amr = as_yes(has_amr),
    incidence_component = as_yes(incidence_component),
    amr_component = as_yes(amr_component)
  )
incidence <- incidence %>%
  mutate(
    include_primary = as_yes(include_primary),
    n_ssi_events = as.numeric(n_ssi_events),
    n_total = as.numeric(n_total),
    pct_calculated = as.numeric(pct_calculated)
  )
amr <- amr %>%
  mutate(
    include_primary = as_yes(include_primary),
    n_resistant = as.numeric(n_resistant),
    n_tested = as.numeric(n_tested),
    pct_calculated = as.numeric(pct_calculated)
  )

# ---------------------------------------------------------------------------
# Study-level groupings used in the protocol subgroup analyses
# ---------------------------------------------------------------------------
specialty_group <- function(x) {
  x <- tolower(x)
  dplyr::case_when(
    grepl("mixed|internal medicine|general, obstetric|obstetrics/gynaecology and general|general surgery and orthopaed", x) ~ "Mixed/other",
    grepl("orthopaed|orthoped|amputation", x) & !grepl("general surgery", x) ~ "Orthopaedic",
    grepl("obstetric|caesarean|gynaec|gynec", x) ~ "Obstetrics/gynaecology",
    grepl("general surgery|general surgical|laparotomy|abdominal surgery", x) ~ "General surgery",
    TRUE ~ "Mixed/other"
  )
}

hospital_group <- function(x) {
  x <- tolower(x)
  dplyr::case_when(
    grepl("tertiary|referral", x) ~ "Tertiary/referral",
    grepl("secondary|district|primary", x) ~ "Secondary/primary",
    TRUE ~ "Mixed"
  )
}

design_group <- function(x) {
  x <- tolower(x)
  dplyr::case_when(
    grepl("randomi[sz]ed|trial", x) ~ "RCT",
    grepl("before-after|pre-post|stepped", x) ~ "Before-after/QI",
    grepl("cohort|longitudinal", x) ~ "Cohort",
    grepl("cross-sectional|chart review", x) ~ "Cross-sectional",
    TRUE ~ "Other"
  )
}

period_group <- function(y) ifelse(y <= 2019, "2013-2019", "2020-2026")

study_level <- studies %>%
  mutate(
    specialty_group = specialty_group(specialty),
    hospital_group  = hospital_group(hospital_level),
    design_group    = design_group(design),
    period_group    = period_group(publication_year)
  ) %>%
  left_join(
    quality %>% select(study_id, quality_overall = overall, quality_rationale = rationale),
    by = "study_id"
  )

# ---------------------------------------------------------------------------
# Primary SSI incidence analysis set
# ---------------------------------------------------------------------------
covariates <- readr::read_csv(
  file.path(data_dir, "ssi_positive_characteristics.csv"),
  show_col_types = FALSE, progress = FALSE
) %>%
  mutate(across(-c(study_id, notes), as.numeric))

primary <- incidence %>%
  filter(include_primary) %>%
  select(
    study_id, cohort_label, events = n_ssi_events, n = n_total,
    pct_calculated, percent_check, estimate_type, numerator_definition,
    denominator_definition, derived_flag, notes_incidence = notes
  ) %>%
  left_join(
    study_level %>%
      select(study_id, first_author, publication_year, country, subregion,
             hospital_level, hospital_group, design, design_group,
             specialty, specialty_group, period_group, quality_overall),
    by = "study_id"
  ) %>%
  left_join(covariates %>% rename(notes_covariate = notes), by = "study_id") %>%
  mutate(
    pct = events / n,
    male_pct = ifelse(!is.na(male_ssi) & !is.na(female_ssi), male_ssi / (male_ssi + female_ssi), NA_real_),
    hiv_pct = ifelse(!is.na(hiv_positive) & !is.na(hiv_known), hiv_positive / hiv_known, NA_real_),
    preop_pct = ifelse(!is.na(preop_prophylaxis_yes) & !is.na(preop_prophylaxis_known),
                       preop_prophylaxis_yes / preop_prophylaxis_known, NA_real_),
    contaminated_pct = ifelse(!is.na(wound_contaminated) & !is.na(wound_dirty) & !is.na(wound_class_known),
                              (wound_contaminated + wound_dirty) / wound_class_known, NA_real_),
    included_in_primary = TRUE
  ) %>%
  arrange(publication_year, first_author)

# Guard against accidental duplicate cohorts
if (anyDuplicated(primary$study_id)) stop("Duplicate study_id in primary incidence set")

# ---------------------------------------------------------------------------
# AMR priority analysis set
# ---------------------------------------------------------------------------
amr_priority <- amr %>%
  filter(include_primary) %>%
  select(
    study_id, pathogen, pathogen_group, resistance_outcome, resistance_marker,
    n_resistant, n_tested, pct_calculated, denominator_source, derived_flag,
    notes, priority_outcome, include_primary
  ) %>%
  mutate(
    outcome_group = case_when(
      resistance_outcome == "MRSA" ~ "MRSA",
      resistance_outcome %in% c("3GC resistance", "ESBL", "3GC non-susceptibility") ~ "3GC/ESBL resistance",
      resistance_outcome == "Carbapenem resistance" ~ "Carbapenem resistance",
      TRUE ~ "Other"
    ),
    pathogen_group2 = case_when(
      grepl("^Escherichia", pathogen) ~ "E. coli",
      grepl("^Klebsiella", pathogen) ~ "Klebsiella spp.",
      grepl("Acinetobacter", pathogen) ~ "A. baumannii",
      grepl("Pseudomonas", pathogen) ~ "P. aeruginosa",
      grepl("Enterobacter", pathogen) ~ "Enterobacterales (aggregate)",
      grepl("Gram-negative", pathogen) ~ "Gram-negative (aggregate)",
      TRUE ~ pathogen
    ),
    pct = n_resistant / n_tested
  ) %>%
  left_join(
    study_level %>% select(study_id, first_author, publication_year, country, subregion,
                           hospital_level, hospital_group, design_group, specialty_group),
    by = "study_id"
  )

amr_only <- study_level %>%
  filter(has_amr & !has_incidence) %>%
  arrange(publication_year, first_author)

# ---------------------------------------------------------------------------
# Descriptive tables
# ---------------------------------------------------------------------------
table1 <- primary %>%
  transmute(
    Study = paste0(first_author, " ", publication_year),
    Country = country,
    Subregion = subregion,
    `Hospital level` = hospital_level,
    Design = design,
    Specialty = specialty_group,
    `Cohort n` = n,
    `SSI events` = events,
    `SSI %` = round(100 * pct, 1),
    `SSI definition` = numerator_definition,
    `AMR data` = ifelse(study_id %in% amr$study_id, "Yes", "No"),
    Quality = quality_overall,
    `Derived numerator` = ifelse(tolower(derived_flag) == "derived", "Yes", "No")
  )

table2 <- amr_only %>%
  transmute(
    Study = paste0(first_author, " ", publication_year),
    Country = country,
    Subregion = subregion,
    `Hospital level` = hospital_level,
    Design = design,
    Specialty = specialty_group,
    `SSI patients/isolates` = population,
    `AMR rows` = n_amr_rows,
    Quality = quality_overall
  )

tableS1 <- study_level %>%
  transmute(
    Study = paste0(first_author, " ", publication_year),
    Country = country,
    Subregion = subregion,
    `Hospital level` = hospital_level,
    Design = design,
    Specialty = specialty,
    `Cohort n` = cohort_n,
    Incidence = ifelse(has_incidence, "Yes", "No"),
    AMR = ifelse(has_amr, "Yes", "No"),
    `Primary incidence rows` = n_primary_incidence_rows,
    `AMR rows` = n_amr_rows,
    Quality = quality_overall
  )

tableS2 <- screening %>%
  filter(tolower(decision) == "excluded") %>%
  transmute(
    Document = label,
    `Document ID` = doc_id,
    Reason = reason,
    `Linked study` = linked_study_id,
    Notes = notes
  )

# ---------------------------------------------------------------------------
# Counts used in the PRISMA flow and results text
# ---------------------------------------------------------------------------
flow_counts <- list(
  documents = nrow(screening),
  excluded = sum(tolower(screening$decision) == "excluded"),
  included = sum(tolower(screening$decision) == "included"),
  incidence_component = sum(studies$incidence_component),
  primary_incidence = nrow(primary),
  amr_component = sum(studies$amr_component),
  amr_only = nrow(amr_only),
  amr_priority_studies = length(unique(amr_priority$study_id)),
  amr_priority_rows = nrow(amr_priority)
)

# ---------------------------------------------------------------------------
# Write derived datasets and tables
# ---------------------------------------------------------------------------
write_csv(primary, file.path(data_dir, "analysis_incidence.csv"))
write_csv(amr_priority, file.path(data_dir, "analysis_amr.csv"))
write_csv(study_level, file.path(data_dir, "analysis_studies.csv"))
write_csv(table1, file.path(tab_dir, "table1_primary_incidence_studies.csv"))
write_csv(table2, file.path(tab_dir, "table2_amr_only_studies.csv"))
write_csv(tableS1, file.path(tab_dir, "tableS1_all_included_studies.csv"))
write_csv(tableS2, file.path(tab_dir, "tableS2_excluded_documents.csv"))
saveRDS(flow_counts, file.path(data_dir, "flow_counts.rds"))

# Validation log
sink(file.path(log_dir, "01_data_prep.log"))
cat("ECSA SSI/AMR data preparation\n")
cat("=============================\n\n")
cat("Documents screened:", flow_counts$documents, "\n")
cat("Excluded documents:", flow_counts$excluded, "\n")
cat("Included unique cohorts:", flow_counts$included, "\n")
cat("Studies with incidence component:", flow_counts$incidence_component, "\n")
cat("Primary incidence cohorts:", flow_counts$primary_incidence, "\n")
cat("Studies with AMR component:", flow_counts$amr_component, "\n")
cat("AMR-only studies:", flow_counts$amr_only, "\n")
cat("Primary AMR rows:", flow_counts$amr_priority_rows, "from", flow_counts$amr_priority_studies, "studies\n\n")
cat("Primary incidence rows and checked percentages:\n")
print(primary %>% select(study_id, events, n, pct, percent_check, derived_flag, quality_overall))
cat("\nMissingness in SSI-positive covariates:\n")
print(primary %>%
        summarise(
          male_pct = sum(!is.na(male_pct)),
          hiv_pct = sum(!is.na(hiv_pct)),
          preop_pct = sum(!is.na(preop_pct)),
          contaminated_pct = sum(!is.na(contaminated_pct))
        ))
cat("\nCovariate denominator checks (n_ssi should equal events):\n")
print(primary %>% filter(!is.na(n_ssi) & n_ssi != events) %>% select(study_id, events, n_ssi))
sink()

cat("Data preparation complete:",
    flow_counts$primary_incidence, "primary cohorts,",
    flow_counts$amr_priority_rows, "priority AMR rows.\n")
