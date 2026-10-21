# ---------------------------------------------------------------------------
# 03_amr.R
# Priority AMR outcomes and antimicrobial susceptibility patterns.
#
# AMR estimates are deliberately NOT meta-analysed. They are summarised as
# study-level proportions (median, range, IQR) and crude aggregate proportions
# because the underlying populations, isolate denominators, breakpoints and
# resistance definitions are too heterogeneous for a meaningful pooled effect.
# ---------------------------------------------------------------------------

source("analysis/R/00_setup.R")

amr <- readr::read_csv(
  file.path(data_dir, "analysis_amr.csv"),
  show_col_types = FALSE, progress = FALSE
)
studies <- readr::read_csv(
  file.path(data_dir, "analysis_studies.csv"),
  show_col_types = FALSE, progress = FALSE
)
ast <- readr::read_csv(
  file.path(data_dir, "ast_panel.csv"),
  show_col_types = FALSE, progress = FALSE
)

outcome_levels <- c("MRSA", "3GC/ESBL resistance", "Carbapenem resistance")
pathogen_levels <- c(
  "Staphylococcus aureus", "Escherichia coli", "Klebsiella pneumoniae",
  "Acinetobacter baumannii", "Pseudomonas aeruginosa"
)

amr <- amr %>%
  mutate(
    outcome_group = factor(outcome_group, levels = outcome_levels),
    resistance_outcome = as.character(resistance_outcome)
  )

# ---------------------------------------------------------------------------
# One priority estimate per study / pathogen / outcome
# ---------------------------------------------------------------------------
# A few studies report both a direct 3GC result and an ESBL phenotype for the
# same isolates. Prefer the direct 3GC result, then the non-susceptibility
# result, then ESBL, so the same isolates are not counted twice.
amr_dedup <- amr %>%
  mutate(
    method_priority = dplyr::case_when(
      resistance_outcome == "3GC resistance" ~ 1,
      resistance_outcome == "3GC non-susceptibility" ~ 2,
      resistance_outcome == "ESBL" ~ 3,
      TRUE ~ 1
    )
  ) %>%
  group_by(study_id, pathogen_group2, outcome_group) %>%
  arrange(method_priority, dplyr::desc(n_tested)) %>%
  slice(1) %>%
  ungroup()

dup_check <- amr_dedup %>%
  count(study_id, pathogen_group2, outcome_group) %>%
  filter(n > 1)
if (nrow(dup_check) > 0) stop("Duplicate study/pathogen/outcome rows remain")

# ---------------------------------------------------------------------------
# One estimate per study / outcome for regional summaries
# ---------------------------------------------------------------------------
# If a study reports an aggregate Enterobacterales or Gram-negative row, use
# that row alone. Otherwise sum the species-level rows (different isolates).
amr_study <- amr_dedup %>%
  group_by(study_id, outcome_group) %>%
  mutate(
    aggregate = pathogen_group2 %in% c("Enterobacterales (aggregate)",
                                        "Gram-negative (aggregate)"),
    aggregate_n = ifelse(aggregate, n_tested, -Inf)
  ) %>%
  filter(if (any(aggregate)) aggregate_n == max(aggregate_n) else TRUE) %>%
  summarise(
    n_resistant = sum(n_resistant),
    n_tested = sum(n_tested),
    pathogens = paste(sort(unique(pathogen_group2)), collapse = "; "),
    methods = paste(sort(unique(resistance_outcome)), collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(pct = n_resistant / n_tested) %>%
  left_join(
    studies %>%
      select(study_id, first_author, publication_year, country, subregion,
             hospital_level, hospital_group),
    by = "study_id"
  ) %>%
  mutate(
    study_label = paste0(first_author, " ", publication_year),
    subregion = factor(subregion, levels = c("East Africa", "Central Africa", "Southern Africa")),
    hospital_group = factor(hospital_group,
                            levels = c("Tertiary/referral", "Secondary/primary", "Mixed"))
  )

if (any(amr_study$n_resistant > amr_study$n_tested)) stop("Resistant exceeds tested")
if (any(amr_study$pct < 0 | amr_study$pct > 1)) stop("AMR proportion outside 0-1")

# ---------------------------------------------------------------------------
# Descriptive summaries
# ---------------------------------------------------------------------------
summarise_amr <- function(data, group_vars = character()) {
  data %>%
    group_by(dplyr::across(dplyr::all_of(c("outcome_group", group_vars)))) %>%
    summarise(
      Studies = dplyr::n_distinct(study_id),
      Resistant = sum(n_resistant),
      Tested = sum(n_tested),
      Crude = Resistant / Tested,
      Median = median(pct),
      Min = min(pct),
      Max = max(pct),
      Q1 = unname(quantile(pct, 0.25)),
      Q3 = unname(quantile(pct, 0.75)),
      .groups = "drop"
    )
}

amr_overall    <- summarise_amr(amr_study)
amr_by_region  <- summarise_amr(amr_study, "subregion")
amr_by_hosp    <- summarise_amr(amr_study, "hospital_group")
amr_by_reg_hosp <- summarise_amr(amr_study, c("subregion", "hospital_group"))
amr_by_pathogen <- summarise_amr(amr_dedup %>% mutate(pct = n_resistant / n_tested),
                                 "pathogen_group2")

format_summary <- function(x, group_name = NULL) {
  out <- x %>%
    transmute(
      Outcome = as.character(outcome_group),
      Studies,
      `Isolates tested` = Tested,
      `Resistant isolates` = Resistant,
      `Crude aggregate %` = round(100 * Crude, 1),
      `Median study %` = round(100 * Median, 1),
      `Range %` = sprintf("%.1f-%.1f", 100 * Min, 100 * Max),
      `IQR %` = sprintf("%.1f-%.1f", 100 * Q1, 100 * Q3)
    )
  if (!is.null(group_name)) {
    out <- x %>%
      transmute(
        Outcome = as.character(outcome_group),
        Group = as.character(.data[[group_name]]),
        Studies,
        `Isolates tested` = Tested,
        `Resistant isolates` = Resistant,
        `Crude aggregate %` = round(100 * Crude, 1),
        `Median study %` = round(100 * Median, 1),
        `Range %` = sprintf("%.1f-%.1f", 100 * Min, 100 * Max),
        `IQR %` = sprintf("%.1f-%.1f", 100 * Q1, 100 * Q3)
      )
  }
  out
}

table6  <- format_summary(amr_overall)
table7  <- format_summary(amr_by_region, "subregion") %>% rename(Region = Group)
table8  <- format_summary(amr_by_hosp, "hospital_group") %>% rename(`Hospital level` = Group)
table9  <- format_summary(amr_by_pathogen, "pathogen_group2") %>% rename(Pathogen = Group)
tableS8 <- amr_by_reg_hosp %>%
  transmute(
    Outcome = as.character(outcome_group),
    Region = as.character(subregion),
    `Hospital level` = as.character(hospital_group),
    Studies,
    `Isolates tested` = Tested,
    `Resistant isolates` = Resistant,
    `Crude aggregate %` = round(100 * Crude, 1),
    `Median study %` = round(100 * Median, 1),
    `Range %` = sprintf("%.1f-%.1f", 100 * Min, 100 * Max),
    `IQR %` = sprintf("%.1f-%.1f", 100 * Q1, 100 * Q3)
  ) %>%
  arrange(Outcome, Region, `Hospital level`)

tableS9_study <- amr_study %>%
  arrange(outcome_group, subregion, study_label) %>%
  transmute(
    Study = study_label,
    Year = publication_year,
    Country = country,
    Region = as.character(subregion),
    `Hospital level` = as.character(hospital_group),
    Outcome = as.character(outcome_group),
    Pathogens = pathogens,
    `Resistance method` = methods,
    Resistant = n_resistant,
    Tested = n_tested,
    `Resistance %` = round(100 * pct, 1)
  )

# ---------------------------------------------------------------------------
# AST harmonisation
# ---------------------------------------------------------------------------
# Combine equivalent agents into drug classes for display. Within a study,
# repeated agents in the same class are averaged so one study contributes once.
ast_std <- ast %>%
  mutate(
    pathogen = factor(pathogen, levels = pathogen_levels),
    drug = dplyr::case_when(
      pathogen == "Staphylococcus aureus" & antibiotic %in% c("Oxacillin", "Cefoxitin") ~
        "Oxacillin/cefoxitin (MRSA)",
      antibiotic %in% c("Cefotaxime", "Ceftriaxone", "Cefotaxime/cephalexin") ~
        "Cefotaxime/ceftriaxone (3GC)",
      antibiotic %in% c("Meropenem", "Imipenem") ~ "Meropenem/imipenem",
      antibiotic %in% c("Erythromycin/azithromycin", "erythromycin(%)") ~
        "Erythromycin/azithromycin",
      antibiotic == "amoxiclav(%)" ~ "Amoxicillin-clavulanate",
      TRUE ~ antibiotic
    )
  ) %>%
  group_by(study_id, pathogen, drug) %>%
  summarise(resistance_pct = mean(resistance_pct), .groups = "drop")

ast_summary <- ast_std %>%
  group_by(pathogen, drug) %>%
  summarise(
    Studies = n_distinct(study_id),
    Median = median(resistance_pct),
    Min = min(resistance_pct),
    Max = max(resistance_pct),
    Q1 = unname(quantile(resistance_pct, 0.25)),
    Q3 = unname(quantile(resistance_pct, 0.75)),
    .groups = "drop"
  )

tableS10_ast <- ast_summary %>%
  transmute(
    Pathogen = as.character(pathogen),
    Drug = drug,
    Studies,
    `Median resistance %` = round(Median, 1),
    `Range %` = sprintf("%.1f-%.1f", Min, Max),
    `IQR %` = sprintf("%.1f-%.1f", Q1, Q3)
  ) %>%
  arrange(Pathogen, desc(`Median resistance %`))

# ---------------------------------------------------------------------------
# Figures
# ---------------------------------------------------------------------------
# Figure 4: study-level priority AMR estimates
amr_medians <- amr_study %>%
  group_by(outcome_group) %>%
  summarise(Median = median(pct), .groups = "drop")

p_amr <- ggplot(amr_study, aes(x = outcome_group, y = 100 * pct)) +
  geom_jitter(aes(colour = subregion, shape = hospital_group),
              width = 0.13, height = 0, size = 2.7, alpha = 0.85) +
  geom_segment(
    data = amr_medians,
    aes(x = as.numeric(outcome_group) - 0.30,
        xend = as.numeric(outcome_group) + 0.30,
        y = 100 * Median, yend = 100 * Median),
    inherit.aes = FALSE, linewidth = 0.9
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_colour_manual(values = c("East Africa" = "#1b9e77",
                                 "Central Africa" = "#d95f02",
                                 "Southern Africa" = "#7570b3"),
                      drop = TRUE) +
  scale_shape_manual(values = c("Tertiary/referral" = 16,
                                "Secondary/primary" = 17,
                                "Mixed" = 15),
                     drop = TRUE) +
  labs(
    x = NULL,
    y = "Resistance among isolates (%)",
    colour = "ECSA sub-region",
    shape = "Hospital level",
    subtitle = "Each point is a study-level estimate; horizontal bars are medians. No meta-analysis was performed."
  ) +
  guides(
    colour = guide_legend(nrow = 1, title.position = "top"),
    shape = guide_legend(nrow = 1, title.position = "top")
  ) +
  theme_ecsa() +
  theme(legend.box = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 9))

save_png(p_amr, "fig4_amr_priority_dotplot.png", width = 10.5, height = 6.5)

# Figure 5: study x outcome heatmap
study_order <- amr_study %>%
  group_by(study_label, subregion) %>%
  summarise(mean_pct = mean(pct), .groups = "drop") %>%
  arrange(subregion, mean_pct) %>%
  pull(study_label)

heat_df <- amr_study %>%
  mutate(
    study_label = factor(study_label, levels = study_order),
    outcome_short = factor(
      dplyr::case_when(
        outcome_group == "MRSA" ~ "MRSA",
        outcome_group == "3GC/ESBL resistance" ~ "3GC/ESBL",
        TRUE ~ "Carbapenem"
      ),
      levels = c("MRSA", "3GC/ESBL", "Carbapenem")
    )
  )

p_heat <- ggplot(heat_df, aes(x = outcome_short, y = study_label, fill = 100 * pct)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.0f", 100 * pct)), size = 2.5, colour = "black") +
  facet_grid(subregion ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#fee391", "#f16913", "#7f0000"),
    limits = c(0, 100), na.value = "grey95", name = "Resistance (%)"
  ) +
  labs(x = NULL, y = NULL,
       subtitle = "Each cell is one study-level estimate.\nBlank cells indicate no priority AMR row.") +
  theme_ecsa() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        plot.margin = margin(5.5, 8, 5.5, 45))

save_png(p_heat, "fig5_amr_study_outcome_heatmap.png", width = 8.5, height = 9)

# ---------------------------------------------------------------------------
# AST panels
# ---------------------------------------------------------------------------
gn_drugs <- c(
  "Ampicillin", "Amoxicillin-clavulanate", "Cefotaxime/ceftriaxone (3GC)",
  "Ceftazidime", "Cefepime", "Piperacillin-tazobactam", "Meropenem/imipenem",
  "Gentamicin", "Amikacin", "Ciprofloxacin", "Levofloxacin",
  "Trimethoprim-sulfamethoxazole", "Tetracycline", "Chloramphenicol",
  "Colistin", "Tigecycline"
)
gp_drugs <- c(
  "Penicillin", "Oxacillin/cefoxitin (MRSA)", "Erythromycin/azithromycin",
  "Clindamycin", "Gentamicin", "Ciprofloxacin", "Levofloxacin",
  "Trimethoprim-sulfamethoxazole", "Tetracycline", "Doxycycline",
  "Chloramphenicol", "Vancomycin"
)

# Figure 6: Gram-negative heatmap
gn_df <- ast_summary %>%
  filter(pathogen != "Staphylococcus aureus", drug %in% gn_drugs) %>%
  mutate(
    pathogen = factor(pathogen,
                      levels = c("Escherichia coli", "Klebsiella pneumoniae",
                                 "Pseudomonas aeruginosa", "Acinetobacter baumannii")),
    drug = factor(drug, levels = rev(gn_drugs)),
    label = sprintf("%.0f\n(n=%d)", Median, Studies)
  )

p_gn <- ggplot(gn_df, aes(x = pathogen, y = drug, fill = Median)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = label), size = 2.0, lineheight = 0.85) +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#fee391", "#f16913", "#7f0000"),
    limits = c(0, 100), na.value = "grey95", name = "Median resistance (%)"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Gram-negative antimicrobial resistance patterns",
    subtitle = "Median of study-level resistance percentages; n = number of studies contributing."
  ) +
  theme_ecsa() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))

save_png(p_gn, "fig6_ast_gram_negative_heatmap.png", width = 10, height = 8)

# Figure 7: S. aureus median resistance with study-level points
sa_df <- ast_std %>%
  filter(pathogen == "Staphylococcus aureus", drug %in% gp_drugs)
sa_summary <- ast_summary %>%
  filter(pathogen == "Staphylococcus aureus", drug %in% gp_drugs) %>%
  mutate(drug = factor(drug, levels = rev(gp_drugs)))

p_sa <- ggplot(sa_summary, aes(x = Median, y = drug)) +
  geom_col(fill = "grey85", width = 0.55) +
  geom_point(
    data = sa_df,
    aes(x = resistance_pct, y = drug),
    position = position_jitter(height = 0.09, width = 0, seed = 2026),
    size = 1.9, colour = "steelblue", alpha = 0.70, inherit.aes = FALSE
  ) +
  geom_text(aes(label = sprintf("%.0f%% (n=%d)", Median, Studies)),
            x = 103, hjust = 0, size = 2.7) +
  scale_x_continuous(limits = c(0, 145), breaks = seq(0, 100, 20)) +
  labs(
    x = "Resistance among S. aureus isolates (%)",
    y = NULL,
    title = "Staphylococcus aureus resistance patterns",
    subtitle = "Bars are medians; points are individual study estimates."
  ) +
  theme_ecsa() +
  theme(panel.grid.major.y = element_blank())

save_png(p_sa, "fig7_ast_saureus.png", width = 9, height = 6.5)

# Figure S3: pathogen-specific priority AMR estimates
p_path <- amr_dedup %>%
  mutate(pct = n_resistant / n_tested) %>%
  filter(outcome_group != "MRSA") %>%
  ggplot(aes(x = pathogen_group2, y = 100 * pct, colour = outcome_group)) +
  geom_jitter(width = 0.12, height = 0, size = 2.2, alpha = 0.8) +
  stat_summary(fun = median, geom = "errorbar",
               aes(ymin = after_stat(y), ymax = after_stat(y)),
               width = 0.35, linewidth = 0.7, colour = "black") +
  facet_wrap(~ outcome_group, scales = "free_x", ncol = 1) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = NULL, y = "Resistance among isolates (%)",
       subtitle = "Each point is a study-level estimate; bars are medians.") +
  theme_ecsa() +
  theme(legend.position = "none", axis.text.y = element_text(size = 8))

save_png(p_path, "figS3_amr_pathogen_specific.png", width = 9, height = 8)

# ---------------------------------------------------------------------------
# Write tables
# ---------------------------------------------------------------------------
write_csv(table6,  file.path(tab_dir, "table6_amr_overall.csv"))
write_csv(table7,  file.path(tab_dir, "table7_amr_by_region.csv"))
write_csv(table8,  file.path(tab_dir, "table8_amr_by_hospital.csv"))
write_csv(table9,  file.path(tab_dir, "table9_amr_by_pathogen.csv"))
write_csv(tableS8, file.path(tab_dir, "tableS8_amr_region_hospital.csv"))
write_csv(tableS9_study, file.path(tab_dir, "tableS9_amr_study_estimates.csv"))
write_csv(tableS10_ast,  file.path(tab_dir, "tableS10_ast_summary.csv"))

# ---------------------------------------------------------------------------
# Save results for the R Markdown report
# ---------------------------------------------------------------------------
results <- list(
  overall = amr_overall,
  by_region = amr_by_region,
  by_hospital = amr_by_hosp,
  by_region_hospital = amr_by_reg_hosp,
  by_pathogen = amr_by_pathogen,
  study_level = amr_study,
  ast_summary = ast_summary,
  n_priority_rows = nrow(amr),
  n_dedup_rows = nrow(amr_dedup),
  n_study_outcome_rows = nrow(amr_study),
  n_studies = dplyr::n_distinct(amr_study$study_id),
  n_ast_rows = nrow(ast),
  n_ast_studies = dplyr::n_distinct(ast$study_id)
)
saveRDS(results, file.path(data_dir, "amr_results.rds"))

# ---------------------------------------------------------------------------
# Validation log
# ---------------------------------------------------------------------------
sink(file.path(log_dir, "03_amr.log"))
cat("ECSA priority AMR descriptive synthesis\n")
cat("=======================================\n\n")
cat("Priority AMR rows:", nrow(amr), "\n")
cat("Rows after within-pathogen de-duplication:", nrow(amr_dedup), "\n")
cat("Study-level outcome rows:", nrow(amr_study), "from",
    dplyr::n_distinct(amr_study$study_id), "studies\n")
cat("AST rows:", nrow(ast), "from", dplyr::n_distinct(ast$study_id), "studies\n\n")
cat("Overall descriptive summary\n---------------------------\n")
print(as.data.frame(table6), row.names = FALSE)
cat("\nBy ECSA sub-region\n------------------\n")
print(as.data.frame(table7), row.names = FALSE)
cat("\nBy hospital level\n-----------------\n")
print(as.data.frame(table8), row.names = FALSE)
cat("\nBy pathogen\n-----------\n")
print(as.data.frame(table9), row.names = FALSE)
cat("\nAST data completeness\n---------------------\n")
print(as.data.frame(tableS10_ast), row.names = FALSE)
cat("\nChecks\n------\n")
cat("Southern Africa priority AMR rows:",
    sum(amr_study$subregion == "Southern Africa"), "(expected 0 in the extracted priority set)\n")
cat("Studies contributing >1 priority outcome:",
    sum(table(amr_study$study_id) > 1), "\n")
sink()

cat("AMR synthesis complete:", nrow(amr_study), "study-outcome rows from",
    dplyr::n_distinct(amr_study$study_id), "studies.\n")
