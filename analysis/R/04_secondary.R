# ---------------------------------------------------------------------------
# 04_secondary.R
# Secondary clinical and economic outcomes, and descriptive/thematic synthesis
# of reported drivers of SSI/AMR. These data are summarised narratively; no
# pooling is attempted because definitions, comparators and effect measures
# are not comparable.
# ---------------------------------------------------------------------------

source("analysis/R/00_setup.R")

outcomes <- readr::read_csv(
  file.path(root, "output", "outcomes.csv"),
  show_col_types = FALSE, progress = FALSE
)
drivers <- readr::read_csv(
  file.path(root, "output", "drivers.csv"),
  show_col_types = FALSE, progress = FALSE
)
studies <- readr::read_csv(
  file.path(data_dir, "analysis_studies.csv"),
  show_col_types = FALSE, progress = FALSE
)

study_meta <- studies %>%
  select(study_id, first_author, publication_year, country, subregion, hospital_level)

outcomes <- outcomes %>%
  left_join(study_meta, by = "study_id") %>%
  mutate(
    study_label = paste0(first_author, " ", publication_year),
    p_value = as.character(p_value)
  )
drivers <- drivers %>%
  left_join(study_meta, by = "study_id") %>%
  mutate(
    study_label = paste0(first_author, " ", publication_year),
    p_value = as.character(p_value)
  )

# Worku 2023 reports two descriptive proportions whose stated percentages do
# not match the stated denominator (388/752 = 51.6%, not 52.9%; 619/752 =
# 82.3%, not 82.4%). Apply the pre-specified count-first rule so that the
# manuscript and Table 11 use the arithmetic values.
drivers <- drivers %>%
  mutate(
    effect_value = dplyr::case_when(
      study_id == "worku2023" & driver == "Previous use of antibiotics" ~ 100 * 388 / 752,
      study_id == "worku2023" & driver == "History of hospital admission" ~ 100 * 619 / 752,
      TRUE ~ effect_value
    ),
    notes = dplyr::case_when(
      study_id == "worku2023" & driver == "Previous use of antibiotics" ~
        paste0(notes, " Count-based percentage 51.6%; source reports 52.9%."),
      study_id == "worku2023" & driver == "History of hospital admission" ~
        paste0(notes, " Count-based percentage 82.3%; source reports 82.4%."),
      TRUE ~ notes
    )
  )

if (any(is.na(outcomes$study_label))) stop("Outcome row without study metadata")
if (any(is.na(drivers$study_label))) stop("Driver row without study metadata")

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------
ratio_measures <- c("AOR", "OR", "ARR", "cHR", "RR", "COR")

fmt_effect <- function(measure, value, lo, hi, domain = NA_character_) {
  if (is.na(value)) return(NA_character_)
  if (!is.na(domain) && domain == "Economic") {
    return("Approximately EUR 1500 estimated hospital saving (author estimate)")
  }
  if (measure %in% c("Prevalence", "Proportion")) {
    return(sprintf("%.1f%%", value))
  }
  if (measure %in% c("Significance")) return("Reported as statistically significant")
  if (!is.na(lo) && !is.na(hi)) {
    return(sprintf("%s %.2f (95%% CI %.2f-%.2f)", measure, value, lo, hi))
  }
  if (measure %in% ratio_measures) return(sprintf("%s %.2f", measure, value))
  sprintf("%.2f", value)
}

# ---------------------------------------------------------------------------
# Table 10: secondary clinical and economic outcomes
# ---------------------------------------------------------------------------
table10 <- outcomes %>%
  mutate(
    `Group / comparator` = dplyr::case_when(
      ssi_group == "SSI" & !is.na(comparator) & comparator != "Not applicable" ~
        paste0("SSI vs ", comparator),
      TRUE ~ ssi_group
    ),
    `Effect (95% CI)` = purrr::pmap_chr(
      list(effect_measure, effect_value, ci_low, ci_high, domain),
      fmt_effect
    )
  ) %>%
  transmute(
    Study = study_label,
    Year = publication_year,
    Country = country,
    Region = subregion,
    Domain = domain,
    Outcome = outcome,
    `Group / comparator`,
    `Reported value` = value,
    `Effect (95% CI)`,
    p = p_value,
    Notes = notes
  ) %>%
  arrange(Domain, Study, Outcome)

# ---------------------------------------------------------------------------
# Table 11: reported drivers of SSI / AMR
# ---------------------------------------------------------------------------
table11 <- drivers %>%
  mutate(
    `Effect (95% CI)` = purrr::pmap_chr(
      list(effect_measure, effect_value, ci_low, ci_high, domain),
      fmt_effect
    )
  ) %>%
  transmute(
    Study = study_label,
    Year = publication_year,
    Country = country,
    Region = subregion,
    Domain = domain,
    Driver = driver,
    Comparison = comparison,
    `Effect (95% CI)`,
    p = p_value,
    Direction = direction,
    Notes = notes
  ) %>%
  arrange(Domain, Study, Driver)

# Domain-level summary, used in the Results narrative
table12 <- drivers %>%
  group_by(Domain = domain) %>%
  summarise(
    Studies = dplyr::n_distinct(study_id),
    Estimates = dplyr::n(),
    `Ratio estimates with CI` = sum(effect_measure %in% ratio_measures &
                                      !is.na(effect_value) & !is.na(ci_low) & !is.na(ci_high)),
    `Example drivers` = paste(unique(driver)[seq_len(min(3, dplyr::n_distinct(driver)))],
                              collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(desc(Studies), Domain)

# ---------------------------------------------------------------------------
# Figure 8: ratio measures for reported drivers
# ---------------------------------------------------------------------------
driver_plot_df <- drivers %>%
  filter(
    effect_measure %in% ratio_measures,
    !is.na(effect_value), !is.na(ci_low), !is.na(ci_high),
    ci_low > 0, effect_value > 0
  ) %>%
  mutate(
    Label = paste0(study_label, ": ", driver),
    Direction = ifelse(effect_value >= 1, "Increases risk", "Protective / lower risk"),
    Measure = factor(effect_measure, levels = ratio_measures)
  )

p_drivers <- ggplot(
  driver_plot_df,
  aes(x = effect_value, y = reorder(Label, effect_value))
) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey35") +
  geom_pointrange(
    aes(xmin = ci_low, xmax = ci_high, colour = Direction, shape = Measure),
    size = 0.45
  ) +
  scale_x_log10(
    breaks = c(0.3, 0.5, 1, 2, 5, 10, 20, 50, 100),
    labels = c("0.3", "0.5", "1", "2", "5", "10", "20", "50", "100")
  ) +
  scale_colour_manual(values = c("Increases risk" = "#d95f02",
                                 "Protective / lower risk" = "#1b9e77")) +
  scale_shape_manual(values = c(AOR = 16, OR = 17, ARR = 15, cHR = 18, RR = 3, COR = 4)) +
  labs(
    x = "Ratio measure (log scale)",
    y = NULL,
    colour = "Direction",
    shape = "Effect measure",
    subtitle = "Ratios above 1 indicate higher SSI risk. Estimates are not pooled."
  ) +
  guides(
    colour = guide_legend(nrow = 1, title.position = "top"),
    shape = guide_legend(nrow = 2, title.position = "top")
  ) +
  theme_ecsa() +
  theme(axis.text.y = element_text(size = 7.5),
        legend.box = "vertical",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9))

save_png(p_drivers, "fig8_driver_effect_estimates.png", width = 11.5, height = 8.5)

# ---------------------------------------------------------------------------
# Write tables
# ---------------------------------------------------------------------------
write_csv(table10, file.path(tab_dir, "table10_secondary_outcomes.csv"))
write_csv(table11, file.path(tab_dir, "table11_reported_drivers.csv"))
write_csv(table12, file.path(tab_dir, "table12_driver_domains.csv"))

# ---------------------------------------------------------------------------
# Save results for the R Markdown report
# ---------------------------------------------------------------------------
results <- list(
  outcomes = outcomes,
  drivers = drivers,
  table10 = table10,
  table11 = table11,
  table12 = table12,
  n_outcome_rows = nrow(outcomes),
  n_outcome_studies = dplyr::n_distinct(outcomes$study_id),
  n_driver_rows = nrow(drivers),
  n_driver_studies = dplyr::n_distinct(drivers$study_id),
  n_ratio_estimates = nrow(driver_plot_df)
)
saveRDS(results, file.path(data_dir, "secondary_results.rds"))

# ---------------------------------------------------------------------------
# Validation log
# ---------------------------------------------------------------------------
sink(file.path(log_dir, "04_secondary.log"))
cat("ECSA secondary outcomes and AMR/SSI drivers\n")
cat("===========================================\n\n")
cat("Secondary outcome rows:", nrow(outcomes), "from",
    dplyr::n_distinct(outcomes$study_id), "studies\n")
cat("Driver rows:", nrow(drivers), "from",
    dplyr::n_distinct(drivers$study_id), "studies\n")
cat("Ratio estimates plotted:", nrow(driver_plot_df), "\n\n")
cat("Secondary outcomes\n------------------\n")
print(as.data.frame(table10), row.names = FALSE)
cat("\nDriver domains\n--------------\n")
print(as.data.frame(table12), row.names = FALSE)
cat("\nDriver estimates\n----------------\n")
print(as.data.frame(table11), row.names = FALSE)
cat("\nExclusion checks\n----------------\n")
cat("FALCON / Oladipo rows in outcomes or drivers:",
    sum(grepl("falcon|oladipo", tolower(c(outcomes$study_id, drivers$study_id,
                                          outcomes$study_label, drivers$study_label)))), "\n")
cat("Starr 2023 secondary rows included:",
    sum(outcomes$study_id == "starr2023"), " (not used in primary pooling)\n")
sink()

cat("Secondary synthesis complete:", nrow(outcomes), "outcome rows and",
    nrow(drivers), "driver rows.\n")
