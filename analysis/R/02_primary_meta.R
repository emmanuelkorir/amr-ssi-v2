# ---------------------------------------------------------------------------
# 02_primary_meta.R
# Primary SSI incidence: random-effects meta-analysis of proportions,
# protocol subgroup analyses, sensitivity analyses, small-study effects and
# exploratory meta-regression.
# ---------------------------------------------------------------------------

source("analysis/R/00_setup.R")

primary <- readr::read_csv(
  file.path(data_dir, "analysis_incidence.csv"),
  show_col_types = FALSE, progress = FALSE
) %>%
  mutate(
    study_label = paste0(first_author, " ", publication_year),
    quality_overall = factor(quality_overall, levels = c("High", "Moderate", "Low"))
  )

if (nrow(primary) != 25) stop("Expected 25 primary incidence cohorts")
if (anyDuplicated(primary$study_id)) stop("Duplicate study_id in primary set")
if (any(primary$events > primary$n)) stop("Events exceed denominator")

# ---------------------------------------------------------------------------
# Model helpers
# ---------------------------------------------------------------------------
fit_meta <- function(data, ci = "HK", common = FALSE, prediction = TRUE) {
  meta::metaprop(
    event = events, n = n, data = data,
    studlab = study_label,
    sm = "PLOGIT",
    method = "Inverse",
    method.tau = "REML",
    method.random.ci = ci,
    prediction = prediction,
    common = common,
    random = TRUE
  )
}

# Overall model used throughout the manuscript
m <- fit_meta(primary)

tidy_overall <- function(model, label, data = NULL, type = c("random", "common")) {
  type <- match.arg(type)
  if (is.null(data)) data <- model$data
  est <- if (type == "common") model$TE.common else model$TE.random
  lo  <- if (type == "common") model$lower.common else model$lower.random
  hi  <- if (type == "common") model$upper.common else model$upper.random
  pi_lo <- if (type == "random" && !is.null(model$lower.predict)) plogis(model$lower.predict) else NA_real_
  pi_hi <- if (type == "random" && !is.null(model$upper.predict)) plogis(model$upper.predict) else NA_real_
  tibble::tibble(
    Analysis = label,
    k = as.integer(model$k),
    Events = sum(data$events),
    Patients = sum(data$n),
    Proportion = plogis(est),
    Lower = plogis(lo),
    Upper = plogis(hi),
    PI_lower = pi_lo,
    PI_upper = pi_hi,
    tau2 = model$tau2,
    I2 = 100 * model$I2,
    Q = model$Q,
    p_Q = model$pval.Q
  )
}

overall <- tidy_overall(m, "Primary random-effects model")

# Fixed/common-effect estimate for comparison
m_common <- fit_meta(primary, common = TRUE, prediction = FALSE)

# Classic Wald random-effects CI (sensitivity)
m_classic <- fit_meta(primary, ci = "classic", prediction = FALSE)

# ---------------------------------------------------------------------------
# Small-study effects and publication bias
# ---------------------------------------------------------------------------
egger <- meta::metabias(m, method.bias = "Egger", k.min = 10)
begg  <- meta::metabias(m, method.bias = "Begg",  k.min = 10)
trimfill <- meta::trimfill(m)

bias <- tibble::tibble(
  Test = c("Egger regression", "Begg rank correlation", "Trim-and-fill (adjusted)"),
  Estimate = c(
    unname(egger$statistic),
    unname(begg$statistic),
    plogis(trimfill$TE.random)
  ),
  p_value = c(egger$pval, begg$pval, NA_real_),
  Detail = c(
    sprintf("bias coefficient %.3f (SE %.3f)", egger$estimate["bias"], egger$estimate["se.bias"]),
    sprintf("Kendall score %.1f (SE %.1f)", begg$estimate["ks"], begg$estimate["se.ks"]),
    sprintf("%d studies added; adjusted proportion %.1f%% (95%% CI %.1f-%.1f)",
            trimfill$k0, 100 * plogis(trimfill$TE.random),
            100 * plogis(trimfill$lower.random), 100 * plogis(trimfill$upper.random))
  )
)

# ---------------------------------------------------------------------------
# Leave-one-out analysis
# ---------------------------------------------------------------------------
mi <- meta::metainf(m, pooled = "random")
loo <- tibble::tibble(
  Omitted = mi$studlab,
  Proportion = plogis(mi$TE),
  Lower = plogis(mi$lower),
  Upper = plogis(mi$upper),
  I2 = 100 * mi$I2
) %>%
  arrange(Proportion)

# ---------------------------------------------------------------------------
# Protocol subgroup analyses
# ---------------------------------------------------------------------------
subgroup_vars <- list(
  "Surgical specialty" = "specialty_group",
  "Hospital level" = "hospital_group",
  "ECSA sub-region" = "subregion",
  "Study design" = "design_group",
  "Publication period" = "period_group",
  "Wound contamination class" = "contamination_group"
)

# Wound contamination is a study-level descriptor for most cohorts; individual
# patient wound class is only available as a proportion and is handled in the
# meta-regression below. Do not invent a three-level subgroup from it.

subgroup_vars <- subgroup_vars[names(subgroup_vars) != "Wound contamination class"]

run_subgroup <- function(var, label) {
  d <- primary %>% filter(!is.na(.data[[var]]))
  if (length(unique(d[[var]])) < 2) return(NULL)
  ms <- update(m, subgroup = d[[var]], subgroup.name = label)
  tibble::tibble(
    Subgroup_variable = label,
    Subgroup = as.character(ms$subgroup.levels),
    k = as.integer(ms$k.w),
    Events = as.numeric(ms$event.w),
    Patients = as.numeric(ms$n.w),
    Proportion = plogis(ms$TE.random.w),
    Lower = plogis(ms$lower.random.w),
    Upper = plogis(ms$upper.random.w),
    tau2 = as.numeric(ms$tau2.w),
    I2 = 100 * as.numeric(ms$I2.w),
    p_within = as.numeric(ms$pval.Q.w),
    Q_between = as.numeric(ms$Q.b.random),
    df_between = as.numeric(ms$df.Q.b.random),
    p_between = as.numeric(ms$pval.Q.b.random)
  )
}

subgroups <- purrr::map2_dfr(subgroup_vars, names(subgroup_vars), run_subgroup)

# ---------------------------------------------------------------------------
# Exploratory meta-regression (study-level covariates)
# ---------------------------------------------------------------------------
# All models use complete cases for the covariate in question. Effects are
# expressed per pre-specified increment so the coefficients are interpretable.
mr_specs <- tibble::tribble(
  ~variable,        ~label,                                      ~increment, ~increment_label,
  "male_pct",       "Male proportion among SSI cases",           0.10,       "per 10 percentage points",
  "hiv_pct",        "HIV positivity among SSI cases",            0.10,       "per 10 percentage points",
  "preop_pct",      "Preoperative antibiotic prophylaxis",       0.10,       "per 10 percentage points",
  "contaminated_pct", "Contaminated or dirty wound class",       0.10,       "per 10 percentage points",
  "publication_year", "Publication year",                        1,          "per year",
  "log_n",          "Cohort size (log10)",                       log10(2),   "per doubling"
)

primary <- primary %>% mutate(log_n = log10(n))

run_metareg <- function(variable, label, increment, increment_label) {
  d <- primary %>% filter(!is.na(.data[[variable]]))
  if (nrow(d) < 4) return(NULL)
  md <- fit_meta(d, prediction = FALSE)
  rg <- meta::metareg(md, as.formula(paste("~", variable)))
  idx <- which(rownames(rg$beta) == variable)
  beta <- rg$beta[idx, 1]
  se <- as.numeric(rg$se)[idx]
  p <- as.numeric(rg$pval)[idx]
  df <- max(rg$k - 2, 1)
  tcrit <- qt(0.975, df)
  tibble::tibble(
    Covariate = label,
    k = as.integer(rg$k),
    Increment = increment_label,
    OR = exp(beta * increment),
    Lower = exp((beta - tcrit * se) * increment),
    Upper = exp((beta + tcrit * se) * increment),
    p_value = p,
    R2_percent = rg$R2,
    tau2 = rg$tau2
  )
}

metareg <- purrr::map2_dfr(
  mr_specs$variable, mr_specs$label,
  ~ run_metareg(.x, .y, mr_specs$increment[mr_specs$variable == .x],
                mr_specs$increment_label[mr_specs$variable == .x])
)

# ---------------------------------------------------------------------------
# Sensitivity analyses
# ---------------------------------------------------------------------------
sensitivity_sets <- list(
  "Primary (all cohorts)" = primary,
  "Excluding derived incidence counts" = primary %>% filter(tolower(derived_flag) != "derived"),
  "Excluding low-quality studies" = primary %>% filter(quality_overall != "Low"),
  "Excluding before-after/quality-improvement designs" = primary %>% filter(design_group != "Before-after/QI"),
  "Excluding the culture-confirmed-only cohort (Boru 2025)" = primary %>% filter(study_id != "boru2025"),
  "Cohort studies only" = primary %>% filter(design_group == "Cohort"),
  "Excluding cohorts with >1,000 patients" = primary %>% filter(n <= 1000)
)

sensitivity <- purrr::map2_dfr(
  sensitivity_sets, names(sensitivity_sets),
  ~ tidy_overall(fit_meta(.x, prediction = FALSE), .y, data = .x)
)

# Add model-based sensitivity rows
sensitivity <- bind_rows(
  sensitivity,
  tidy_overall(m_common, "Common-effect model", data = primary, type = "common"),
  tidy_overall(m_classic, "Random-effects model with Wald CI", data = primary)
) %>%
  mutate(
    Proportion = 100 * Proportion, Lower = 100 * Lower, Upper = 100 * Upper,
    I2 = round(I2, 1), tau2 = round(tau2, 4)
  )

# ---------------------------------------------------------------------------
# Study-level estimates with Wilson confidence intervals
# ---------------------------------------------------------------------------
wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denom
  c(lower = max(0, centre - half), upper = min(1, centre + half))
}

study_estimates <- primary %>%
  rowwise() %>%
  mutate(
    wilson = list(wilson_ci(events, n)),
    Wilson_lower = wilson["lower"],
    Wilson_upper = wilson["upper"]
  ) %>%
  ungroup() %>%
  transmute(
    Study = study_label,
    Country = country,
    Subregion = subregion,
    `Surgical specialty` = specialty_group,
    `Hospital level` = hospital_group,
    Events = events,
    Patients = n,
    `SSI %` = round(100 * pct, 1),
    `Wilson 95% CI` = sprintf("%.1f-%.1f", 100 * Wilson_lower, 100 * Wilson_upper),
    `Derived count` = ifelse(tolower(derived_flag) == "derived", "Yes", "No"),
    Quality = quality_overall
  ) %>%
  arrange(Study)

# ---------------------------------------------------------------------------
# Figures
# ---------------------------------------------------------------------------
# Figure 1: study-level forest plot
ragg::agg_png(
  file.path(fig_dir, "fig1_forest_primary.png"),
  width = 3000, height = 2700, res = 300
)
meta::forest(
  m,
  pscale = 100,
  prediction = TRUE,
  common = FALSE,
  col.diamond = "black",
  col.predict = "darkred",
  fontsize = 8,
  spacing = 1.1,
  addrow = TRUE,
  label.left = "Lower SSI incidence",
  label.right = "Higher SSI incidence"
)
dev.off()

# Figure 2: custom funnel plot on the proportion scale
funnel_df <- primary %>%
  mutate(
    logit_p = logit_prop(events, n),
    se_logit = sqrt(var_logit_prop(events, n))
  )
mu <- m$TE.random
se_max <- max(funnel_df$se_logit) * 1.08
se_grid <- seq(0.001, se_max, length.out = 200)

contour_df <- purrr::map_dfr(c(0.90, 0.95, 0.99), function(level) {
  z <- qnorm(1 - (1 - level) / 2)
  tibble::tibble(
    level = paste0(level * 100, "%"),
    se = se_grid,
    xmin = mu - z * se_grid,
    xmax = mu + z * se_grid
  )
})

# Trace each funnel contour as a closed polygon (left side down, right side up)
contour_poly <- contour_df %>%
  group_by(level) %>%
  group_modify(~ tibble::tibble(
    x = c(.x$xmin, rev(.x$xmax)),
    y = c(.x$se, rev(.x$se))
  )) %>%
  ungroup() %>%
  mutate(level = factor(level, levels = c("99%", "95%", "90%")))

pct_breaks <- c(0.01, 0.02, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70)
pct_breaks <- pct_breaks[pct_breaks > plogis(min(funnel_df$logit_p) - 0.2) &
                           pct_breaks < plogis(max(funnel_df$logit_p) + 0.2)]

p_funnel <- ggplot(funnel_df, aes(x = logit_p, y = se_logit)) +
  geom_polygon(
    data = contour_poly,
    aes(x = x, y = y, group = level, fill = level),
    colour = "grey55", linewidth = 0.2, inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c("99%" = "grey90", "95%" = "grey78", "90%" = "grey64"),
                    guide = "none") +
  geom_vline(xintercept = mu, linetype = "dashed", colour = "black") +
  geom_point(shape = 16, size = 2.3, colour = "steelblue", alpha = 0.9) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.02))) +
  scale_x_continuous(
    breaks = log(pct_breaks / (1 - pct_breaks)),
    labels = paste0(round(100 * pct_breaks), "%"),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  labs(
    x = "SSI proportion (logit scale)",
    y = "Standard error",
    subtitle = sprintf(
      "Egger test p = %.3f; Begg test p = %.3f; trim-and-fill adjusted %.1f%%",
      egger$pval, begg$pval, 100 * plogis(trimfill$TE.random)
    )
  ) +
  theme_ecsa() +
  theme(panel.grid.major = element_line(colour = "white", linewidth = 0.3))

save_png(p_funnel, "fig2_funnel_primary.png", width = 8, height = 6.5)

# Figure 3: subgroup summary (all protocol subgroups)
subgroup_plot_df <- subgroups %>%
  mutate(
    label = sprintf("%s (k=%d)", Subgroup, k),
    Subgroup_variable = factor(
      Subgroup_variable,
      levels = c("Surgical specialty", "Hospital level", "ECSA sub-region",
                 "Study design", "Publication period")
    )
  )

p_sub <- ggplot(subgroup_plot_df, aes(x = 100 * Proportion, y = label)) +
  geom_vline(xintercept = 100 * plogis(mu), linetype = "dashed", colour = "grey35") +
  geom_pointrange(aes(xmin = 100 * Lower, xmax = 100 * Upper), size = 0.45,
                  colour = "steelblue") +
  facet_wrap(~ Subgroup_variable, scales = "free_y", ncol = 2) +
  coord_cartesian(xlim = c(0, 65)) +
  labs(x = "SSI proportion (%)", y = NULL,
       subtitle = "Dashed line is the overall random-effects estimate.\nSubgroup CIs extending beyond 65% are truncated for readability.") +
  theme_ecsa() +
  theme(panel.grid.major.y = element_blank())

save_png(p_sub, "fig3_subgroup_summary.png", width = 10, height = 8)

# Supplementary: study-level forest for the specialty subgroup
ragg::agg_png(
  file.path(fig_dir, "figS2_forest_specialty.png"),
  width = 3200, height = 3000, res = 300
)
ms_specialty <- update(m, subgroup = primary$specialty_group,
                             subgroup.name = "Surgical specialty")
meta::forest(
  ms_specialty,
  pscale = 100,
  prediction = FALSE,
  common = FALSE,
  col.diamond = "black",
  fontsize = 7.5,
  spacing = 1.0,
  test.subgroup = TRUE,
  label.left = "Lower SSI incidence",
  label.right = "Higher SSI incidence"
)
dev.off()

# Figure S1: leave-one-out
p_loo <- ggplot(loo, aes(x = 100 * Proportion, y = reorder(Omitted, Proportion))) +
  geom_vline(xintercept = 100 * plogis(mu), linetype = "dashed", colour = "grey35") +
  geom_pointrange(aes(xmin = 100 * Lower, xmax = 100 * Upper),
                  size = 0.35, colour = "steelblue") +
  labs(
    x = "Pooled SSI proportion after omitting the study (%)",
    y = NULL,
    subtitle = sprintf("Overall estimate %.1f%% (dashed line)", 100 * plogis(mu))
  ) +
  theme_ecsa()

save_png(p_loo, "figS1_leave_one_out.png", width = 8, height = 7)

# ---------------------------------------------------------------------------
# Tables
# ---------------------------------------------------------------------------
table_meta <- overall %>%
  transmute(
    Analysis,
    k,
    Events,
    Patients,
    `Pooled SSI %` = round(100 * Proportion, 2),
    `95% CI lower` = round(100 * Lower, 2),
    `95% CI upper` = round(100 * Upper, 2),
    `Prediction interval` = sprintf("%.2f-%.2f", 100 * PI_lower, 100 * PI_upper),
    `I2 (%)` = round(I2, 1),
    `tau2` = round(tau2, 4),
    `Q` = round(Q, 1),
    `p heterogeneity` = ifelse(p_Q < 0.001, "<0.001", sprintf("%.3f", p_Q))
  )

table_subgroups <- subgroups %>%
  transmute(
    `Subgroup variable` = Subgroup_variable,
    Subgroup,
    k,
    Events,
    Patients,
    `SSI %` = round(100 * Proportion, 2),
    `95% CI` = sprintf("%.1f-%.1f", 100 * Lower, 100 * Upper),
    `I2 (%)` = round(I2, 1),
    `p within` = ifelse(p_within < 0.001, "<0.001", sprintf("%.3f", p_within)),
    `p between` = ifelse(p_between < 0.001, "<0.001", sprintf("%.3f", p_between))
  )

table_metareg <- metareg %>%
  transmute(
    Covariate,
    k,
    Increment,
    `OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", OR, Lower, Upper),
    `p` = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)),
    `R2 (%)` = round(R2_percent, 1),
    `tau2` = round(tau2, 4)
  )

table_sensitivity <- sensitivity %>%
  transmute(
    Analysis,
    k,
    Events,
    Patients,
    `SSI %` = round(Proportion, 2),
    `95% CI` = sprintf("%.1f-%.1f", Lower, Upper),
    `I2 (%)` = I2,
    `tau2` = tau2
  )

table_loo <- loo %>%
  transmute(
    `Omitted study` = Omitted,
    `Pooled SSI %` = round(100 * Proportion, 2),
    `95% CI` = sprintf("%.1f-%.1f", 100 * Lower, 100 * Upper),
    `I2 (%)` = round(I2, 1)
  )

table_bias <- bias

write_csv(table_meta,        file.path(tab_dir, "table3_primary_meta_results.csv"))
write_csv(table_subgroups,   file.path(tab_dir, "table4_subgroup_incidence.csv"))
write_csv(table_metareg,     file.path(tab_dir, "table5_exploratory_metaregression.csv"))
write_csv(table_sensitivity, file.path(tab_dir, "tableS3_sensitivity_analyses.csv"))
write_csv(table_loo,         file.path(tab_dir, "tableS4_leave_one_out.csv"))
write_csv(table_bias,        file.path(tab_dir, "tableS5_small_study_effects.csv"))
write_csv(study_estimates,   file.path(tab_dir, "tableS6_study_level_estimates.csv"))

# ---------------------------------------------------------------------------
# Save results for the R Markdown report
# ---------------------------------------------------------------------------
results <- list(
  overall = overall,
  overall_common = tidy_overall(m_common, "Common-effect model", data = primary, type = "common"),
  overall_classic = tidy_overall(m_classic, "Wald CI", data = primary),
  subgroups = subgroups,
  metareg = metareg,
  sensitivity = sensitivity,
  loo = loo,
  bias = bias,
  egger = egger,
  begg = begg,
  trimfill = list(
    k0 = trimfill$k0,
    k = trimfill$k,
    proportion = plogis(trimfill$TE.random),
    lower = plogis(trimfill$lower.random),
    upper = plogis(trimfill$upper.random)
  ),
  flow = readRDS(file.path(data_dir, "flow_counts.rds")),
  analysis_set = primary
)
saveRDS(results, file.path(data_dir, "primary_meta_results.rds"))
saveRDS(m, file.path(data_dir, "primary_meta_model.rds"))

# ---------------------------------------------------------------------------
# Validation log
# ---------------------------------------------------------------------------
sink(file.path(log_dir, "02_primary_meta.log"))
cat("ECSA SSI primary random-effects meta-analysis\n")
cat("=============================================\n\n")
cat("Cohorts:", overall$k, " Events:", overall$Events, " Patients:", overall$Patients, "\n")
cat(sprintf("Pooled SSI: %.2f%% (95%% CI %.2f-%.2f)\n",
            100 * overall$Proportion, 100 * overall$Lower, 100 * overall$Upper))
cat(sprintf("Prediction interval: %.2f-%.2f%%\n", 100 * overall$PI_lower, 100 * overall$PI_upper))
cat(sprintf("I2 = %.1f%%, tau2 = %.4f, Q = %.1f, p < %.2g\n\n",
            overall$I2, overall$tau2, overall$Q, overall$p_Q))
cat("Small-study effects\n-------------------\n")
print(as.data.frame(table_bias), row.names = FALSE)
cat("\nProtocol subgroups\n------------------\n")
print(as.data.frame(table_subgroups), row.names = FALSE)
cat("\nExploratory meta-regression\n--------------------------\n")
print(as.data.frame(table_metareg), row.names = FALSE)
cat("\nSensitivity analyses\n--------------------\n")
print(as.data.frame(table_sensitivity), row.names = FALSE)
cat("\nLeave-one-out range:",
    sprintf("%.2f%% to %.2f%%", 100 * min(loo$Proportion), 100 * max(loo$Proportion)), "\n")
cat("\nOutputs written to analysis/output/figures and analysis/output/tables\n")
sink()

cat("Primary meta-analysis complete:",
    sprintf("%.1f%% (95%% CI %.1f-%.1f), k = %d\n",
            100 * overall$Proportion, 100 * overall$Lower, 100 * overall$Upper, overall$k))
