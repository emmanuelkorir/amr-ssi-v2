# ---------------------------------------------------------------------------
# 99_verify.R
# Verify the key counts, estimates, datasets, tables and figures.
#
# Usage (from the repository root):
#   .analysis-env/r-env/bin/Rscript analysis/R/99_verify.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(file_arg) == 1 && nzchar(file_arg)) {
  root <- normalizePath(file.path(dirname(file_arg), "..", ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
setwd(root)

dirs <- c("analysis/data", "analysis/output/tables", "analysis/output/figures",
          "analysis/output/logs")
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
log_file <- file.path(root, "analysis/output/logs/99_verify.log")

log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
on.exit({
  sink()
  close(log_con)
}, add = TRUE)

cat("ECSA SSI/AMR verification\n")
cat("=========================\n")
cat("Run:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

results <- list()
check <- function(label, ok, detail = "") {
  ok <- isTRUE(ok)
  results[[length(results) + 1]] <<- data.frame(
    Check = label, Pass = ok, Detail = as.character(detail),
    stringsAsFactors = FALSE
  )
  cat(sprintf("[%s] %s%s\n", ifelse(ok, "PASS", "FAIL"), label,
              ifelse(nzchar(detail), paste0("  (", detail, ")"), "")))
  invisible(ok)
}

# ---- Core result objects --------------------------------------------------
pm  <- readRDS(file.path(root, "analysis/data/primary_meta_results.rds"))
amr <- readRDS(file.path(root, "analysis/data/amr_results.rds"))
sec <- readRDS(file.path(root, "analysis/data/secondary_results.rds"))
pr  <- readRDS(file.path(root, "analysis/data/prisma_results.rds"))

inc  <- read_csv(file.path(root, "analysis/data/analysis_incidence.csv"), show_col_types = FALSE)
amrd <- read_csv(file.path(root, "analysis/data/analysis_amr.csv"), show_col_types = FALSE)
stud <- read_csv(file.path(root, "analysis/data/analysis_studies.csv"), show_col_types = FALSE)
ast  <- read_csv(file.path(root, "analysis/data/ast_panel.csv"), show_col_types = FALSE)

# ---- PRISMA counts --------------------------------------------------------
check("PRISMA documents = 56", pr$flow$documents == 56)
check("PRISMA excluded = 18", pr$flow$excluded == 18)
check("PRISMA included cohorts = 38", pr$flow$included == 38)
check("Incidence component = 26", pr$flow$incidence_component == 26)
check("Primary incidence cohorts = 25", pr$flow$primary_incidence == 25)
check("AMR component = 26", pr$flow$amr_component == 26)
check("AMR-only cohorts = 12", pr$flow$amr_only == 12)
check("Priority AMR studies = 23", pr$flow$amr_priority_studies == 23)
check("Priority AMR rows = 54", pr$flow$amr_priority_rows == 54)
check("Exclusion categories sum to 18", sum(pr$exclusion_counts$n) == 18,
      paste(pr$exclusion_counts$n, collapse = "+"))

# ---- Primary dataset and model -------------------------------------------
check("Primary dataset has 25 rows", nrow(inc) == 25, nrow(inc))
check("Primary events = 2,077", sum(inc$events) == 2077, sum(inc$events))
check("Primary patients = 13,233", sum(inc$n) == 13233, sum(inc$n))
check("No duplicate study IDs in primary dataset",
      !anyDuplicated(inc$study_id))
check("All primary events <= denominator", all(inc$events <= inc$n))
check("All primary denominators positive", all(inc$n > 0))

ov <- pm$overall
check("Model k = 25", ov$k == 25)
check("Model events = 2,077", ov$Events == 2077)
check("Model patients = 13,233", ov$Patients == 13233)
check("Pooled SSI proportion = 15.03%", abs(ov$Proportion - 0.1503) < 0.001,
      sprintf("%.4f", ov$Proportion))
check("Pooled SSI 95% CI lower = 10.60%", abs(ov$Lower - 0.1060) < 0.001,
      sprintf("%.4f", ov$Lower))
check("Pooled SSI 95% CI upper = 20.89%", abs(ov$Upper - 0.2089) < 0.001,
      sprintf("%.4f", ov$Upper))
check("I2 = 97.5%", abs(ov$I2 - 97.5) < 0.1, sprintf("%.2f", ov$I2))
check("tau2 = 0.8986", abs(ov$tau2 - 0.8986) < 0.001, sprintf("%.4f", ov$tau2))
check("Prediction interval lower = 2.35%", abs(ov$PI_lower - 0.0235) < 0.001,
      sprintf("%.4f", ov$PI_lower))
check("Prediction interval upper = 56.57%", abs(ov$PI_upper - 0.5657) < 0.001,
      sprintf("%.4f", ov$PI_upper))
check("Common-effect estimate = 18.45%",
      abs(pm$overall_common$Proportion - 0.1845) < 0.001,
      sprintf("%.4f", pm$overall_common$Proportion))
check("Subgroup table has 16 rows", nrow(pm$subgroups) == 16)
check("Meta-regression table has 6 rows", nrow(pm$metareg) == 6)
check("Sensitivity table has 9 rows", nrow(pm$sensitivity) == 9)
check("Leave-one-out table has 25 rows", nrow(pm$loo) == 25)
check("Trim-and-fill added 6 studies", pm$trimfill$k0 == 6)
check("Trim-and-fill adjusted estimate = 20.26%",
      abs(pm$trimfill$proportion - 0.2026) < 0.001,
      sprintf("%.4f", pm$trimfill$proportion))

# ---- Inclusion/exclusion integrity ---------------------------------------
check("Starr is absent from the primary analysis dataset",
      !any(grepl("starr", inc$study_id, ignore.case = TRUE)))
check("Starr is present in the included-study data",
      any(grepl("starr", stud$study_id, ignore.case = TRUE)))
check("Starr has zero primary incidence rows",
      all(stud$n_primary_incidence_rows[grepl("starr", stud$study_id, ignore.case = TRUE)] == 0))
check("Starr has an incidence component",
      all(stud$incidence_component[grepl("starr", stud$study_id, ignore.case = TRUE)]))
check("Starr is absent from Table 1",
      !any(grepl("Starr", read_csv(file.path(root, "analysis/output/tables/table1_primary_incidence_studies.csv"),
                                   show_col_types = FALSE)$Study)))
check("Starr is present in Table S1",
      any(grepl("Starr", read_csv(file.path(root, "analysis/output/tables/tableS1_all_included_studies.csv"),
                                  show_col_types = FALSE)$Study)))

falcon_hits <- c(
  grepl("falcon|aaron|oladipo", paste(stud$study_id, stud$first_author, collapse = " "), ignore.case = TRUE),
  grepl("falcon|aaron|oladipo", paste(inc$study_id, collapse = " "), ignore.case = TRUE),
  grepl("falcon|aaron|oladipo", paste(amrd$study_id, collapse = " "), ignore.case = TRUE),
  grepl("falcon|aaron|oladipo", paste(sec$outcomes$study_id, collapse = " "), ignore.case = TRUE),
  grepl("falcon|aaron|oladipo", paste(sec$drivers$study_id, collapse = " "), ignore.case = TRUE)
)
check("FALCON is absent from all analysis datasets", !any(falcon_hits))
check("FALCON is recorded as an exclusion in Table S2",
      any(grepl("FALCON", read_csv(file.path(root, "analysis/output/tables/tableS2_excluded_documents.csv"),
                                   show_col_types = FALSE)$Document, ignore.case = TRUE)))

# ---- AMR datasets and summaries ------------------------------------------
check("Priority AMR extracted rows = 54", amr$n_priority_rows == 54)
check("Priority AMR de-duplicated rows = 51", amr$n_dedup_rows == 51)
check("AMR study-outcome rows = 40", amr$n_study_outcome_rows == 40)
check("AMR studies = 23", amr$n_studies == 23)
check("AST panel rows = 535", amr$n_ast_rows == 535)
check("AST panel studies = 16", amr$n_ast_studies == 16)
check("AMR overall table has 3 rows", nrow(amr$overall) == 3)
check("AMR study-level rows = 40", nrow(amr$study_level) == 40)
check("No duplicate study/outcome AMR rows",
      !anyDuplicated(paste(amr$study_level$study_id, amr$study_level$outcome_group)))
check("No AMR resistant count exceeds tested",
      all(amr$study_level$n_resistant <= amr$study_level$n_tested))
check("No Southern African priority AMR row",
      !any(amr$by_region$subregion == "Southern Africa"))

mrsa <- amr$overall[amr$overall$outcome_group == "MRSA", ]
gc3  <- amr$overall[amr$overall$outcome_group == "3GC/ESBL resistance", ]
carb <- amr$overall[amr$overall$outcome_group == "Carbapenem resistance", ]
check("MRSA studies = 16", mrsa$Studies == 16)
check("MRSA resistant/tested = 283/517", mrsa$Resistant == 283 && mrsa$Tested == 517)
check("3GC/ESBL studies = 15", gc3$Studies == 15)
check("3GC/ESBL resistant/tested = 597/759", gc3$Resistant == 597 && gc3$Tested == 759)
check("Carbapenem studies = 9", carb$Studies == 9)
check("Carbapenem resistant/tested = 102/286", carb$Resistant == 102 && carb$Tested == 286)

# ---- Secondary outcomes and drivers --------------------------------------
check("Secondary outcome rows = 12", sec$n_outcome_rows == 12)
check("Secondary outcome studies = 7", sec$n_outcome_studies == 7)
check("Driver rows = 26", sec$n_driver_rows == 26)
check("Driver studies = 13", sec$n_driver_studies == 13)
check("Ratio estimates = 21", sec$n_ratio_estimates == 21)
check("Table 10 has 12 rows", nrow(sec$table10) == 12)
check("Table 11 has 26 rows", nrow(sec$table11) == 26)
check("Table 12 has 12 domains", nrow(sec$table12) == 12)

# ---- Table and figure inventory ------------------------------------------
table_files <- c(
  "table1_primary_incidence_studies.csv", "table2_amr_only_studies.csv",
  "table3_primary_meta_results.csv", "table4_subgroup_incidence.csv",
  "table5_exploratory_metaregression.csv", "table6_amr_overall.csv",
  "table7_amr_by_region.csv", "table8_amr_by_hospital.csv",
  "table9_amr_by_pathogen.csv", "table10_secondary_outcomes.csv",
  "table11_reported_drivers.csv", "table12_driver_domains.csv",
  "tableS1_all_included_studies.csv", "tableS2_excluded_documents.csv",
  "tableS3_sensitivity_analyses.csv", "tableS4_leave_one_out.csv",
  "tableS5_small_study_effects.csv", "tableS6_study_level_estimates.csv",
  "tableS7_amr_pathogen_region.csv", "tableS8_amr_region_hospital.csv",
  "tableS9_amr_study_estimates.csv", "tableS10_ast_summary.csv",
  "tableS11_prisma_counts.csv", "tableS12_exclusion_reasons.csv"
)
missing_tables <- table_files[!file.exists(file.path(root, "analysis/output/tables", table_files))]
check("All 24 table files exist", length(missing_tables) == 0,
      paste(missing_tables, collapse = ", "))

check("Table 1 has 25 rows",
      nrow(read_csv(file.path(root, "analysis/output/tables/table1_primary_incidence_studies.csv"),
                    show_col_types = FALSE)) == 25)
check("Table 2 has 12 rows",
      nrow(read_csv(file.path(root, "analysis/output/tables/table2_amr_only_studies.csv"),
                    show_col_types = FALSE)) == 12)
check("Table S1 has 38 rows",
      nrow(read_csv(file.path(root, "analysis/output/tables/tableS1_all_included_studies.csv"),
                    show_col_types = FALSE)) == 38)
check("Table S2 has 18 rows",
      nrow(read_csv(file.path(root, "analysis/output/tables/tableS2_excluded_documents.csv"),
                    show_col_types = FALSE)) == 18)
check("Table S7 has 14 rows",
      nrow(read_csv(file.path(root, "analysis/output/tables/tableS7_amr_pathogen_region.csv"),
                    show_col_types = FALSE)) == 14)
check("Table S8 has 10 rows",
      nrow(read_csv(file.path(root, "analysis/output/tables/tableS8_amr_region_hospital.csv"),
                    show_col_types = FALSE)) == 10)
check("Table S9 has 40 rows",
      nrow(read_csv(file.path(root, "analysis/output/tables/tableS9_amr_study_estimates.csv"),
                    show_col_types = FALSE)) == 40)
check("Table S10 has 107 rows",
      nrow(read_csv(file.path(root, "analysis/output/tables/tableS10_ast_summary.csv"),
                    show_col_types = FALSE)) == 107)

figure_files <- c(
  "fig1_forest_primary.png", "fig2_funnel_primary.png",
  "fig3_subgroup_summary.png", "fig4_amr_priority_dotplot.png",
  "fig5_amr_study_outcome_heatmap.png", "fig6_ast_gram_negative_heatmap.png",
  "fig7_ast_saureus.png", "fig8_driver_effect_estimates.png",
  "figS1_leave_one_out.png", "figS2_forest_specialty.png",
  "figS3_amr_pathogen_specific.png", "figS4_prisma_flow.png"
)
missing_figs <- figure_files[!file.exists(file.path(root, "analysis/output/figures", figure_files))]
check("All 12 figure files exist", length(missing_figs) == 0,
      paste(missing_figs, collapse = ", "))

rmd <- file.path(root, "analysis/ECSA_SSI_AMR_analysis.Rmd")
check("Manuscript Rmd exists", file.exists(rmd))
check("Manuscript HTML exists",
      file.exists(file.path(root, "analysis/output/ECSA_SSI_AMR_analysis.html")))
check("Manuscript DOCX exists",
      file.exists(file.path(root, "analysis/output/ECSA_SSI_AMR_analysis.docx")))
check("Manuscript DOCX is non-empty",
      file.info(file.path(root, "analysis/output/ECSA_SSI_AMR_analysis.docx"))$size > 10000,
      file.info(file.path(root, "analysis/output/ECSA_SSI_AMR_analysis.docx"))$size)

# ---- Summary --------------------------------------------------------------
res <- do.call(rbind, results)
n_fail <- sum(!res$Pass)
cat("\nSummary:", nrow(res), "checks;", sum(res$Pass), "passed;", n_fail, "failed.\n")
if (n_fail > 0) {
  cat("\nFailed checks:\n")
  print(res[!res$Pass, c("Check", "Detail")], row.names = FALSE)
}

cat("\nVerification log written to:", log_file, "\n")
if (n_fail > 0) quit(status = 1)
