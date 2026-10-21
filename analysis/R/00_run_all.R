# ---------------------------------------------------------------------------
# 00_run_all.R
# Run the complete ECSA SSI/AMR analysis and render the manuscript.
#
# Usage (from any directory):
#   .analysis-env/r-env/bin/Rscript analysis/R/00_run_all.R
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(file_arg) == 1 && nzchar(file_arg)) {
  root <- normalizePath(file.path(dirname(file_arg), "..", ".."), mustWork = TRUE)
} else {
  root <- normalizePath(getwd(), mustWork = TRUE)
}
setwd(root)

if (!file.exists(file.path(root, "output", "incidence.csv"))) {
  stop("Cannot locate the repository root (output/incidence.csv not found).")
}

rscript <- file.path(R.home("bin"), "Rscript")
log_file <- file.path(root, "analysis/output/logs/00_run_all.log")
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

# Make the bundled pandoc visible to rmarkdown when it is not on PATH.
pandoc_dir <- file.path(root, ".analysis-env/r-env/bin")
if (dir.exists(pandoc_dir)) {
  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
}

log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
on.exit({
  sink()
  close(log_con)
}, add = TRUE)

cat("ECSA SSI/AMR full pipeline\n")
cat("==========================\n")
cat("Repository root:", root, "\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

run_script <- function(path) {
  cat("\n--- Running", path, "---\n")
  status <- system2(rscript, path)
  if (!identical(status, 0L)) {
    stop("Script failed with status ", status, ": ", path)
  }
  cat("--- Completed", path, "---\n")
}

scripts <- c(
  "analysis/R/01_data_prep.R",
  "analysis/R/02_primary_meta.R",
  "analysis/R/03_amr.R",
  "analysis/R/04_secondary.R",
  "analysis/R/05_prisma.R"
)

for (script in scripts) run_script(script)

cat("\n--- Rendering manuscript ---\n")
rmd <- file.path(root, "analysis/ECSA_SSI_AMR_analysis.Rmd")
out_dir <- file.path(root, "analysis/output")

for (fmt in c("html_document", "word_document")) {
  cat("\nRendering", fmt, "...\n")
  rmarkdown::render(
    input = rmd,
    output_format = fmt,
    output_dir = out_dir,
    knit_root_dir = root,
    quiet = FALSE
  )
}

cat("\n--- Verifying outputs ---\n")
status <- system2(rscript, file.path(root, "analysis/R/99_verify.R"))
if (!identical(status, 0L)) {
  stop("Verification failed with status ", status)
}

cat("\nPipeline completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("HTML:", file.path(out_dir, "ECSA_SSI_AMR_analysis.html"), "\n")
cat("DOCX:", file.path(out_dir, "ECSA_SSI_AMR_analysis.docx"), "\n")
