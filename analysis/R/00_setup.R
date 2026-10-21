# ---------------------------------------------------------------------------
# 00_setup.R
# Shared setup for the ECSA SSI/AMR reproducibility pipeline.
# Run all scripts from the repository root.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(meta)
  library(metafor)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(scales)
  library(patchwork)
  library(knitr)
})

root <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(root, "output", "incidence.csv"))) {
  stop("Run this pipeline from the repository root (the folder containing output/incidence.csv).")
}

dirs <- c(
  "analysis/data",
  "analysis/output",
  "analysis/output/figures",
  "analysis/output/tables",
  "analysis/output/logs"
)
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

fig_dir  <- file.path(root, "analysis/output/figures")
tab_dir  <- file.path(root, "analysis/output/tables")
log_dir  <- file.path(root, "analysis/output/logs")
data_dir <- file.path(root, "analysis/data")

theme_ecsa <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(colour = "grey30"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

save_png <- function(plot, filename, width = 9, height = 6, dpi = 300) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot = plot, width = width, height = height, dpi = dpi, bg = "white"
  )
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", sprintf(paste0("%.", digits, "f%%"), 100 * x))
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

fmt_ci <- function(est, lo, hi, digits = 1) {
  paste0(fmt_pct(est, digits), " (95% CI ", fmt_pct(lo, digits), " to ", fmt_pct(hi, digits), ")")
}

# logit-transformed proportion and variance, used for meta-regression
logit_prop <- function(events, n) {
  q <- (events + 0.5) / (n + 1)
  log(q / (1 - q))
}
var_logit_prop <- function(events, n) {
  1 / (events + 0.5) + 1 / (n - events + 0.5)
}
