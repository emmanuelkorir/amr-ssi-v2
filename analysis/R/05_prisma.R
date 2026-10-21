# ---------------------------------------------------------------------------
# 05_prisma.R
# PRISMA 2020 flow diagram and screening counts.
#
# The supplied corpus starts after database searching and de-duplication, so
# the diagram begins at "documents assessed for eligibility" rather than
# inventing identification counts that are not available in the extraction.
# ---------------------------------------------------------------------------

source("analysis/R/00_setup.R")

flow <- readRDS(file.path(data_dir, "flow_counts.rds"))
screening <- readr::read_csv(
  file.path(root, "output", "screening.csv"),
  show_col_types = FALSE, progress = FALSE
)

excluded <- screening %>%
  filter(tolower(decision) == "excluded") %>%
  mutate(
    category = dplyr::case_when(
      grepl("falcon", reason, ignore.case = TRUE) ~ "Post-hoc FALCON secondary analysis",
      grepl("duplicate|overlapping", reason, ignore.case = TRUE) ~ "Duplicate or overlapping reports",
      grepl("not a primary study|not a study|no extractable|workbook|review protocol|response to reviewers|correction/other",
            reason, ignore.case = TRUE) ~ "Not a primary study or no extractable data",
      TRUE ~ "Wrong population or outcome"
    )
  )

exclusion_counts <- excluded %>%
  count(category, name = "n") %>%
  arrange(desc(n))

if (sum(exclusion_counts$n) != flow$excluded) {
  stop("Exclusion categories do not sum to the excluded total")
}
if (flow$documents - flow$excluded != flow$included) {
  stop("PRISMA counts are inconsistent")
}

# ---------------------------------------------------------------------------
# Table of flow counts
# ---------------------------------------------------------------------------
tableS11 <- tibble::tibble(
  Stage = c(
    "Documents in supplied corpus after de-duplication",
    "Documents excluded",
    "Unique cohorts included",
    "Cohorts with an SSI incidence component",
    "Cohorts in the primary SSI meta-analysis",
    "Cohorts with an AMR component",
    "AMR-only cohorts",
    "Studies contributing priority AMR estimates",
    "Priority AMR estimates (rows)"
  ),
  n = c(
    flow$documents,
    flow$excluded,
    flow$included,
    flow$incidence_component,
    flow$primary_incidence,
    flow$amr_component,
    flow$amr_only,
    flow$amr_priority_studies,
    flow$amr_priority_rows
  ),
  Note = c(
    "Starting point of the supplied extraction; identification counts were not available.",
    "See Table S12 for reasons.",
    "No duplicate study IDs among included documents.",
    "Includes Starr 2023.",
    "Starr 2023 excluded from pooling because the reported counts and percentages are incompatible.",
    "Incidence and/or AMR data.",
    "No operated-patient denominator; AMR synthesis only.",
    "Three AMR-component studies contributed no species-level priority row.",
    "Before within-pathogen de-duplication; 51 rows after de-duplication."
  )
)

tableS12 <- exclusion_counts %>%
  mutate(
    Examples = purrr::map_chr(category, function(cat) {
      paste(utils::head(excluded$label[excluded$category == cat], 3), collapse = "; ")
    })
  ) %>%
  rename(`Exclusion category` = category, n = n)

readr::write_csv(tableS11, file.path(tab_dir, "tableS11_prisma_counts.csv"))
readr::write_csv(tableS12, file.path(tab_dir, "tableS12_exclusion_reasons.csv"))

# ---------------------------------------------------------------------------
# PRISMA diagram
# ---------------------------------------------------------------------------
box <- function(xmin, xmax, ymin, ymax, fill = "#e8f1f8", colour = "grey35") {
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
           fill = fill, colour = colour, linewidth = 0.45)
}
box_text <- function(x, y, label, size = 3.1, fontface = "plain") {
  annotate("text", x = x, y = y, label = label, size = size,
           lineheight = 0.92, fontface = fontface)
}
arrow <- function(x, y, xend, yend) {
  annotate("segment", x = x, y = y, xend = xend, yend = yend,
           colour = "grey30", linewidth = 0.45,
           arrow = grid::arrow(length = grid::unit(0.07, "inches"), type = "closed"))
}

p <- ggplot() +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), clip = "off") +
  # Phase labels
  annotate("text", x = 4, y = 91, label = "Identification", angle = 90,
           size = 2.8, colour = "grey40", fontface = "bold") +
  annotate("text", x = 4, y = 76, label = "Screening", angle = 90,
           size = 2.8, colour = "grey40", fontface = "bold") +
  annotate("text", x = 4, y = 55, label = "Included", angle = 90,
           size = 2.8, colour = "grey40", fontface = "bold") +
  annotate("text", x = 4, y = 25, label = "Analysis", angle = 90,
           size = 2.8, colour = "grey40", fontface = "bold") +
  # Main boxes
  box(14, 58, 90, 97) +
  box_text(36, 93.5, "Documents in the supplied corpus\nafter de-duplication\nn = 56", fontface = "bold") +
  box(14, 58, 78, 86) +
  box_text(36, 82, "Documents assessed for eligibility\nn = 56") +
  box(14, 58, 62, 72) +
  box_text(36, 67, "Unique cohorts included\nn = 38", fontface = "bold") +
  # Branches
  box(8, 44, 42, 56, fill = "#f4f4f4") +
  box_text(26, 49, "SSI incidence component\nn = 26 cohorts") +
  box(52, 92, 42, 56, fill = "#f4f4f4") +
  box_text(72, 49, "AMR component\nn = 26 cohorts\n12 AMR-only") +
  # Analysis boxes
  box(6, 46, 20, 34, fill = "#e8f5e9") +
  box_text(26, 27, "Primary SSI meta-analysis\nn = 25 cohorts\n2,077 events / 13,233 patients\nStarr 2023 excluded from pooling: incompatible counts",
           size = 2.9, fontface = "bold") +
  box(50, 94, 20, 34, fill = "#e8f5e9") +
  box_text(72, 27, "Priority AMR synthesis\nn = 23 studies\n54 extracted priority estimates\n3 AMR-component studies had no priority row",
           size = 2.9, fontface = "bold") +
  # Bottom summary
  box(14, 86, 4, 13, fill = "#fff8e1") +
  box_text(50, 8.5, "38 cohorts included: 25 contributed to the primary SSI meta-analysis\nand 23 to the priority AMR synthesis", fontface = "bold") +
  # Excluded side box
  box(64, 98, 68, 88, fill = "#fdecec", colour = "#b03a2e") +
  box_text(81, 78, sprintf(
    "Excluded (n = %d)\nDuplicate/overlapping reports  %d\nNot primary/no extractable data  %d\nWrong population or outcome  %d\nPost-hoc FALCON analysis  %d",
    flow$excluded,
    exclusion_counts$n[exclusion_counts$category == "Duplicate or overlapping reports"],
    exclusion_counts$n[exclusion_counts$category == "Not a primary study or no extractable data"],
    exclusion_counts$n[exclusion_counts$category == "Wrong population or outcome"],
    exclusion_counts$n[exclusion_counts$category == "Post-hoc FALCON secondary analysis"]
  ), size = 2.75) +
  # Arrows
  arrow(36, 90, 36, 86.2) +
  arrow(36, 78, 36, 72.2) +
  arrow(30, 62, 26, 56.2) +
  arrow(42, 62, 72, 56.2) +
  arrow(26, 42, 26, 34.2) +
  arrow(72, 42, 72, 34.2) +
  arrow(26, 20, 40, 13.2) +
  arrow(72, 20, 60, 13.2) +
  arrow(58, 82, 63.8, 82) +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

save_png(p, "figS4_prisma_flow.png", width = 11, height = 8.5)

# ---------------------------------------------------------------------------
# Save results
# ---------------------------------------------------------------------------
results <- list(
  flow = flow,
  exclusion_counts = exclusion_counts,
  tableS11 = tableS11,
  tableS12 = tableS12
)
saveRDS(results, file.path(data_dir, "prisma_results.rds"))

sink(file.path(log_dir, "05_prisma.log"))
cat("PRISMA flow counts\n")
cat("==================\n\n")
print(as.data.frame(tableS11), row.names = FALSE)
cat("\nExclusion categories\n--------------------\n")
print(as.data.frame(tableS12), row.names = FALSE)
sink()

cat("PRISMA flow complete:", flow$documents, "documents,", flow$excluded,
    "excluded,", flow$included, "cohorts included.\n")
