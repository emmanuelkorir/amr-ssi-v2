# SSI burden and antimicrobial resistance in East, Central and Southern Africa

Reproducible, evidence-linked extraction for a systematic review and
meta-analysis of surgical site infection (SSI) burden and antimicrobial
resistance (AMR) in East, Central and Southern Africa (ECSA).

The extraction was built independently from the source documents. The
pre-existing human workbook was used only as a comparator and is never a
source of numeric values.

## Contents

- [What this repository produces](#what-this-repository-produces)
- [Review scope and eligibility](#review-scope-and-eligibility)
- [Extraction rules](#extraction-rules)
- [Evidence model](#evidence-model)
- [Reproducing the build](#reproducing-the-build)
- [Output files](#output-files)
- [Dataset summary](#dataset-summary)
- [Key source decisions and inconsistencies](#key-source-decisions-and-inconsistencies)
- [Human extraction discrepancies](#human-extraction-discrepancies)
- [Limitations](#limitations)
- [Reviewer checklist](#reviewer-checklist)

## What this repository produces

| Item | Count |
|---|---:|
| Documents screened | 56 |
| Included unique cohorts | 38 |
| Excluded documents | 18 |
| Studies with an incidence component | 26 |
| Primary incidence rows (one per cohort) | 25 |
| All incidence rows, including secondary definitions | 32 |
| AMR rows | 71 |
| Studies contributing AMR rows | 25 |
| AMR-only studies | 12 |
| Outcome rows | 12 |
| Driver/risk-factor rows | 26 |
| Quality-assessment rows | 38 |
| Documented human-extraction discrepancies | 17 |
| Evidence items resolved verbatim | 169 |

Of the 38 cohorts, 32 are in East Africa, 3 in Central Africa and 3 in
Southern Africa (11 countries in total). Publication years range from
2013 to 2026.

## Review scope and eligibility

The review follows `amr-ssis-ecsa-files/amr-ssis-ecsa/protocol.docx`.

**Population.** Patients undergoing surgery in ECSA countries who are
followed for SSI, and patients with SSI from whom wound isolates were
cultured and tested for antimicrobial susceptibility.

**Included designs.** Prospective and retrospective cohorts,
cross-sectional studies, case-control studies, before-after/quality
improvement cohorts, and randomised trials.

**Excluded.** Reviews, editorials, protocols, duplicate or overlapping
reports, studies outside ECSA, wound infections that are not surgical
site infections, and multi-country pooled reports without ECSA-specific
strata.

**Screening.** All 56 documents in the supplied corpus were screened.
Thirty-eight unique cohorts were included; 18 documents were excluded
(duplicates, non-SSI wounds, non-ECSA settings, pooled analyses without
ECSA strata, or non-study documents). The full decision log is in
`output/screening.csv`.

This repository does **not** re-run database searches. It starts from the
56-document corpus supplied with the review and extracts from those
documents.

## Extraction rules

### Incidence

- The primary numerator is **clinical SSI**, using the CDC definition
  where the paper reports one.
- When both are reported, culture-confirmed SSI is extracted as a separate,
  secondary row and is never substituted for clinical SSI. If a study
  reports only culture-confirmed SSI, that is used as its primary
  estimate and flagged in the row notes.
- A study is included in SSI incidence only when it reports an explicit
  **operated-patient denominator**. Culture-positive series without an
  operated-patient denominator are retained for AMR prevalence only.
- Pre-intervention or control periods are the primary row for pre/post
  studies. Post-intervention results are recorded as secondary.
- Overlapping cohorts are merged at study level. Duplicate documents are
  excluded and cross-referenced to the included study.
- Studies whose reported percentages cannot be reconciled with their
  counts are retained with the counts and an explicit mismatch flag.
  They are not silently corrected.
- Numerators reconstructed from a percentage and denominator, and
  denominators reconstructed by summing table rows, are marked
  `derived`.

### AMR

- AMR prevalence uses an **SSI-patient or SSI-isolate denominator**.
- Priority outcomes are MRSA, third-generation cephalosporin
  resistance/ESBL and carbapenem resistance. MDR is recorded as a
  supplementary outcome.
- Species-level denominators are preferred. Aggregate "all isolates"
  percentages are recorded only as supplementary MDR rows and are not
  used to infer species-level prevalence.
- Phenotype and marker are recorded separately (for example cefoxitin
  versus oxacillin for MRSA, and ceftriaxone resistance versus a
  confirmed ESBL phenotype).
- Values calculated from reported percentages are marked `derived`.
  Arithmetic is always recomputed, and any mismatch greater than 0.5
  percentage points is flagged.

### Deduplication and overlap

Overlap groups are recorded in `output/studies.csv`:

- `gashaw-jimma`
- `worku-ethiopia`
- `gentilotti-dodoma`
- `clean-cut-ethiopia`
- `mbabazi-kawempe`

The Clean Cut group contains two distinct sequential cohorts, so both are
retained. Duplicate PDFs and overlapping molecular reports are excluded
and linked to the primary study in `output/screening.csv`.

## Evidence model

Every extracted outcome value (incidence, AMR, outcome, driver, quality
and discrepancy rows) links to an `evidence_id`. Each evidence item
stores:

- the source `doc_id`;
- the PDF page number;
- a verbatim supporting quote;
- a short description of what the quote supports.

Quotes are **never typed by hand** into the exported dataset. Each item is
defined by a start anchor, an optional end anchor, and a page number in
`scripts/records.py`. `scripts/evidence_lib.py` resolves the anchors
against the whitespace-normalised page text in `corpus/json/`, and
`scripts/check_evidence.py` asserts that every resolved quote is an exact
substring of its source page.

CSV files preserve the exact verbatim corpus slice, including PDF
ligatures and control bytes. The XLSX substitutes visible equivalents
(for example, the PDF decimal control byte becomes `.`) but does not
change the underlying CSV evidence.

## Reproducing the build

Requires `uv`. A local binary is available at `.tools/uv`.

```bash
# Recreate the environment from the lockfile
.tools/uv sync

# Check all evidence anchors resolve to verbatim page slices
.venv/bin/python scripts/check_evidence.py

# Validate counts, recompute percentages and write CSV/XLSX outputs
.venv/bin/python scripts/build_dataset.py
```

Environment:

- `uv` 0.13.4
- Python 3.12.15
- pandas 3.0.6
- openpyxl 3.1.5
- PyMuPDF, pdfplumber, python-docx, Pillow, pytesseract, RapidOCR

The build fails with a non-zero exit status if evidence does not resolve
or if a structural check fails (for example, a numerator larger than its
denominator, a missing evidence reference, or a study with no extracted
row).

Source extraction is handled by `scripts/extract_text.py`, which writes
one JSON file per document plus `corpus/manifest.csv` with source path,
document type, page count, SHA-256 and title.

## Output files

All files are written to `output/`.

| File | Contents |
|---|---|
| `studies.csv` | One row per included cohort: setting, design, population, SSI definition, overlap group, notes |
| `incidence.csv` | SSI numerators, denominators, reported and recalculated percentages, definition, primary/secondary status, evidence |
| `amr.csv` | Pathogen, resistance outcome, marker, counts, denominator source, derived flag, primary/secondary status, evidence |
| `outcomes.csv` | SSI consequences such as mortality, length of stay and cost |
| `drivers.csv` | Risk factors and reported effect estimates |
| `quality.csv` | Study-level quality assessment across selection, ascertainment and denominator domains |
| `discrepancies.csv` | Errors and omissions found in the human extraction |
| `screening.csv` | All 56 documents with include/exclude decision, reason and linked study |
| `evidence.csv` | Full evidence register with page numbers and verbatim quotes |
| `validation_report.txt` | Counts, percentage mismatches, derived values, priority AMR availability and structural errors |
| `extraction.xlsx` | Formatted workbook with one sheet per table plus a README sheet |

In the workbook, derived rows are highlighted pale yellow, arithmetic
mismatches orange, and secondary/non-primary rows are shown in grey
italics. Every data sheet has a frozen header and an auto-filter.

## Dataset summary

### Primary incidence

The 25 primary incidence cohorts report **2,077 SSI events among 13,233
operated patients**, a crude pooled proportion of **15.7%**. This is a
crude descriptive total, not a risk-adjusted or random-effects estimate.
One cohort (Boru 2025) reports only culture-confirmed SSI; it is retained
and flagged because no clinical count is available. Three primary
numerators (Forrester 2021 pre-intervention, Kefale 2020 and Scherbaum
2014) are reconstructed from reported percentages or table rows and are
marked `derived`.

Primary cohorts by country:

| Country | Cohorts | SSI events | Operated patients |
|---|---:|---:|---:|
| Ethiopia | 11 | 1,120 | 6,446 |
| Tanzania | 4 | 350 | 1,015 |
| Rwanda | 3 | 91 | 1,390 |
| Kenya | 2 | 67 | 564 |
| Democratic Republic of the Congo | 1 | 71 | 218 |
| Malawi | 1 | 20 | 208 |
| Gabon | 1 | 14 | 1,022 |
| Namibia | 1 | 126 | 1,248 |
| Zambia | 1 | 218 | 1,122 |

Two primary cohorts are in Central Africa (85 events / 1,240 patients)
and three are in Southern Africa (364 events / 2,578 patients).

### AMR availability

Primary AMR rows, by priority outcome:

| Outcome | Rows | Studies |
|---|---:|---:|
| MRSA | 16 | 16 |
| 3GC resistance / ESBL (E. coli, Klebsiella, Enterobacterales) | 27 | 15 |
| Carbapenem resistance (A. baumannii, P. aeruginosa, Enterobacterales) | 11 | 9 |
| MDR (supplementary, all rows) | 11 | 10 |

Sixteen AMR values are explicitly marked `derived`, mostly counts
reconstructed from a reported percentage and a table denominator.

## Key source decisions and inconsistencies

These are deliberate, documented decisions. The source counts are
retained; the dataset does not silently "fix" a paper.

- **Starr 2023** reports SSI of 23.2% before and 17.4% after an
  intervention, but the supplied denominators are 1,575 and 1,789. Those
  percentages imply approximately 365 and 311 events, not the extracted
  152 and 144. The counts are retained with a mismatch flag, and the study
  is excluded from primary incidence pooling.
- **Mukagendaneza 2019** reports 34/294 but prints 10.2% (the abstract
  also says 10.9%); the correct arithmetic is 11.56%. The counts are
  retained and flagged.
- **Worku 2023** Table 3 reports S. aureus cefoxitin resistance of 43%
  (66/153) and oxacillin resistance of 20% (31/153), while the text says
  cefoxitin 22.7%. The table value is used (66/153) and the text
  inconsistency is flagged. The human workbook used 40/163, which matches
  neither denominator.
- **Burugu 2022** reports MRSA as 9/16 (56.3%) in the table but
  62.5% (10/16) in the abstract. The table value is retained and flagged.
- **Mengesha 2014** reports overall MDR as 102/123 (82.9%), while the
  subgroup percentages imply 53/65 Gram-negative plus 48/58
  Gram-positive = 101/123. Both subgroup rows are marked derived and the
  one-isolate inconsistency is documented.
- **Amulioto 2020** states MRSA was 17/26 of "total Staphylococcus
  species". Table 2 shows S. aureus oxacillin resistance of 13/22
  (59.1%) and CoNS oxacillin resistance of 4/4. The MRSA row therefore
  uses **13/22 S. aureus**; the CoNS row is recorded separately as
  non-primary. Using 17/26 as an S. aureus denominator would be wrong.
- **Billoro 2019** tested only a small number of isolates and did not
  test for MRSA or confirm ESBL production. Its Table 4 still provides
  ceftriaxone results: E. coli 2/2 (100%) and Klebsiella 1/9 (11.1%).
  These are recorded as 3GC resistance, not as confirmed ESBL.
- **Misha 2021** Table 3 provides species-level ceftriaxone and
  meropenem results. E. coli ceftriaxone 5/9 (55.6%), Klebsiella
  ceftriaxone 3/5 (60%), P. aeruginosa meropenem 5/8 (62.5%) and E. coli
  meropenem 0/9 are recorded.
- **FALCON / Aaron Oladipo 2024** is a seven-country secondary analysis
  without ECSA-specific strata and is excluded.
- **De Nardo 2016** is an earlier report of the Gentilotti 2020
  pre-intervention cohort and is excluded as a duplicate.
- **Abosse 2021, Burugu 2022 and Mbabazi 2025** are AMR-only studies.
  Their culture-positive SSI series lack an operated-patient denominator
  and are not used for SSI incidence.

## Human extraction discrepancies

`output/discrepancies.csv` documents 17 issues in the pre-existing human
workbook. They fall into four groups:

1. **Wrong denominator or numerator.** Gashaw 2026 used 1,038
   (complete follow-up) instead of 1,205 (recruited); Ali 2023 used
   culture-confirmed 41/338 instead of clinical 49/338.
2. **Incidence/AMR category errors.** Abosse 2021, Burugu 2022 and
   Mbabazi 2025 were treated as incidence studies even though they have
   no operated-patient denominator.
3. **Duplicate or missing cohorts.** De Nardo 2016 duplicates the
   Gentilotti pre-intervention cohort; Scherbaum 2014 and Amulioto 2020
   were omitted entirely; Iyamba 2014, Manyahi 2014, Garoy 2021, Hope
   2019 and Wekesa 2020 were missing AMR data; Worku 2023 was present
   but its MRSA numerator and denominator were wrong.
4. **Transcription and naming issues.** Worku 2023 MRSA was recorded as
   40/163, Starr 2023 had blank incidence values, Burugu 2022 recorded
   both incompatible MRSA percentages, and Habtemariam 2023 was
   misspelled as "Habteweld".

The human workbook is retained as a comparator only. No human value was
copied into this extraction.

## Limitations

- This is an extraction dataset, not the final meta-analysis. The 15.7%
  crude proportion is unadjusted and does not account for clustering,
  case-mix, or between-study heterogeneity.
- Several studies report only aggregate resistance percentages. When a
  species-level denominator is unavailable, no species-level row is
  invented; the study may therefore contribute fewer AMR rows than its
  narrative suggests.
- Some AMR values are reconstructed from percentages and are marked
  `derived`. Rounding can move a reconstructed count by one isolate.
- Several source papers contain internal arithmetic inconsistencies.
  These are retained and flagged rather than corrected.
- The corpus contains OCR text with ligatures and control bytes. Evidence
  matching is performed on whitespace-normalised text; the CSV retains
  the exact slice, while the XLSX sanitises non-printable characters for
  display.
- Study quality is assessed at the row level in `quality.csv`; it is not
  a formal risk-of-bias score and is not yet weighted in a meta-analysis.
- The extraction should be reviewed by a second reviewer before the
  pooled analysis is finalised. In particular, the flagged mismatches,
  derived values, and the Amulioto/Worku/Burugu MRSA decisions should be
  checked against the source PDFs.

## Reviewer checklist

Before the pooled analysis is finalised, a second reviewer should:

1. Confirm the 25 primary incidence rows in `incidence.csv`
   (`include_primary == "yes"`) and the exclusion of Starr 2023 from
   primary pooling.
2. Check the three flagged arithmetic mismatches: Mukagendaneza 2019,
   Starr 2023 pre-intervention and Starr 2023 post-intervention.
3. Check the 16 derived AMR values and four derived incidence values
   listed in `validation_report.txt`.
4. Confirm the MRSA decisions for Amulioto 2020 (13/22 S. aureus),
   Worku 2023 (cefoxitin 66/153) and Burugu 2022 (table 9/16).
5. Review the 17 human-extraction discrepancies in
   `discrepancies.csv`, especially the incidence/AMR category errors.
6. Decide whether aggregate-only AMR studies such as Mukagendaneza 2019
   should be described narratively even though they contribute no
   species-level AMR row.

## Repository layout

```text
amr-ssi-v2/
├── README.md
├── pyproject.toml
├── uv.lock
├── amr-ssis-ecsa-files/        # supplied protocol, source PDFs and human workbook
├── corpus/
│   ├── json/                   # page-level text used for evidence resolution
│   ├── text/                   # plain-text extracts
│   ├── ocr_images/             # OCR pages for the scanned Kisibo 2017 PDF
│   └── manifest.csv            # provenance and SHA-256 for all 56 documents
├── scripts/
│   ├── extract_text.py         # PDF/DOCX to page-level JSON
│   ├── records.py              # curated studies, rows and evidence anchors
│   ├── evidence_lib.py         # verbatim quote resolver
│   ├── check_evidence.py       # evidence resolution check
│   └── build_dataset.py        # validation, CSV export and XLSX formatting
└── output/                     # generated CSV/XLSX deliverables
```
