# Work packages still open

Each package is one branch, one pull request and one fresh session. Give the assistant `CLAUDE.md` and exactly one brief. Packages that edit `_targets.R` run one at a time. The reviewer's own actions are listed at the end: they are not packages.

Brief template: goal; read first; touch; do not touch; done when; check with; stop and ask if.

## P1 · Traceability matrix of the studies

- **Goal:** extend `docs/traceability-matrix.csv` (today only the benchmark) with every output of 301, 201 and the integrated safety analysis, generated from the outputs specifications, with `qc_status` from the second-programming comparisons.
- **Read first:** `docs/traceability.md`, `R/outputs.R` (`traceability_301()`), `docs/sap/*-outputs.yml`.
- **Touch:** `R/outputs.R`, a target in `_targets.R`, `docs/traceability*`, tests.
- **Do not touch:** the outputs specifications, the QC programs.
- **Done when:** one row per output; a test fails if an output has no row or a `match` without a clean comparison.
- **Stop and ask if:** a table has no second programming (state it, do not mark it `match`).

## P2 · Extension E2: simulation study of MMRM and reference-based imputation under MNAR

- **Goal:** bias, coverage and power of the treatment-policy analysis (rbmi, jump to reference) and the MMRM under missing-not-at-random dropout, on data generated from the **public planning values**, with public seeds (never the sealed truth, never `simulator/`).
- **Read first:** ADR 0019 to 0021, `R/dry_run_301.R`, `R/efficacy_301.R`, `R/sample_size.R`, `docs/sap/fjf-2026-301-sap.yml`.
- **Touch:** `R/e2_*.R`, `studies/e2/`, a plan ADR before results, `_targets.R`, tests.
- **Done when:** a report with scenarios, estimands, Monte Carlo errors; plan written and committed before the results; tests.
- **Stop and ask if:** a scenario needs a truth value that is not in the public assumptions.

## P3 · Integrated safety report

- **Goal:** a Quarto report of the integrated safety analysis by treatment (tables, listing, figures of `studies/iss/output/treatment`), labelled, with the plan's principles and the post hoc rule. Draft of the discussion is marked as a draft for the reviewer.
- **Read first:** ADR 0034 and 0035, `docs/sap/fjf-2026-iss-sap.yml`, `studies/301/csr.qmd`, `R/csr.R`.
- **Touch:** `studies/iss/report.qmd`, `R/csr.R` helpers, `_targets.R`.
- **Do not touch:** the plan, the tables' programs.

## P4 · Interactive explorer (app/)

- **Goal:** a Shiny app on aggregate, labelled results of both studies (efficacy by dose and estimand, safety by treatment), exportable as a static site. Only aggregate data, never patient rows.
- **Read first:** `README.md` (app section), `studies/*/output/tlf`, ADR 0014.
- **Touch:** `app/`, a data-export target, tests.

## P5 · Web case pages (reviewer's site, outside this repository)

- Bilingual (es, en) pages for the benchmark and the three cases in `~/Desktop/web&portfolio`. A case is marked published only when its criteria hold (CI from scratch green, double programming matching, validator issues explained, simulated-data label, the reviewer can explain every decision). Needs the reviewer's text approval.

## P6 · Extension E1: SAS replication of the primary analysis

- Blocked: needs the reviewer's SAS OnDemand for Academics account (ADR 0002). Brief once available: replicate T-14.2.1 in SAS from the ADaM XPT files and compare.

## The reviewer's own actions (not packages)

1. Approve in writing the pull requests of F3 and F4 and answer the control questions in `docs/notes/F3-*` and `F4-*`.
2. Decide the 201 compliance finding (ADR 0035).
3. Declare the analyses final and open the sealed truth; then a package writes the recovery report from his output.
4. Write the CSR discussions (drafts may be prepared as drafts).
5. Tag `gate-3` and `gate-4`; choose the licence; publish at gate 6 (repository public, website, app).
