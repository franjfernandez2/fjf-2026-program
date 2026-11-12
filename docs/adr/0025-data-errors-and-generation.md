# ADR 0025: Data errors in the raw data, and how the study data are generated and loaded

- **Status:** Proposed
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

The dossier asks for messy raw data, so that cleaning and mapping in F3 are real work. The raw layer refuses values outside a codelist ([ADR 0024](0024-raw-data-layer.md)). The study data must be generated once, from the sealed seed, after the SAPs are frozen ([ADR 0018](0018-sealed-truth-and-data-generating-model.md)).

## Decision

**Errors of the kinds an EDC lets through,** added by `add_data_errors()` with their own random stream:

- partial dates (day unknown, year only);
- visits outside their window, with their measurements;
- duplicated laboratory and vital-sign records;
- weight in pounds and height in metres;
- missing severity, and an adverse event that ends before it starts;
- laboratory values reported as below a limit (`<0.2`).

Rates are public and small (0.3% to 25%, by kind) and set in `simulator/R/errors.R`. The simulator keeps a log of the errors for its own tests. It is never written to disk: finding the errors is the work of F3, and the log is compared with what F3 found at the unblinding.

**Generation.** `tools/generate-study-data.sh` refuses to run unless:

- the tags `sap-301-v1.0` and `sap-201-v1.0` are in the history;
- the working tree is clean;
- no study data exist yet.

It mounts the seed file read-only into the container and generates both studies at the sizes of the pipeline (95 and 150 per arm). It writes `data/raw/<study>/<table>.csv` and a manifest with row counts and SHA-256 hashes. Only row counts are printed. The data are committed.

**Rehearsal.** The same R script with `--public-seed` writes into any directory except `data/raw`, so the whole path can be tested before the sealed run.

**Loading.** `tools/load-raw-data.R` checks every file against the manifest and the specification. It loads each study in one transaction as `fjf_loader`, so every row is in the audit trail. It refuses to load into a schema that already has data. This code is on the analysis side (`R/raw_load.R`) and reads files only.

## Alternatives considered

- **Errors that break the codelists** (misspelt codes, invalid arms). The database would refuse them at loading, as an EDC refuses them at entry, so they would never reach F3.
- **Writing the error log next to the data.** It would hand F3 the answers.
- **Generating in the pipeline.** CI has no seed, and the data must not change after they are generated.
- **Loading as the administrator.** Faster, but it would bypass the roles and the audit trail.

## Consequences

- F3 needs real data review: duplicates by natural key, date imputation, unit conversion, window checks and term coding.
- The rehearsal of 2026-11-12 (public seed 2026) generated 475 + 300 patients. It loaded 22 tables into a throwaway database, and the review query found 45 duplicated laboratory keys in 301.
- After the sealed run, the data are fixed. A defect found later is corrected by an audited update with a reason, not by generating again.
