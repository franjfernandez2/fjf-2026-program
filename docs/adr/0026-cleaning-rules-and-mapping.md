# ADR 0026: Cleaning rules, and where cleaning happens

- **Status:** Accepted (reviewer's clinical review, 2026-11-12)
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

The data-quality report (PR 28) found, in both studies, what an EDC lets through: duplicated records, partial dates, visits outside their window, weights in pounds, heights in metres, qualified laboratory results, adverse events that end before they start and missing severities. F3 maps the raw layer to SDTM and then to ADaM ([ADR 0024](0024-raw-data-layer.md)). In a real trial each finding would become a query to the site. Here there is no site, so each kind of finding needs a rule decided before the data are mapped.

## Decision

- **The raw layer is never changed.** It stays as collected, with its audit trail. Cleaning happens when the data are mapped to SDTM, or derived into ADaM.
- **One rule per kind of finding,** in [`docs/specs/cleaning-rules.yml`](../specs/cleaning-rules.yml), each with its reason and the layer where it acts. A test fails if a check of the data-quality report has no rule.
- **SDTM keeps what was collected.** Partial dates stay partial (ISO 8601 truncated), and collected values and units stay in the `--ORRES` variables. Standardised values (`--STRESN`) are converted, never retyped.
- **Imputation belongs to ADaM,** always with its flag: partial adverse event and medication start dates, birth and diagnosis dates, and missing severity (counted as severe).
- **Analysis visits come from the SAP's windows,** so no measurement is dropped for being out of window.
- **Duplicates keep the first entry.** When duplicates differ, the first record entered is kept and the difference is listed. A rule that picked a value could pick by the result.
- **Every finding a rule touches is listed** in a data-issues listing, so the reviewer sees each one.
- **The rules are written blind.** The data-quality report does not use the treatment arm, and these rules were written before any result by arm exists (ADR 0027, F3 day 2).

## Alternatives considered

- **Correcting the raw tables.** It would break the audit trail and the comparison with the simulator's error log at the unblinding.
- **Dropping problem records.** Simple, but it loses data and can bias the analysis, for example when late visits are the ones dropped.
- **Imputing in SDTM.** SDTM records what was collected; imputation is an analysis decision, and ADaM flags it.
- **Choosing among differing duplicates by plausibility.** It needs judgment record by record, which cannot be blinded or reproduced.

## Consequences

- SDTM programs read the rules file, so a rule changes in one place.
- Pinnacle 21 may flag partial dates and missing values in SDTM. They are expected and explained in the reviewer's guide (cSDRG).
- At the unblinding, the simulator's error log is rebuilt from the seed and compared with what these rules touched. An error the checks missed is reported, not hidden.
