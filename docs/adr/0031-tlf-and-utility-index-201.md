# ADR 0031: Tables, figures and clinical utility index of FJF-2026-201

- **Status:** Accepted (reviewer, 2026-11-16)
- **Date:** 2026-11-13
- **Decided by:** proposed by the programmer

## Context

The SAP of FJF-2026-201 (tag `sap-201-v1.0`) names its analyses (A1, S1, S2, X1, SF1) but,
unlike FJF-2026-301, was frozen without a list of outputs. Its exploratory clinical utility
index (X1) fixes the components, the scaling to [0, 1] and the weights (0.7, 0.3), and leaves
the details open. Both are written now, on blinded data (dummy arms, ADR 0027), before the
database lock, so that nothing depends on the real results.

## Decision

- `docs/sap/fjf-2026-201-outputs.yml` lists 14 outputs: disposition and demographics, the
  HbA1c table by dose (A1), the MCP-Mod table (A1, S1, S2), the dose-response figure, the
  utility index (X1), and the safety outputs of FJF-2026-301 (SF1). `check_outputs()` now also
  reads the SAP's `primary` and `exploratory` items, so every item of the 201 SAP has an output.
- The patient and safety outputs reuse the 301 programs through one-line programs in
  `studies/201/tlf/` (ADR 0030); disposition drops the per-protocol row, as the 201 SAP has no
  per-protocol set. Tables carry their study in the header.
- X1 (`studies/201/analysis/cui.R`):
  - HbA1c effect: the fitted effect of the A1 selected model at each dose; with no selected
    model, the MMRM difference from placebo.
  - Safety: the share of the safety set, by dose taken, with a treatment-emergent adverse
    event whose action was "DRUG WITHDRAWN".
  - Scaling: linear over the study doses, 1 for the best and 0 for the worst dose; a component
    equal at every dose scores 1. Weights are read from the SAP text.
- The confidence intervals of the MMRM differences use the normal approximation, as MCP-Mod
  does with the same means and covariance.
- Second programming (`qc/201/tlf_qc.R`) recomputes counts, summaries, MMRM differences and
  the index, and reformats the MCP-Mod results.

## Consequences

- The outputs and the index are fixed before the lock; after it only the arms change.
- The index is relative to the doses studied: it ranks doses, it does not measure benefit.
- The outputs file is not part of the frozen SAP; changing it after the lock needs an ADR.

## Note on the lock of FJF-2026-201

`docs/lock/dblock-201.yml` (tag `dblock-201`, 2026-11-13) names ADR 0028 in its `decisions`
field: `tools/lock-database.sh` then wrote that ADR for every study. The decisions taken
before the lock of FJF-2026-201 are ADR 0030 and this ADR. The lock file is left as written;
the script now names each study's own ADRs.
