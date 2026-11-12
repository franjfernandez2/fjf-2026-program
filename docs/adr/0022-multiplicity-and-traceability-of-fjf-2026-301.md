# ADR 0022: Multiplicity, subgroups and output traceability of FJF-2026-301

- **Status:** Proposed
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

Study FJF-2026-301 has one primary and three key secondary endpoints: HbA1c below 7%, body weight and time to rescue ([ADR 0019](0019-estimands-of-fjf-2026-301.md)). A claim on a secondary endpoint is only credible if the chance of a false claim across all four is controlled. Every table of the study report must also be traceable to the plan, to the data it reads and to the program that makes it.

## Decision

**Graphical multiplicity procedure** (weighted Bonferroni, `graphicalMCP`), one-sided 2.5%:

- All the alpha starts on the primary hypothesis H1. Nothing else is tested unless H1 is rejected.
- H1 passes half its alpha to H2 (HbA1c below 7%) and half to H3 (body weight).
- H2 and H3 each pass half to the other and half to H4 (time to rescue); H4 passes back to H2 and H3 in halves.
- The graph lives in the SAP file; `check_multiplicity()` checks that it tests every key secondary analysis, that the initial weight is on the primary, and that its transition weights are valid.

**Subgroups** (screening HbA1c, age, sex, body mass index) are binary and descriptive: an estimate within each level and an arm-by-subgroup interaction, with no multiplicity adjustment and no claim.

**Outputs as data.** Every table and figure is listed in [`fjf-2026-301-outputs.yml`](../sap/fjf-2026-301-outputs.yml) with the SAP item it implements, the ADaM variables it reads and the program that will make it. `check_outputs()` fails if an analysis has no output or an output points to nothing in the SAP. The traceability matrix is built from this file.

## Alternatives considered

- **Fixed-sequence testing** (H1, then H2, then H3, then H4). Simple, but a failure early in the sequence blocks every later endpoint, even one with a clear effect.
- **Bonferroni across the secondaries after H1.** Controls the error rate, but wastes the alpha of hypotheses already rejected.
- **Graph with weights tuned by simulated power.** Possible with `graphicalMCP`, but the power of the secondaries depends on assumptions with weak sources (weight variance, correlation between endpoints). Equal halves are a defensible default; the reviewer may ask for tuning.
- **Shells in a word processor.** Common in industry, but they drift from the SAP. Here the shells and the plan are checked against each other on every run.

## Consequences

- A secondary claim requires H1 first, and the error rate across the four hypotheses stays at one-sided 2.5%.
- The dry run shows the graph on planning data. On the trial of the planned size all four hypotheses are rejected; that is one trial, not the power of the procedure.
- The ADaM variables named in the outputs are checked against the ADaM specification when it exists (F3); until then only their form and dataset are checked.
