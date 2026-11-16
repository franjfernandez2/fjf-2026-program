# ADR 0034: Plan of the integrated safety analysis, written before the unblinded results

- **Status:** Accepted (reviewer, 2026-11-16)
- **Date:** 2026-11-13
- **Decided by:** the reviewer (unblind through the documented process); plan proposed by the programmer

## Context

The reviewer authorised the unblinding of FJF-2026-301 and FJF-2026-201 (commit 77910f1,
`docs/unblind/`). The integrated safety analysis by treatment had no plan: the blinded pooled
review (ADR 0033) only pools over arms. A plan written after the unblinded results are read
could be shaped by them.

## Decision

- The plan (`docs/sap/fjf-2026-iss-sap.yml`) is written and committed while the unblinded
  pipeline is still running, before any result with the real arms is read. It is committed
  on its own branch, so its timestamp comes before the commit that carries the unblinded
  results.
- One placebo-controlled pool of both studies, by dose actually taken, with a FJF-2026 20 mg
  column (both studies) and an all-doses column.
- Exposure-adjusted incidence rates are the main measure, because the studies last 12 and 26
  weeks. FJF-2026 20 mg against placebo: study-stratified risk differences and, for level 2
  or 3 hypoglycaemia, a negative binomial rate ratio with study as a factor.
- Descriptive only: no multiplicity adjustment and no hypothesis test.

## Alternatives considered

- **Crude pooled percentages.** Simpson's paradox: the 20 mg column takes more of its
  patients from the longer study than the placebo column does.
- **Pooling only the 20 mg dose.** Loses the dose-response of safety that only FJF-2026-201
  can show.

## Consequences

- The reviewer reviews and accepts the plan before its programs are written; it can be
  frozen with a tag (`iss-sap-v1.0`), as the study SAPs were.
- Analyses that are not in this plan are labelled post hoc in the integrated report.

## Update, 2026-11-16

The reviewer accepted the plan without change, and it is frozen as version 1.0 with the tag
`iss-sap-v1.0`. Its content is the one committed in 164da63 on 2026-11-13, before the unblinded
results were read; only the version and status lines changed. The acceptance came after the
unblinding, so the evidence that the plan was not shaped by the results is the date of that
commit, kept on `main` by merging pull request 54 with a merge commit.
