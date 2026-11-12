# ADR 0021: Missing data in FJF-2026-301, and a dry run of the SAP on planning data

- **Status:** Proposed
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

The SAP must be frozen before any study data exist ([ADR 0018](0018-sealed-truth-and-data-generating-model.md)). A plan that has never been run can still fail: a model that does not converge, a setting the code ignores, a method that does not estimate what its estimand asks. In industry the answer is a dry run on dummy data before unblinding.

Two methods choices also need a record: how missing values are handled under each estimand, and which sensitivity analyses test those assumptions.

## Decision

**Missing data.**

- **E1, primary (treatment policy):** multiple imputation with `rbmi` (approximate Bayesian), 500 imputations, then an ANCOVA at week 26 on each dataset, combined by Rubin's rules. Values missing after a patient leaves the study are imputed by **jump to reference**. Values missing between observed ones are imputed under missing at random.
- **E2, supplementary (hypothetical):** a **mixed model for repeated measures** (`mmrm`, unstructured covariance, Kenward-Roger), on the values on treatment and before rescue.
- **Sensitivity for E1:** missing at random (S1), copy reference (S2) and a **tipping point** (S3): imputed values on FJF-2026 shifted by a delta, from 0 to 5 HbA1c points, until the upper 95% limit reaches 0.

**The settings live in the SAP file** ([`fjf-2026-301-sap.yml`](../sap/fjf-2026-301-sap.yml)), and the code (`R/efficacy_301.R`) reads them from there. A check (`check_sap()`) ties each analysis to an estimand of the study and the reference arm to the protocol's placebo arm.

**Dry run.** Before the freeze, every analysis runs on data generated from the **planning values** with a public seed (`R/dry_run_301.R`):

- one trial of the planned size, as the study report would show it;
- one large trial, to check that each analysis recovers the value of its own estimand;
- 200 trials of the planned size, analysed by the real primary analysis and by the simplified one that chose the sample size, to check the power that ADR 0020 assumed.

**The dry run does not breach the sealed truth.** Its generator lives on the analysis side, but it uses the public planning values only, as the sample size does. It does not read the seed or call the simulator. Its model is deliberately simple: no laboratory, no rescue rule driven by glucose, no data errors. Its data are labelled as planning data and are never study data.

## Alternatives considered

- **No dry run; trust the code's unit tests.** Unit tests check pieces, not whether the plan as a whole answers its questions.
- **A dry run on the simulator's data with a public seed.** Closer to the real data, but the analysis side would call the simulator, which ADR 0018 forbids. The simulator is also built after the SAP.
- **MMRM as the primary analysis.** Common in older diabetes trials, but under missing at random it answers the hypothetical question, not the treatment-policy one ([ADR 0019](0019-estimands-of-fjf-2026-301.md)).
- **Retrieved-dropout imputation** (imputing from patients who stopped treatment but stayed in the study). It fits treatment policy well, but needs enough such patients; with about 10% discontinuation and a 1:1 design, that is about 15 per arm. J2R is the established default; retrieved dropouts can be added as a sensitivity analysis in part 2 if the reviewer wants it.

## Consequences

- The plan is known to run, and to estimate what it says, before it is frozen.
- The SAP shows the dry-run numbers in a section marked as not study results.
- Each pipeline run spends a few minutes on the 200 dry-run trials. They run in parallel, and each trial has its own seed, so the result does not depend on the number of cores. The seed covers both rbmi steps: the draws of the imputation model and the imputation itself. Until PR 19 only the draws were seeded, so the imputation took whatever state the random number generator was in; a test now runs the same trials twice, from different states and with different numbers of cores, and requires identical results.
- With 200 trials, the share of successful trials is known only within about ±5 percentage points. The power implied by the mean and spread of the estimates is far more precise, and is what the comparison uses.
- First result (2026-11-12, confirmed after the seeding fix): the real primary analysis has about 90% implied power at 150 patients per arm, more than the simplified analysis on the same trials (about 85%), and its standard error matches the spread of its estimates. The sample size stands.
