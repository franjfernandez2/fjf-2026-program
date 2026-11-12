# ADR 0020: Sample size of FJF-2026-301 by simulation

- **Status:** Proposed
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

The primary estimand is treatment policy ([ADR 0019](0019-estimands-of-fjf-2026-301.md)). Under it, the effect is diluted by patients who stop FJF-2026 and by rescue, which helps placebo patients more. Missing values are imputed by jump to reference, which shrinks the effect further. No closed formula covers all of this, so the sample size is found by simulation.

## Decision

- **Planning model** (`R/sample_size.R`). Each simulated patient's week-26 change in HbA1c is built from:
  - the placebo change plus noise;
  - the drug effect, if the patient is on FJF-2026 and has not stopped it;
  - the rescue benefit, if the patient is rescued.

  Patients who leave the study have no week-26 value. It is imputed from the observed placebo values, as jump to reference does, and the arms are compared by a two-sample test.
- **Inputs:** the planning values in the [design assumptions](../design/assumptions.qmd) only. The sample size never uses the simulator or the sealed truth.
- **Target:** 90% power, two-sided alpha of 5%.
- **Simulation:** 5,000 trials per sample size, from 100 to 220 patients per arm in steps of 10, with a public seed (20261112), as the target `sample_size_301_power`.
- **Rule:** the smallest sample size whose lower 95% Monte Carlo limit of power reaches 90%. A lucky simulation therefore cannot choose too small a study.
- **Robustness:** at the chosen size, power is recomputed with single planning values moved to the edge of their range.

## Alternatives considered

- **The formula for two means**, with the effect diluted by hand. It is quick and gives a similar answer, but it hides the imputation and the extra variance from the mixture of patients.
- **Simulating the full primary analysis** (multiple imputation with `rbmi`) for every trial. It is the most faithful, but thousands of trials times the imputations make it too slow for each pipeline run. The simplified analysis has the same expected difference. The SAP's analysis is checked on simulated data in the SAP's own PR.
- **Inflating a formula's result by the dropout rate.** Common, but it treats missing data as if they simply vanished, whereas under treatment policy they are imputed and dilute the effect.

## Consequences

- The chosen size is read from the pipeline, not typed in. The protocol and the SAP show the power curve, its Monte Carlo error and the robustness table.
- The result: 150 patients per arm, 300 in all. Under the planning values the expected treatment-policy difference is about −0.39, against −0.50 on treatment.
- The study is not powered for a true effect at the low end of its range (−0.30 on treatment). That is a deliberate choice: the planning value is already below the class, and planning for the worst case would double the study.
