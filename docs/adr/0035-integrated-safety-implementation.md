# ADR 0035: Implementation of the integrated safety analysis by treatment

- **Status:** Proposed (for the reviewer's review)
- **Date:** 2026-11-16
- **Decided by:** proposed by the programmer

## Context

The plan of the integrated safety analysis is frozen as version 1.0 (tag `iss-sap-v1.0`,
[ADR 0034](0034-integrated-safety-plan.md)). It names six analyses (I1 to I6), three measures
and the columns of one placebo-controlled pool. Like the SAP of FJF-2026-201
([ADR 0031](0031-tlf-and-utility-index-201.md)), it was frozen without a list of outputs, and
it leaves some details of the methods open. They are fixed here, and none of them adds an
analysis to the plan.

## Decision

- **Outputs.** `docs/sap/fjf-2026-iss-treatment-outputs.yml` lists 13 outputs, numbered after
  the blinded pooled review (T-ISS-1 to T-ISS-7, [ADR 0033](0033-blinded-pooled-safety-review.md)):

  | Analysis | Outputs |
  | --- | --- |
  | I1 Overview of adverse events | T-ISS-9 (by column), T-ISS-10 (20 mg against placebo) |
  | I2 By system organ class and preferred term | T-ISS-11 (patients), T-ISS-12 (events), T-ISS-13 (common terms, 20 mg against placebo) |
  | I3 Hypoglycaemia | T-ISS-14 (by level), T-ISS-15 (negative binomial model) |
  | I4 Dose-response of safety | T-ISS-16, F-ISS-1 |
  | I5 Liver safety | T-ISS-17, L-ISS-1, F-ISS-2 |
  | I6 Exposure | T-ISS-8 |

  `check_iss_outputs()` fails if an analysis of the plan has no output or an output names no
  analysis.
- **Pooling.** The ADaM datasets of the two studies are stacked, not derived again. A patient
  is in the column of the dose actually taken (TRT01A); a patient on FJF-2026 is also in the
  all-doses column. A dose with no column in the plan stops the pipeline.
- **Programs.** Exposure (T-ISS-8) and hypoglycaemia by level (T-ISS-14) are the
  FJF-2026-301 programs, unchanged, run on the pooled datasets. The others are in
  `studies/iss/tlf/iss_treatment.R`, with the layout helpers of FJF-2026-301.
- **Exposure-adjusted incidence rate (M2).** As the plan writes it: patients with at least one
  event over the total exposure of the column, per 100 patient-years. The denominator is the
  whole time on study drug, not the time to the first event.
- **Risk difference stratified by study.** The Mantel-Haenszel estimate (weights
  n1 n0 / (n1 + n0)) with the variance of Sato (1989) and a 95% Wald interval, in percentage
  points. Each study and the crude pooled percentages are printed next to it.
- **Common adverse events (I2).** A preferred term is shown when its incidence, not rounded,
  is 2% or more in any FJF-2026 column, the all-doses column included.
- **Negative binomial model (I3).** The level 2 or 3 episodes on treatment of each patient of
  FJF-2026 20 mg and placebo; log link, treatment and study as factors, log of the exposure in
  years as offset (`MASS::glm.nb`); 95% Wald interval on the log scale. With no episode in one
  arm the ratio is not estimable and is printed as "-". A warning of the fit is printed as a
  footnote.
- **Liver safety (I5).** Peaks are the highest value on treatment as a multiple of the upper
  limit of the reference range (ADLB.ANRHI). The aminotransferase peak is the larger of the
  ALT and AST peaks. As in the plan, the two peaks need not be on the same day.
- **Dose-response (I4).** FJF-2026-201 only, percentages by dose joined by lines; no model.
- **Numbers.** An estimate that rounds to zero is printed without a sign.
- **Blinding.** The outputs go to `studies/iss/output/treatment`. Until both studies are
  unblinded ([ADR 0032](0032-locking-does-not-unblind.md)) the arms are the dummy assignment
  and every output carries the blinding note.
- **Second programming** (`qc/iss/tlf_qc.R`). It does not use the pooled datasets: it starts
  from the ADaM datasets of each study and finds the columns from ADSL. The risk difference is
  written from the cells of the 2x2 tables, and the negative binomial model is fitted by
  maximising its likelihood directly, with the standard error from the expected information.
  `tlf_iss_treatment_qc_check` stops the pipeline on any difference.

## Alternatives considered

- **Time at risk to the first event as the denominator of M2.** The more usual definition of
  an exposure-adjusted incidence rate, but not what the frozen plan says. It would be a
  post hoc analysis.
- **A fixed-weights variance for the risk difference**, as the `tern` package uses. Simpler,
  but it ignores that the weights depend on the data. The point estimate is the same, and a
  test checks ours against `tern`.
- **One wide table per analysis** with every measure. Too wide to read with six columns; the
  measures are split over tables instead.

## Consequences

- No analysis is added to the plan, so no output is labelled post hoc.
- The confidence intervals are descriptive: 13 outputs and many rows, with no adjustment.
- Both programs have the same author, as in the studies; the direct likelihood and the check
  against `tern` reduce what the two can share.

## Findings while programming

- **Few level 2 or 3 episodes.** With the study data, `MASS::glm.nb` reaches its iteration
  limit for the dispersion: the episodes are too few to show overdispersion, and the model is
  close to a Poisson model. The rate ratio and its interval are stable, and the second
  programming, which maximises the likelihood itself, prints the same cell. The table says so
  in a footnote.
- **Compliance of FJF-2026-201.** T-ISS-8 shows compliance near 186% in the columns that come
  from FJF-2026-201. It is not an error of the pooling: the table of the study itself
  (T-14.3.6 of FJF-2026-201) shows the same. FJF-2026-201 is double-dummy, two tablets a day,
  and its ADSL uses the FJF-2026-301 derivation ([ADR 0030](0030-study-201-reuses-301-programs.md)),
  which counts one tablet a day. The study is locked and unblinded, so the correction is the
  reviewer's decision and needs its own record; nothing is changed here. It does not depend on
  the treatment arm, and no efficacy or safety result uses compliance in FJF-2026-201.
