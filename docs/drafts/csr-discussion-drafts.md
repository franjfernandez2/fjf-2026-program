# Drafts of the discussion and conclusions (FJF-2026-301 and FJF-2026-201)

**Draft written by the assistant for the reviewer to rewrite, approve or discard.** The discussion and the
conclusions of a study report are the study team's, that is the reviewer's (`R/csr.R`, `csr_discussion()`);
nothing here is in `studies/*/csr.qmd`. **Simulated data · fictional compound.** Every figure is copied from
the tables in `studies/<study>/output/tlf`, with the arms real since the unblinding. The sealed truth was
not used and the recovery report was not read. Nothing is post hoc unless marked.

## FJF-2026-301

**Primary.** At week 26, FJF-2026 20 mg added to metformin lowered HbA1c more than placebo under the
treatment-policy estimand: difference -0.36 points (95% CI -0.56 to -0.17; p=0.0003; T-14.2.1). H1 is rejected
under the multiplicity graph (adjusted one-sided p=0.0002; T-14.2.7).

**Robustness.** Missing at random gave -0.38 (-0.58, -0.18) and copy reference -0.37 (-0.57, -0.18) (T-14.2.3).
The hypothetical estimand, values on treatment and before rescue, gave -0.47 (-0.68, -0.27) (T-14.2.2). The
tipping point is 3.00 points of HbA1c added to the imputed values of the FJF-2026 arm, far outside any
plausible shift (T-14.2.3). The treatment-policy effect is smaller than the hypothetical one, as expected
when patients who stop or need rescue dilute it: 6.7% of the FJF-2026 arm and 11.3% of placebo had rescue (T-14.2.6)
, and the discontinuation figures are in T-14.1.1.

**Key secondary.** Not rejected: responders below 7% (H2: 19.6% vs 17.0%, adjusted p=0.2238), body weight
(H3, adjusted p=0.6529) and time to rescue (H4, hazard ratio 0.58, 95% CI 0.26 to 1.26, adjusted p=0.3366).
By the graph these cannot be called effects; the rescue direction is favourable but imprecise.

**Safety.** Any TEAE 72.0% vs 67.3%; serious TEAE 7.3% vs 4.7%; discontinuation for TEAE 6.0% vs 10.7%; no
fatal TEAE (T-14.3.1). Level 2 or 3 hypoglycaemia in 8 patients (5.3%) on FJF-2026 and 2 (1.3%) on placebo
(T-14.3.3): a signal to follow in the integrated analysis (T-ISS-15), with 8 and 2 events it is imprecise.

**Limits to state.** Simulated data from one seed. A single study of 300 patients; 142 per arm at week 26.
No comparison with the truth had been made when the analyses were finalised.

**Draft conclusion.** FJF-2026 20 mg lowered HbA1c versus placebo at week 26 robustly to the handling of
missing data, with no benefit shown on the key secondary endpoints and a numerical excess of level 2 or 3
hypoglycaemia.

## FJF-2026-201

**Dose-response.** A signal was found with all four candidate models (adjusted one-sided p between 0.0012
and 0.0167; T-14.2.2). The selected model is quadratic. Fitted effects against placebo at week 12: -0.14
(5 mg), -0.25 (10 mg), -0.38 (20 mg), -0.30 (40 mg). The MMRM differences are -0.33 (-0.58, -0.09) for the
highest dose and -0.41 (-0.66, -0.16) for 20 mg (T-14.2.1).

**Target dose.** 20 mg in the primary analysis, in the model-averaged analysis and in the analysis of all
observed values. The curve flattens and falls at 40 mg; with a 12-week study and about 95 patients per
arm the difference between 20 and 40 mg is not established, and the quadratic shape should not be read as
evidence that more drug is worse.

**Clinical utility index (exploratory).** 0.80 for 20 mg and 0.74 for 40 mg, with weights 0.7 and 0.3
(ADR 0031); it orders doses and does not measure benefit.

**Limits to state.** The study shows the effect better than the dose (the dry run chose 20 mg 46% of the
time). The compliance in the 201 ADaM is about 186% and is a derivation problem, not a result (ADR 0035).

**Draft conclusion.** Dose-response was demonstrated and 20 mg was selected for phase III.

## What the reviewer must decide

- Whether any of this sentence-level interpretation is the reviewer's. These are draft sentences.
- Anything beyond the plan is post hoc and must be labelled so; this draft adds only descriptive
  comparisons already in the tables.
