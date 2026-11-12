# ADR 0023: MCP-Mod design and sample size of FJF-2026-201

- **Status:** Accepted (reviewer's gate-2 review, 2026-11-12)
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

Study FJF-2026-201 compares placebo with FJF-2026 5, 10, 20 and 40 mg for 12 weeks. It has to show that the drug lowers HbA1c, describe how the effect changes with the dose and recommend a dose for phase III. The shape of the dose-response is unknown before the study. The phase III study was built first, on 20 mg ([ADR 0005](0005-build-order.md)), and both studies are simulated from the same sealed curve ([ADR 0018](0018-sealed-truth-and-data-generating-model.md)).

## Decision

**Estimand.** Hypothetical for both rescue and stopping treatment: the effect of each dose if taken for 12 weeks without rescue. A dose-finding study asks what the dose does, not what happens after assigning it.

**Analysis: generalised MCP-Mod** (Pinheiro and colleagues, 2014; `DoseFinding`).

- A mixed model for repeated measures on the values on treatment and before rescue gives the mean change by dose at week 12 and its covariance matrix.
- MCP step: optimal contrast tests for four candidate models (linear; Emax, ED50 6 mg; sigmoid Emax, ED50 10 mg and Hill 3; quadratic, an umbrella). One-sided alpha 2.5%, adjusted across the four.
- Mod step: among the significant models, the one with the smallest generalised AIC estimates the curve.
- Target dose: the smallest dose in the study with at least 80% of the estimated maximum effect and at least 0.3 points better than placebo.
- Sensitivity: model averaging with bootstrap; the analysis on all observed values.

**Planning curve.** An Emax curve whose ED50 (5.7 mg) follows from the assumption that 5 mg gives 60% of the effect of 20 mg, scaled to 12 weeks by the share of the 26-week effect reached at week 12 (85%, a judgement, range 75% to 95%). On it the rule chooses 20 mg.

**Sample size: 95 per arm, 475 in all.** It is the smallest size on a 5-patient grid whose power for the MCP step is at least 90% under every candidate model and the planning curve (lowest: 90.4%, the planning curve). Patients with no week-12 value under the hypothetical estimand (about 13%) are taken off the effective size.

## Alternatives considered

- **Pairwise comparisons with placebo, Dunnett-adjusted.** Simple, but it spends the alpha on four tests, ignores the ordering of the doses and does not estimate a curve.
- **One assumed Emax model.** More power if right, but a wrong shape gives a wrong target dose and no warning.
- **Power target of 80%.** About 70 per arm. The signal would be missed one time in five; the study is the basis for choosing the phase III dose, so 90% is used.
- **ED90 or a utility-based dose as the rule.** ED90 sits on the flat part of an Emax curve and is poorly estimated. A utility needs the safety data; it is kept as an exploratory analysis (X1).

## Consequences

- The dry run of 300 trials at 95 per arm finds a signal in 93% of them, consistent with the planned power once the MMRM first stage is included.
- The dose is less certain than the signal: the rule picks 20 mg in 46% of trials, 10 mg in 18%, 40 mg in 18%, 5 mg in 11%, and no dose in 7%. On a curve that flattens early, neighbouring doses differ by less than their standard errors. This is stated in the SAP rather than hidden behind the power.
- The dose that 201 recommends is compared with the 20 mg of 301 at the unblinding. A disagreement is reported as a finding.
