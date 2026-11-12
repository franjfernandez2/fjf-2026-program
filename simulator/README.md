# fjfsim: the data simulator of the FJF-2026 programme

> **Simulated data · fictional compound.** FJF-2026 does not exist.

`fjfsim` generates the raw, EDC-like data of studies FJF-2026-201 and FJF-2026-301. It reads only the public design files: the [assumptions](../docs/design/assumptions.yml), the [protocols](../docs/protocols/) and the [raw-layer specification](../docs/specs/raw-layer.yml). It writes exactly the tables and columns of that specification.

The analysis code (`R/`, `studies/`, `qc/`) never loads this package, and a test checks it ([ADR 0018](../docs/adr/0018-sealed-truth-and-data-generating-model.md)).

## The truth

Every parameter of the model has a public planning value and a public truth range. `draw_truth()` draws the true value of each one, uniformly inside its range, from a seed:

- with a **public seed**, it gives a test truth;
- with the **sealed seed** (`read_sealed_seed()`), it gives the sealed truth.

Truth objects and seed objects print as `<truth: 19 parameters, not shown>` and `<sealed seed>`. The truth is never written to disk before the unblinding.

## The model

For patient *i* at actual week *t*, the HbA1c change from baseline is:

```
placebo · B(t)
+ E(dose) · B(min(t, stop)) · fade(t − stop)
+ rescue benefit · min(1, (t − rescue) / 12)
+ shared residual of the patient + visit residual
```

- **E(dose)** is an Emax curve. ED50 follows from the share of the 20 mg effect that 5 mg gives, and Emax from the 20 mg effect at week 26 (`true_curve()`).
- **B(t)** is the build-up of an effect: (1 − e^(−t/τ)) / (1 − e^(−26/τ)). τ is chosen so that the effect at week 12 is the assumed share of the week-26 effect.
- **After stopping treatment**, the drug effect fades linearly over 12 weeks.
- **After rescue**, the rescue benefit builds up over 12 weeks.
- **Residuals** are exchangeable: a shared part and a visit part, with the assumed correlation between visits.

**Weight** follows the same structure, with its own parameters and no rescue effect.

**Fasting glucose** comes from HbA1c through an ADAG-like line and is consistent with the rescue rule:

- at the visit that leads to rescue, it is above the protocol threshold, and an unscheduled visit confirms it;
- at every other visit under the rule, it is below the threshold.

**Intercurrent events** are drawn at the assumed rates, with a constant hazard for the 12-week study:

- **Discontinuation of treatment:** the patient stays in the study.
- **Rescue:** the patient starts a rescue medication.
- **Withdrawal from the study:** nothing is recorded afterwards. A share of withdrawals is missing not at random, chosen more often among the poorer responders.

**Safety data** are drawn after all the efficacy data, so adding them did not change any efficacy value:

- **Adverse events:** background events at the placebo rate, times the relative risk on the drug. On top of that, an early gastrointestinal excess proportional to the dose, serious events, and the event behind each discontinuation for an adverse event, with the drug withdrawn.
- **Terms are verbatim:** "Headache", "HEADACHE", "Tension headache". They are coded in F3.
- **Hypoglycaemia:** levels 1 to 3 from the glucose meter, more frequent after rescue with a sulfonylurea or insulin. Level 2 and 3 episodes are also reported as adverse events.
- **Safety laboratory:** ALT, AST, bilirubin and creatinine, log-normal with a patient and a visit part. eGFR comes from creatinine by CKD-EPI 2021. Rare hepatic elevations above 3 × ULN are proportional to the dose.

Operational settings (sites, recruitment, characteristics that no outcome depends on) are public judgements in `R/settings.R`.

## Use

```r
pkgload::load_all("simulator")
design <- read_design(".")
truth <- planning_truth(design$assumptions)           # or draw_truth(design$assumptions, seed)
tables <- simulate_study(design, truth, "301", 150, seed = 1)
```

The study data are generated once, after the SAPs are frozen, by the generation command of PR 24.

## Tests

The tests use public seeds and the planning values as truth (`simulator/tests/`). With thousands of patients, the simulator must give back:

- the week-26 effect and the placebo change of 301, and the week-12 dose-response of 201;
- the rates of intercurrent events;
- the missing-not-at-random selection;
- the rescue rule;
- the safety rates and the link between discontinuations and adverse events.

They run in their own environment, apart from the analysis tests.
