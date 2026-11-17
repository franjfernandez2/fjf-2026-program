# Reading of the recovery report

Written by the assistant at the reviewer's request (2026-11-17), after the reviewer declared the analyses
final and ran `tools/recovery-report.sh`. It reads `recovery-report.md`; it changes no analysis.
**Simulated data · fictional compound.**

## What the comparison says

All six intervals hold the truth: the 20 mg effect of FJF-2026-301 under both estimands, and the effect of
each dose of FJF-2026-201 at week 12. The dose chosen for phase III (20 mg) is the true target dose.

| Quantity | Estimate | Truth | Error |
| --- | --- | --- | --- |
| 301, 20 mg, week 26, A2 (hypothetical) | -0.47 | -0.34 | -0.13 |
| 301, 20 mg, week 26, A1 (treatment policy) | -0.36 | -0.34 | -0.02 |
| 201, 5 mg, week 12 | -0.24 | -0.24 | 0.00 |
| 201, 10 mg | -0.26 | -0.28 | +0.02 |
| 201, 20 mg | -0.41 | -0.31 | -0.10 |
| 201, 40 mg | -0.33 | -0.33 | 0.00 |

## Reading

1. **The truth is smaller than the planning value.** The planned effect at the phase III dose was -0.50 and
   the true one is -0.34, inside the declared truth range (-0.80 to -0.30) but near its weak end. The study
   found a significant effect all the same: 142 patients per arm give power for -0.34 with a standard
   deviation near 0.9. This is what the truth range was built to allow, "as happens in real programmes".
2. **A2 over-estimates by 0.13, about 1.3 standard errors.** One study cannot tell chance from bias, and the
   interval holds the truth. There is no way to say from one draw whether the method is biased here; E2 (`studies/e2`) measures the bias of the
   MMRM and of the imputation methods under several dropout mechanisms, and this observation is one draw.
3. **A1 sits close to the truth, which should not be read as validation.** A treatment-policy estimand is
   expected to be diluted by discontinuation and rescue, so it should be smaller than -0.34 in magnitude; it
   is -0.36. Part of this is chance. A1 and A2 answer different questions and the "truth" reported is the
   hypothetical one (effect on treatment, no rescue), so the match is a coincidence of one sample.
4. **The dose-response is flatter than the fitted curve.** The true effects rise from -0.24 (5 mg) to -0.33
   (40 mg) and are monotone, with the curve nearly at its plateau by 20 mg. The quadratic model chosen by the
   study peaks at 20 mg and turns back at 40 mg (-0.30), which the truth does not do. The shape is a
   fitting artefact of 95 patients per arm, as the study report says it may be; the clinical statement
   "40 mg is not better than 20 mg" is true, "40 mg is worse" is not.
5. **The target dose is right, with luck.** 20 mg is the true target (effect -0.31, just past the 0.3
   relevance threshold) and 10 mg (-0.28) just misses. With the estimated 20 mg effect at -0.41, the margin
   was in the estimate's favour. A neighbouring dose was a real possibility, as the dry run warned (20 mg
   chosen 46% of the time).
6. **Six of six holding is expected, and not independent.** With a 95% interval, about one in twenty misses.
   The 201 intervals share a placebo arm, so they move together. This does not prove coverage; E2 measures it.

## What it does not say

- It does not say the analyses are correct: it says that this one draw of the data is consistent with the
  truth on the effects the analyses estimate. Safety quantities, the multiplicity decisions and the
  secondary endpoints are not compared.
- It is one seed. It is not a replacement for the simulation study (`studies/e2`).
- The target-dose truth uses the clinical relevance of the SAP (0.3) on the studied doses; the SAP also
  asks for 80% of the maximum effect, which 20 mg also meets.

## Consequence for the reports

The discussions (drafts in `docs/drafts/`) are unchanged: they were written without this comparison, and the
comparison does not contradict them. The study team may add a sentence on the comparison, marked as a
post-analysis check against the sealed truth (ADR 0018).
