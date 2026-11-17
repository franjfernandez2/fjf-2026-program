# Recovery report: estimates against the sealed truth

**Simulated data · fictional compound.** Produced by `tools/recovery-report.R` by the reviewer, after the analyses were final.

| Quantity | Estimate | 95% interval | Truth | Interval holds the truth | Note |
| --- | --- | --- | --- | --- | --- |
| 301 A2, 20 mg vs placebo, week 26 | -0.47 | (-0.68, -0.27) | -0.34 | yes | hypothetical estimand: the matching truth |
| 301 A1, 20 mg vs placebo, week 26 | -0.36 | (-0.56, -0.17) | -0.34 | yes | treatment-policy estimand: expected to differ |
| 201 5 mg vs placebo, week 12 | -0.24 | (-0.49, 0.00) | -0.24 | yes | MMRM, hypothetical estimand |
| 201 10 mg vs placebo, week 12 | -0.26 | (-0.51, -0.01) | -0.28 | yes | MMRM, hypothetical estimand |
| 201 20 mg vs placebo, week 12 | -0.41 | (-0.66, -0.16) | -0.31 | yes | MMRM, hypothetical estimand |
| 201 40 mg vs placebo, week 12 | -0.33 | (-0.58, -0.09) | -0.33 | yes | MMRM, hypothetical estimand |

Target dose of FJF-2026-201: chosen 20 mg, true 20 mg (smallest studied dose whose true effect is at least 0.3 points better than placebo).

Differences are in points of HbA1c (negative is better). One study gives one interval each: a miss is
expected about one time in twenty and is not by itself a flaw; the E2 study (`studies/e2`) measures how
often each method is expected to hold. Nothing here changes a frozen file or a tagged analysis.
