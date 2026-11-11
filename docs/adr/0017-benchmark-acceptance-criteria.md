# ADR 0017: Acceptance criteria of the benchmark (gate 1)

- **Status:** Accepted
- **Date:** 2026-11-11 (accepted 2026-11-12)
- **Decided by:** Francisco Fernández, at the gate 1 review

## Context

The benchmark (phase F1) compares the pipeline with the CDISC pilot, a study whose answer is published. The phase plan sets eight criteria for gate 1, and its main risk was already known: the pilot may not be fully reproducible from its own SDTM. Its ADaM datasets and study report were made over fifteen years ago with SAS programs on legacy data.

That is what happened. Three of the four ADaM datasets and two of the four outputs differ from the pilot. Each difference has a cause, and in every case the cause lies in the pilot:

- a subject counted in a group the data do not support;
- p-values rounded twice;
- LOCF that overwrites a second assessment in the same window;
- specification text with typing errors.

So the plan's words "matches" and "passes" need a precise meaning before the gate is judged. Criterion 5 needs one more decision: CDISC CORE could not check ADaM data (see [`conformance.md`](../../studies/benchmark/conformance.md)).

## Decision

**What "matches" means.**

- **ADaM datasets** are compared with the official ones by key, with diffdf. Text must be identical, and numbers must agree within 1e-8.
- **Outputs** are compared with the study report at printed precision. Our number, rounded as SAS rounds (halves away from zero), must print as the report prints it.
- **A figure printed as an image** (Figure 14-1) cannot be compared number by number. It counts as matching when its analysis dataset matches the official one exactly and a review of the two pages, side by side, finds no difference. The numbers in the figure's box are checked by hand.

**When a difference counts as explained.** A difference does not stop the gate when all of the following hold:

1. It has a row in the discrepancy log with its evidence, cause and decision, and status `explained`.
2. The cause has been established, not guessed. Where the cause can be reproduced, a test or a target does so: for example, rounding twice gives the printed p-values (D-012), and fitting without baseline gives the other p-value (D-013).
3. The fix was never to copy an official or printed value into our data. Our derivation follows the specification and the SAP; where they disagree with the pilot's files, the log says which one was followed and why.
4. The pipeline links the finding to that row. The target `benchmark_findings` collects every finding of every comparison and links it to the log by comparison, dataset and variable. A finding left without a link is unexplained, and a test fails.

**What has no exceptions.** The independent ADSL must match the production ADSL with no difference at all. Two programs that disagree mean at least one of them is wrong; there is nothing to explain away.

**Criterion 5, conformance: a decision for the reviewer.** CDISC CORE v1.0.0 has no published ADaM rules, and its draft ADaM rules do not run. Three ways forward:

- **A. Pinnacle 21 Community (recommended).** This is the fallback the phase plan already names. The reviewer installs it on their Mac, if it is still available, and runs it on the four XPT files with ADaMIG 1.0. Its findings are recorded and explained like any other, and the gate waits for that run.
- **B. Substitute evidence.** Accept, for F1 only, the checks that already run: each dataset against the `define.xml`, the XPT files written from the specification, and the read-back comparison with the official XPT files. CORE is run again when CDISC publishes ADaM rules, and criterion 5 is recorded as replaced, not as passed.
- **C. Postpone gate 1** until CORE can check ADaM. The date is unknown, and phase F2 would wait for it.

**Chosen: A** (the reviewer, 2026-11-11). Pinnacle 21 Community is run on our four XPT files and, for comparison, on the four official ones, with ADaMIG 1.0. An issue reported for both is a property of the pilot. An issue reported only for ours is ours to fix or to explain. Every issue goes to the discrepancy log like any other finding, and gate 1 waits for that run. CORE is run again when CDISC publishes ADaM rules.

## Alternatives considered

- **Require every number to match the pilot exactly.** It cannot be met without copying the pilot's errors, and copying them is what the benchmark must never do. A benchmark that matches by hiding its differences proves less than one that explains them.
- **Accept differences below some size**, for example a third-decimal difference in a p-value. That hides a difference instead of explaining it. D-012 looked like such a case, and its cause turned out to be a specific rounding error in the report.
- **Judge the outputs on the analysis datasets only.** It would skip the check that matters most: whether the statistics and the formats come out as published.

## Consequences

- Gate 1 is judged by running the pipeline and its tests, not by reading the log: the benchmark report shows every finding with the row that explains it.
- Every explained difference is a documented error or ambiguity of the pilot. The log is part of the benchmark's result.
- Criterion 4 is met under this ADR.
- Criterion 5 was met on 2026-11-11 by the Pinnacle 21 run (option A). Our files and the official ones give identical reports. Every issue is caused by the pilot's define.xml and explained (D-014 to D-018); see [`conformance.md`](../../studies/benchmark/conformance.md) for the tool's caveats.
- Phases F3 and F4 reuse the same rules: double programming with no exceptions, every difference logged, explained and linked.
