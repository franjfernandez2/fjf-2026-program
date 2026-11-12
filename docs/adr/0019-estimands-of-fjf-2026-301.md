# ADR 0019: Estimands of FJF-2026-301

- **Status:** Accepted (reviewer's gate-2 review, 2026-11-12)
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

Two intercurrent events can change the meaning of a patient's HbA1c at week 26: starting rescue medication, and stopping study treatment. ICH E9(R1) asks the protocol to say, for each estimand, how each event is handled. The choice defines the question the study answers, before any method is chosen. The FDA's 2023 draft guidance asks for rescue to be documented as an intercurrent event and handled in the analysis.

## Decision

The estimands are written as data in [`fjf-2026-301-estimands.yml`](../protocols/fjf-2026-301-estimands.yml). The protocol and the SAP render them from that file, and `check_estimands()` requires all five attributes and a strategy for each event.

- **Primary (E1): treatment policy for both events.** The question is the effect of being assigned FJF-2026, whatever happens afterwards. Rescue and stopping treatment are part of what happens in practice. This is why the protocol keeps patients in the study after they stop treatment: their HbA1c is part of the variable.
- **Supplementary (E2): hypothetical for both events.** The question is the effect if no rescue were available and every patient took the drug as assigned. It describes the drug itself and is the estimand closest to older trials of the class, which excluded data after rescue.
- **Key secondary (E3 to E5):** HbA1c below 7.0% and body weight, with the strategies of E1. Time to rescue uses a composite strategy for rescue, because rescue is the event itself.

## Alternatives considered

- **Hypothetical as the primary.** It gives a larger, cleaner effect. But it answers a question about a world without rescue, which does not exist, and it needs untestable assumptions about the data it removes.
- **Hypothetical for rescue only, treatment policy for discontinuation** (the dossier's first sketch of E2). It mixes the two questions. Its natural analysis, a mixed model on data before rescue, would also include data after stopping treatment, which a hypothetical-for-rescue question does not need. Treating both events the same way gives a single, explainable question.
- **Composite for rescue in the primary** (counting rescue as failure). It turns a continuous endpoint into a mixed one and loses the size of the effect.

## Consequences

- The primary effect is diluted by rescue and discontinuation, so it is smaller than in older trials of the class. The sample size allows for this ([ADR 0020](0020-sample-size-by-simulation.md)).
- The primary analysis needs data after discontinuation and an assumption about the data still missing (jump to reference). The SAP sets both.
- E1 against E2 is the central explanation of the case study: the same data answer two different questions.
