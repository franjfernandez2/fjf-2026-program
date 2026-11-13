# ADR 0033: Blinded pooled safety review of FJF-2026-201 and FJF-2026-301

- **Status:** Proposed (for the reviewer's review)
- **Date:** 2026-11-13
- **Decided by:** the reviewer asked for the pending work that keeps the studies blinded; content proposed by the programmer

## Context

The programme plans an integrated safety analysis of the two studies (ADR 0005). Its main
question, the safety of FJF-2026 against placebo, needs the real treatment arms, and both
studies stay blinded (ADR 0032). Pooled data that do not separate the arms are still useful
before the unblinding: they show the size and make-up of the safety database, the exposure,
the event rates in the population as a whole and the work of pooling two studies of different
length. Sponsors review such aggregate blinded data during development.

## Decision

- A blinded pooled review (`docs/sap/fjf-2026-iss-outputs.yml`, `studies/iss/tlf/iss.R`)
  pools the safety datasets of FJF-2026-201 and FJF-2026-301 **over their arms**. Its columns
  are the two studies and all patients; no column is a treatment arm, and every arm, placebo
  included, is counted.
- The tables are the FJF-2026-301 safety programs (demography, exposure, adverse events,
  hypoglycaemia, laboratory shifts, vital signs), run on the stacked datasets with the
  treatment variables replaced by the study. No new derivation is written.
- Because the treatment variables are overwritten, the tables are the same whichever
  assignment, dummy or real, the study datasets use. They carry a footnote saying that no
  result is by treatment, and not the dummy-arm note.
- The integrated safety analysis by treatment (pooled FJF-2026 against pooled placebo, by
  dose) is not written now. It needs its own plan, written before the unblinding, and the
  unblinding itself.

## Alternatives considered

- **Wait for the unblinding.** Leaves the integrated safety work undone while the studies stay
  blinded, which may be for good.
- **Pooled tables by dummy arm.** Would repeat the studies' dummy-arm tables with nothing new
  to check.

## Consequences

- The pooled tables can be read now without any risk to the blinding.
- The two studies have different lengths (12 and 26 weeks): percentages of patients are not
  comparable between the study columns; rates per 100 patient-years are, and the tables say so.
- The pooled column counts placebo patients, so its rates are not the rates on FJF-2026.
