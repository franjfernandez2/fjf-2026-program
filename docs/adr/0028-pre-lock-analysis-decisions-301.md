# ADR 0028: Analysis decisions of FJF-2026-301 taken on blinded data, before the lock

- **Status:** Accepted for items 1-3 (reviewer, 2026-11-13); items 4-7 proposed, for the reviewer's review
- **Date:** 2026-11-13
- **Decided by:** Francisco Fernández (items 1-3); proposed by the programmer (items 4-7)

## Context

The SAP is frozen (tag `sap-301-v1.0`). Building and checking SDTM and ADaM on the study data
(F3) found points it does not settle, and conformance checks (P21, CDISC CORE) found two that
need a data decision. All were met while the arms were the dummy assignment of
[ADR 0027](0027-blinded-data-review-and-lock.md), so none was taken knowing a result by arm.
The lock freezes them all.

## Decisions

1. **Vital signs outside predefined limits (SAP SF4).** The SAP names the limits but does not
   give them. They are:
   - systolic blood pressure ≥ 160 or ≤ 90 mmHg;
   - diastolic ≥ 100 or ≤ 50 mmHg;
   - pulse ≥ 120 or ≤ 50 beats/min;
   - body weight up or down by 7% or more from baseline.

   They are in `vital_sign_limits` of [`docs/specs/adam-301.yml`](../specs/adam-301.yml), as
   ADVS.CRIT1FL (high or increase) and CRIT2FL (low or decrease).
2. **Last dose after the last bottle (cleaning rule CR10).** In 129 patients the disposition
   page's last-dose date is 1-8 days after the last bottle ends. The disposition date stands,
   and the last bottle runs until it.
3. **SE element boundaries.** Each element ends the day the next starts, as SDTMIG asks. The
   shared days take the treatment EPOCH, so no record changes EPOCH.
4. **Weight windows.** Body weight uses the SAP's HbA1c window rule at its own scheduled
   visits (weeks 12 and 26).
5. **A value on a window midpoint** goes to the earlier visit, as in the F2 dry-run code.
6. **Partial AE start dates** (17 events) are imputed to the first dose when the period
   contains it and the event did not end before it; otherwise to the first day of the period.
7. **Worst laboratory value on treatment** (shift tables) is the value furthest outside its
   range, as a multiple of the limit crossed; with none outside, the last on treatment.

## Consequences

- Each decision is in a specification, a cleaning rule or a program, and both second
  programmings check the ones they cover.
- Items 4-7 can still change before the lock, if the reviewer decides otherwise; after the
  lock, a change is a deviation (ADR 0027).
