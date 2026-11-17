# ADR 0037: Compliance of FJF-2026-201 counts the two tablets a day

- **Status:** Accepted (decision delegated by the reviewer to the assistant in chat, 2026-11-17)
- **Date:** 2026-11-17
- **Decided by:** the assistant, on the reviewer's delegation ("decide according to industry standards"); recorded as a deviation after the database lock

## Context

ADR 0035 recorded a finding: the compliance of FJF-2026-201 (`ADSL.COMPLPCT`, shown in T-14.3.6 and in the
exposure tables of the integrated safety analysis) averaged about 186%. FJF-2026-201 is double dummy: every
patient takes two tablets a day (protocol, study treatment). The ADaM program is shared with FJF-2026-301
([ADR 0030](0030-study-201-reuses-301-programs.md)) and divided the tablets taken by the days of use, that is,
by one tablet a day. The study was locked (`dblock-201`, 2026-11-13) and unblinded, so the correction was left
as a decision. The reviewer delegated it.

## Decision

Compliance is the tablets taken over the tablets due, as is usual in clinical studies (ICH E3, dispensed minus
returned over expected). The tablets due are the days of use times the tablets a day of the study. The number
is a parameter of the ADaM specification (`compliance.tablets_per_day` in `docs/specs/adam-201.yml`, 2); the
program uses one when the specification has none, so FJF-2026-301 does not change. The second programming
reads the same parameter. The correction is a **deviation after the lock**, recorded here, with its evidence:

- Before and after, every output of FJF-2026-201, FJF-2026-301 and the integrated safety analysis was compared.
  Only the outputs that show compliance changed: T-14.3.6 of FJF-2026-201 (mean 186% becomes 93%, exactly half)
  and the exposure tables of the integrated safety analysis (T-ISS-2 and T-ISS-8). No efficacy result,
  no safety incidence, no hypothesis decision and nothing of FJF-2026-301 changed.
- `PPROTFL` of FJF-2026-201 (compliance of 80% or more) is derived from it. No analysis of FJF-2026-201 uses
  it (its SAP defines no per-protocol set), so no result changed.
- The two programmings agree, the full test suite and the pipeline pass.

The frozen plans (`sap-201-v1.0`, `iss-sap-v1.0`) and the lock file are not changed.

## Alternatives considered

- **Leave it, with a note.** Honest, but a public programme would carry a visibly wrong exposure table.
- **Change the shared derivation to two tablets a day for both studies.** It would be wrong for
  FJF-2026-301 (single dummy, one tablet).
- **Derive the tablets a day from the data.** It would hide the protocol fact in the data and could mask
  real over-dispensing.

## Consequences

- The compliance of FJF-2026-201 is about 93%, consistent with the 301 level, and the finding of ADR 0035 is closed.
- A parameter in the specification now carries a study fact the program used to assume. A future double-dummy
  study sets it.
- The change after a lock is in the open, with its before-and-after evidence, and not rewritten into the history.
