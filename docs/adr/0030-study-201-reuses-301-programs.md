# ADR 0030: FJF-2026-201 uses the SDTM and ADaM programs of FJF-2026-301

- **Status:** Accepted (reviewer, 2026-11-16)
- **Date:** 2026-11-13
- **Decided by:** proposed by the programmer

## Context

FJF-2026-201 (phase II, five arms, 12 weeks) has the same raw layer, CRF, cleaning rules and
derivations as FJF-2026-301: its SAP says so, so that the two studies can be pooled for the
integrated safety analysis. The programs of FJF-2026-301 read the protocol and the
specifications for everything that differs between studies (arms, visits, windows, primary
visit), except a few trial summary (TS) values written in the program.

## Decision

- FJF-2026-201 uses the programs in `studies/301/sdtm`, `studies/301/adam` and `qc/301`, with
  its own protocol and specifications (`docs/specs/sdtm-201.yml`). They keep their `_301`
  names, which now mean "first written for FJF-2026-301".
- What differs between the studies goes into the specification: the TS phase, trial types and
  objectives moved there (`trial_summary`). The values for FJF-2026-301 are unchanged.
- FJF-2026-201 has its own blinding (public seed 201201, chosen by the reviewer), its own
  database lock (`docs/lock/dblock-201.yml`, tag `dblock-201`) and its own conformance notes.

## Consequences

- A fix to a shared program fixes both studies, and both pipelines rebuild with it. A change
  needed by one study only must be driven by the specification, not by a study test in code.
- The second programming of FJF-2026-301 checks FJF-2026-201 too. Both are written by the same
  programmer, as already recorded for FJF-2026-301.
- Renaming the programs to study-neutral names is possible later; it would change no output.
