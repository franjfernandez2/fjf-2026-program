# ADR 0005: Build order: benchmark, phase III, safety, phase II

- **Status:** Accepted
- **Date:** 2026-11-11
- **Decided by:** Francisco Fernández

## Context

Studies can be built in chronological order (phase II first) or in order of value to the portfolio.

## Decision

Build the CDISC benchmark first, then FJF-2026-301 (phase III), then integrated safety, then FJF-2026-201 (phase II).

## Alternatives considered

Chronological order (phase II before phase III): more natural story, but the most valuable piece arrives later. Skipping the benchmark: saves about one week but loses the only test against public data with a known answer.

## Consequences

The story stays coherent because both studies are simulated from the same true dose-response curve, so the dose selected in phase II agrees with the dose already used in phase III.
