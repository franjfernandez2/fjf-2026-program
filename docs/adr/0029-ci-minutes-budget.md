# ADR 0029: Continuous integration within the monthly Actions minutes

- **Status:** Proposed (for the reviewer's review)
- **Date:** 2026-11-13
- **Decided by:** proposed by the programmer

## Context

The repository is private, so GitHub Actions draws on the account's 2,000 minutes a month.
On 2026-11-13 they ran out, with 18 days of the cycle left. In two days about 40 runs had
used them:

- every stacked pull request ran twice, once on its own base and again when retargeted to
  main;
- every push to main, and every run started by hand, rebuilt the whole pipeline from scratch
  (the multiple imputation of FJF-2026-301 alone takes about 35 minutes in CI);
- a change to Markdown only (README, ADRs) ran the full check too.

## Decision

- CI runs on pull requests into main and on pushes to main. A stacked pull request is
  checked when it is retargeted to main; before that, the programmer runs the same checks
  locally in the same Docker image, and says so in the pull request.
- Changes to Markdown only do not run it.
- Pull requests, pushes and runs started by hand reuse the pipeline store, so targets
  rebuilds only what is outdated. A weekly run (Monday 06:00 UTC) rebuilds the whole pipeline
  from scratch, so a fresh rebuild is still checked.
- `analysis_301_data` reads only the datasets the analyses use (ADSL, ADEFF, ADTTE).

## Consequences

- A merge costs one CI run instead of two or three, mostly incremental.
- A fault that only a fresh rebuild shows is caught within a week instead of at once; the
  database lock does not depend on CI, since `tools/lock-database.sh` runs its own checks.
- If the minutes run out, CI stops until the cycle resets (with a $0 budget, nothing is
  charged); local checks in Docker stand in for it, as they did for this change.
- `tools/ci-local.sh [<commit>]` runs the workflow's steps locally on a fresh clone, from
  scratch, with its own throwaway database. While the minutes are used up it is the check of
  record before a milestone (a lock, unblinding, a release), and the pull request says so.
