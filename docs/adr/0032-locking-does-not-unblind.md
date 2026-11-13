# ADR 0032: Locking the database does not unblind

- **Status:** Proposed (for the reviewer's review)
- **Date:** 2026-11-13
- **Decided by:** the reviewer (keep the studies blinded); mechanism proposed by the programmer

## Context

Under ADR 0027 the pipeline switched to the real treatment assignment as soon as a lock file
(`docs/lock/dblock-<study>.yml`) existed. Both studies were locked on 2026-11-13 (tags
`dblock-301` and `dblock-201`), so the next pipeline run, by anyone, locally or in CI, would
have unblinded them. The reviewer decided that the studies stay blinded until they decide
otherwise. When this ADR was written, no target had been built with the real arms: the
stored assignments of both studies were the dummy ones.

## Decision

- Locking and unblinding are two decisions. The pipeline uses the real assignment of a study
  only when its database is locked **and** the reviewer has committed
  `docs/unblind/unblind-<study>.yml` with `status: authorised` (`unblinding_authorised()`).
- The targets `dblock_301` and `dblock_201` become `unblinded_301` and `unblinded_201`; they
  also decide whether the outputs carry the BLINDED note.
- Only the reviewer writes an authorisation; the programmer never does. A test fails while
  any study of the repository is authorised, so an authorisation cannot slip in unnoticed;
  it is removed in the same change as the authorisation.
- The lock files, tags and frozen commit are unchanged.

## Consequences

- Locked studies can be rebuilt, checked and reported on blinded data at any time; the
  GitHub CI of 2026-12-01 and `tools/ci-local.sh` stay blinded.
- This change must be merged before any pipeline run on a commit that has a lock file.
- The blinding code changes after the lock; the analysis programs frozen by the tags do not.
