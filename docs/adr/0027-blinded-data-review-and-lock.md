# ADR 0027: Blinded data review, and the database lock

- **Status:** Proposed
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

In a real trial the programmers write and test the SDTM and ADaM programs, and the tables, before the database is locked, without knowing which subject is in which arm. Mistakes found then are fixed without anyone having seen a result by arm. Only after the lock is the randomisation released and the analysis run once.

Here the raw layer already holds the real arm of each subject (`randomisation.arm_code`), and the pipeline reads it. Without a rule, the first time a table by arm is built during F3 the result would be known before the programs are final, and every later fix to a program or a cleaning rule ([ADR 0026](0026-cleaning-rules-and-mapping.md)) could be suspected of following the result.

## Decision

- **Until the lock, the pipeline uses a dummy assignment.** The real arms are shuffled within each stratum with a public seed (`blinding_seed: 301301` in [`docs/specs/sdtm-301.yml`](../specs/sdtm-301.yml)). Each stratum keeps its number of subjects per arm, so the tables have the shape of the real ones, but the comparisons mean nothing.
- **The lock is a file and a tag.** The database is locked when `docs/lock/dblock-301.yml` exists with `status: locked` and the tag `dblock-301` marks that commit. The pipeline reads the file (it runs in a container without git); the reviewer creates the tag. A tool, `tools/lock-database.sh`, will write the file, check that the tree is clean and create the tag (F3 day 9).
- **The pipeline says which assignment it used.** The target `assignment_301` carries `blinded = TRUE` before the lock, and every output built from it will be marked as dummy.
- **Nothing that depends on the result changes after the lock.** Programs, specifications and cleaning rules are frozen by the tag. A change after it is a deviation, recorded with its reason.
- **The same rule will apply to FJF-2026-201,** with its own seed and lock file, when its SDTM is built.

## Consequences

- Programs and tables can be reviewed and tested on the real data, with the real visit structure and the real data errors, without revealing the effect.
- The dummy seed is public and the shuffle is reproducible, so the blinded outputs can be rebuilt and checked.
- Screen failures and subjects never randomised are not affected: they have no arm.
- Exposure and safety tables by arm built before the lock are also dummy. That is intended: no table by arm is real before the lock.
- Until the lock tool exists, the lock must not be declared by hand.
