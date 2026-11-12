# ADR 0024: Tables of the raw data layer

- **Status:** Accepted (reviewer's gate-2 review, 2026-11-12)
- **Date:** 2026-11-12
- **Decided by:** Francisco Fernández

## Context

The studies' raw data live in PostgreSQL, one schema per study, with separate roles and an audit trail ([ADR 0006](0006-raw-data-database.md), [ADR 0013](0013-database-roles-and-audit-trail.md)). The schemas had no tables yet. The simulator needs a target to write to, and the SDTM programs of F3 need a fixed source. The dossier asks for messy, EDC-like raw data, so that cleaning and mapping are real work.

## Decision

- **One specification file**, [`docs/specs/raw-layer.yml`](../specs/raw-layer.yml), lists the tables, their columns, types, codelists and natural keys. The DDL in `db/init/04-raw-tables.sql` is written from it, and a test fails if the two disagree. The simulator writes exactly these columns.
- **Eleven CRF-like tables**, the same in both studies: enrolment, randomisation, demography, visits, laboratory results, vital signs, exposure, disposition, adverse events, concomitant medication and hypoglycaemia.
- **Raw types.** Dates and laboratory results are text, so partial dates and qualified results (`<5`) can be stored as collected.
- **No natural primary key.** Each row has a `record_id` primary key. Duplicates can therefore be loaded, and F3 must detect them by the natural key.
- **Codelists are CHECK constraints**, like EDC edit checks. Arms and visit codes come from each protocol. Verbatim terms stay free text and are coded in F3.
- **Tables are created empty by the administrator**, so the audit triggers and privileges attach automatically.

## Alternatives considered

- **SDTM-shaped raw tables.** They would make F3 trivial and are unlike any EDC export.
- **Typed dates and numbers in the raw layer.** Cleaner, but they would refuse the partial dates and qualified results that real data contain, or force the simulator to clean its own errors.
- **Natural primary keys.** They would refuse duplicates at loading, so the duplicate check of F3 would have nothing to find.
- **Hand-written DDL.** It drifts from the specification and from the simulator.

## Consequences

- F3 converts dates, parses results, removes duplicates and codes verbatim terms, as in a real study.
- A value outside a codelist cannot be loaded. The data errors of the simulator (PR 24) are therefore of the kinds an EDC lets through: partial dates, out-of-window visits, duplicates, wrong units and unusual values.
- An existing database volume does not run the init scripts again. `tools/write-raw-ddl.R --apply` creates the tables once.
