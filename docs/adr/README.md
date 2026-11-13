# Architecture Decision Records

Each significant decision is recorded once, with its context, the alternatives considered and its consequences. Accepted records are never edited; a later decision that changes one is recorded as a new ADR that supersedes it.

New records copy [0000-template.md](0000-template.md).

| ADR | Decision | Status |
| --- | --- | --- |
| [0001](0001-indication.md) | Indication: type 2 diabetes, HbA1c at week 26 | Accepted |
| [0002](0002-scope.md) | Scope: core programme plus extensions E1 and E2 | Accepted |
| [0003](0003-cadence.md) | Cadence: near full-time, about 12 weeks | Accepted |
| [0004](0004-division-of-work.md) | Division of work: the AI assistant programs, the author reviews and decides | Accepted |
| [0005](0005-build-order.md) | Build order: benchmark, phase III, safety, phase II | Accepted |
| [0006](0006-raw-data-database.md) | Database: PostgreSQL in Docker for the raw data layer | Accepted |
| [0007](0007-language.md) | Language: English repository, bilingual website | Accepted |
| [0008](0008-visibility.md) | Visibility: private until gate 6 | Accepted |
| [0009](0009-phase-iii-comparator.md) | Phase III comparator: placebo added to metformin | Accepted |
| [0010](0010-ai-disclosure.md) | AI assistance is declared clearly | Accepted |
| [0011](0011-naming.md) | Naming: compound FJF-2026 | Accepted |
| [0012](0012-reproducible-environment.md) | Reproducible environment: pinned image, dated package snapshot, renv | Accepted |
| [0013](0013-database-roles-and-audit-trail.md) | Database roles and audit trail | Accepted |
| [0014](0014-documents-with-quarto.md) | Documents with Quarto, PDF through Typst | Accepted |
| [0015](0015-third-party-pilot-data.md) | Third-party data for the benchmark: the CDISC pilot, downloaded and verified | Accepted |
| [0016](0016-specifications-from-define-xml.md) | Benchmark specifications read from the pilot's Define-XML 1.0 | Accepted |
| [0017](0017-benchmark-acceptance-criteria.md) | Acceptance criteria of the benchmark (gate 1) | Accepted |
| [0018](0018-sealed-truth-and-data-generating-model.md) | Sealed truth and the data-generating model | Accepted |
| [0019](0019-estimands-of-fjf-2026-301.md) | Estimands of FJF-2026-301 | Accepted |
| [0020](0020-sample-size-by-simulation.md) | Sample size of FJF-2026-301 by simulation | Accepted |
| [0021](0021-sap-dry-run-on-planning-data.md) | Missing data in FJF-2026-301, and a dry run of the SAP on planning data | Accepted |
| [0022](0022-multiplicity-and-traceability-of-fjf-2026-301.md) | Multiplicity, subgroups and output traceability of FJF-2026-301 | Accepted |
| [0023](0023-mcpmod-design-of-201.md) | MCP-Mod design and sample size of FJF-2026-201 | Accepted |
| [0024](0024-raw-data-layer.md) | Tables of the raw data layer | Accepted |
| [0025](0025-data-errors-and-generation.md) | Data errors in the raw data, and how the study data are generated and loaded | Accepted |
| [0026](0026-cleaning-rules-and-mapping.md) | Cleaning rules, and where cleaning happens | Accepted |
| [0027](0027-blinded-data-review-and-lock.md) | Blinded data review, and the database lock | Accepted |
| [0028](0028-pre-lock-analysis-decisions-301.md) | Analysis decisions of FJF-2026-301 taken on blinded data, before the lock | Accepted |
| [0029](0029-ci-minutes-budget.md) | Continuous integration within the monthly Actions minutes | Proposed |
| [0030](0030-study-201-reuses-301-programs.md) | FJF-2026-201 uses the SDTM and ADaM programs of FJF-2026-301 | Proposed |
| [0031](0031-tlf-and-utility-index-201.md) | Tables, figures and clinical utility index of FJF-2026-201 | Proposed |
| [0032](0032-locking-does-not-unblind.md) | Locking the database does not unblind | Proposed |
