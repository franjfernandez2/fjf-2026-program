# ADR 0016: Benchmark specifications read from the pilot's Define-XML 1.0

- **Status:** Accepted
- **Date:** 2026-11-11
- **Decided by:** Francisco Fernández

## Context

The ADaM programs of the benchmark must follow a specification, not the other way round. The pilot's specification is its ADaM `define.xml`. For every variable, it gives:

- label, type, length and display format;
- the derivation in words;
- codelists and value-level metadata per parameter.

It also holds analysis results metadata for 14 tables and figures: the parameters, the selection criteria, the SAP section and the SAS code of each analysis.

The plan was to read it with `metacore`. But `metacore::define_to_metacore()` 0.3.0 reads only Define-XML 2.0, and the pilot's file is Define-XML 1.0 (ODM 1.2, 2012). It fails with `object 'dataset' not found`.

## Decision

- A reader for Define-XML 1.0, `read_define_v1()` in `R/define_v1.R`, turns the official file into five tables:
  - datasets;
  - variables;
  - value-level metadata;
  - codelists;
  - analysis results.
- In Define-XML 1.0 a value-level item has no WhereClause. Its parameter is the last part of its OID (`ADQSADAS.AVAL.ACTOT` → `PARAMCD == 'ACTOT'`).
- `define_v1_to_metacore()` builds a `metacore` object for the four benchmark datasets, so that `metatools` and `xportr` can use it later.
- Two checks run in the pipeline, and their results go to the discrepancy log:
  - `check_against_spec()` compares a dataset with its specification: presence, order, labels and types of variables, and parameters against value-level metadata.
  - `check_analysis_results()` checks that each analysis selects only parameters it declares.
- The specification is not committed. It is CDISC's metadata, read from the downloaded file on every run (ADR 0015).

## Alternatives considered

- **Retype the specification by hand from `define.pdf`:** that was the fallback in the plan. It would cost a day and would be a copy that can drift from the source without anyone noticing.
- **Convert the file to Define-XML 2.0 with an external tool:** it adds a dependency and an intermediate file that nobody reviews.
- **Write the specification from scratch from the SAP:** that tests our reading of the SAP, not the pipeline against a known answer.

## Consequences

- The pilot is consistent with itself on variables: the four official ADaM datasets match their specification with no findings (169 variables, as of 2026-11-11).
- It is not consistent on analysis results. Discrepancy D-001 is that Table 14-3.01 selects `PARAMCD="ATOT"` but declares `ACTOT`.
- `metacore` (and its dependencies) is added to `renv.lock`, which grows from 95 to 99 packages.
- The reader handles only what the pilot's file uses. For example, it does not handle `def:WhereClauseDef`, which Define-XML 1.0 does not have.
