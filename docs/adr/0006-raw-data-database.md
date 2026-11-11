# ADR 0006: Database: PostgreSQL in Docker for the raw data layer

- **Status:** Accepted
- **Date:** 2026-11-11
- **Decided by:** Francisco Fernández

## Context

Raw clinical trial data normally live in an EDC database. The programme needs a raw layer that can be audited with SQL.

## Decision

The raw layer lives in PostgreSQL running in Docker: one schema per study (raw_201, raw_301), separate roles for loading and reading, and an audit trail of changes. From SDTM onwards, data leave the database as Parquet and SAS XPT files.

## Alternatives considered

DuckDB with Parquet: simpler and serverless, but less like a clinical database. Both PostgreSQL and DuckDB: most realistic, most complex.

## Consequences

More infrastructure: a database service is needed locally (Docker Compose) and in continuous integration (a PostgreSQL service in GitHub Actions).
