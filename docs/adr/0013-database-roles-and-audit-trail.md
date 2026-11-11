# ADR 0013: Database roles and audit trail

- **Status:** Accepted
- **Date:** 2026-11-11
- **Decided by:** Francisco Fernández

## Context

ADR 0006 places the raw data in PostgreSQL to imitate an EDC system. An EDC must show who changed each data point, when, the old and new values and why: the audit-trail expectations for electronic clinical records. The design must make the trail hard to bypass by the role that enters data.

## Decision

- **Three roles.** `fjf_admin` owns the structure (the "study build"); `fjf_loader` inserts, updates and deletes rows; `fjf_reader` only reads. Table structure belongs to the administrator, so the loader cannot alter tables or disable their triggers. This changes the F0 plan, where the loader would have created tables.
- **Automatic audit.** An event trigger attaches row-level and truncate triggers to every table created in `raw_201` or `raw_301`, so no table can be left unaudited.
- **What is logged.** One row in `audit.change_log` per changed row: login (`session_user`), timestamp, transaction, table, operation, old row and new row as JSON, and reason.
- **Reason for change.** `UPDATE` and `DELETE` are refused unless the transaction sets `fjf.change_reason`.
- **Append-only log.** `UPDATE`, `DELETE` and `TRUNCATE` on the log are refused, even for its owner. The loader cannot read or write it; the reader can read it.
- **Tests never touch the study database.** Each test run clones it into a throwaway `<db>_test` database from the study database as a template and drops it afterwards.
- PostgreSQL 18 is pinned by digest, and the port is published only on `127.0.0.1:55432`.

## Alternatives considered

- Loader owns its tables: simpler, but the loader could disable the audit trigger.
- Audit with an extension such as pgAudit: logs statements to the server log rather than old and new values in a queryable table.
- Running tests against the study database: leaves test rows in an append-only log forever.

## Consequences

- Raw table definitions are written by the administrator (in phase F2, by the simulator's database build).
- A PostgreSQL superuser can still disable triggers. That is accepted: in production this would be separated by role and procedure; here the administrator is the study build.
- Rows created by `CREATE TABLE ... AS` are written before the trigger exists and are not logged; raw tables are always created empty.
