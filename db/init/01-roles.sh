#!/usr/bin/env bash
# Creates the two working roles. Runs once, when the database volume is first initialised.
# The administrator (POSTGRES_USER) is created by the postgres image itself.
#
#   fjf_loader  writes raw data rows; cannot change table structure or touch the audit log
#   fjf_reader  reads raw data and the audit log; cannot write anything
set -euo pipefail

psql -v ON_ERROR_STOP=1 \
     --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" \
     -v loader_pw="$FJF_LOADER_PASSWORD" \
     -v reader_pw="$FJF_READER_PASSWORD" <<'SQL'
CREATE ROLE fjf_loader LOGIN PASSWORD :'loader_pw';
CREATE ROLE fjf_reader LOGIN PASSWORD :'reader_pw';
COMMENT ON ROLE fjf_loader IS 'Loads raw clinical data rows (data entry). No DDL, no audit access.';
COMMENT ON ROLE fjf_reader IS 'Read-only access to raw data and the audit trail.';
SQL
