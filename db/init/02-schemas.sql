-- Raw data schemas, one per study, and the privileges of each role.
-- Runs once, as the administrator, when the database volume is first initialised.
--
-- Table structure is owned by the administrator, as in an EDC system where the study
-- build is separate from data entry. fjf_loader therefore cannot alter tables or
-- disable their audit triggers.

-- Nobody gets anything by default.
REVOKE ALL ON SCHEMA public FROM PUBLIC;
DO $$ BEGIN
  EXECUTE format('REVOKE ALL ON DATABASE %I FROM PUBLIC', current_database());
  EXECUTE format('GRANT CONNECT ON DATABASE %I TO fjf_loader, fjf_reader', current_database());
END $$;

CREATE SCHEMA raw_201;
CREATE SCHEMA raw_301;
COMMENT ON SCHEMA raw_201 IS 'Raw EDC-like data for study FJF-2026-201 (phase II). Simulated data.';
COMMENT ON SCHEMA raw_301 IS 'Raw EDC-like data for study FJF-2026-301 (phase III). Simulated data.';

GRANT USAGE ON SCHEMA raw_201, raw_301 TO fjf_loader, fjf_reader;

-- Privileges on tables the administrator creates in these schemas from now on.
ALTER DEFAULT PRIVILEGES FOR ROLE CURRENT_USER IN SCHEMA raw_201, raw_301
  GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO fjf_loader;
ALTER DEFAULT PRIVILEGES FOR ROLE CURRENT_USER IN SCHEMA raw_201, raw_301
  GRANT USAGE, SELECT ON SEQUENCES TO fjf_loader;
ALTER DEFAULT PRIVILEGES FOR ROLE CURRENT_USER IN SCHEMA raw_201, raw_301
  GRANT SELECT ON TABLES TO fjf_reader;
