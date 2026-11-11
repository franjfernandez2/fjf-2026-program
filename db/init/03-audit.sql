-- Audit trail for the raw data schemas.
-- Runs once, as the administrator, when the database volume is first initialised.
--
-- Design, modelled on the audit-trail expectations for electronic clinical records
-- (who, when, what changed, old and new value, and why):
--   * every INSERT, UPDATE, DELETE and TRUNCATE on a raw_* table writes one row to audit.change_log;
--   * the trigger is attached automatically to every table created in raw_201 or raw_301;
--   * UPDATE and DELETE require a reason for change, set per transaction with
--       SET LOCAL fjf.change_reason = '...';
--   * the log cannot be updated or deleted through normal SQL, and fjf_loader cannot read or write it.

CREATE SCHEMA audit;
COMMENT ON SCHEMA audit IS 'Append-only audit trail of changes to raw clinical data.';

CREATE TABLE audit.change_log (
  change_id     bigint      GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  changed_at    timestamptz NOT NULL DEFAULT clock_timestamp(),
  changed_by    text        NOT NULL DEFAULT session_user,
  transaction   bigint      NOT NULL DEFAULT txid_current(),
  schema_name   text        NOT NULL,
  table_name    text        NOT NULL,
  operation     text        NOT NULL CHECK (operation IN ('INSERT', 'UPDATE', 'DELETE', 'TRUNCATE')),
  old_row       jsonb,
  new_row       jsonb,
  change_reason text
);
COMMENT ON TABLE audit.change_log IS 'One row per changed raw-data row: who, when, what, old value, new value, why.';

REVOKE ALL ON audit.change_log FROM PUBLIC;
GRANT USAGE ON SCHEMA audit TO fjf_reader;
GRANT SELECT ON audit.change_log TO fjf_reader;

-- The log is append-only: refuse UPDATE and DELETE, even from the table owner.
CREATE FUNCTION audit.refuse_change() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION 'audit.change_log is append-only: % is not allowed', TG_OP;
END $$;

CREATE TRIGGER change_log_append_only
  BEFORE UPDATE OR DELETE OR TRUNCATE ON audit.change_log
  FOR EACH STATEMENT EXECUTE FUNCTION audit.refuse_change();

-- Row-level logging. SECURITY DEFINER lets the trigger write to the log on behalf of
-- fjf_loader, which has no privilege on it; session_user still records the real login.
CREATE FUNCTION audit.log_row_change() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, pg_temp AS $$
DECLARE
  reason text := nullif(current_setting('fjf.change_reason', true), '');
BEGIN
  IF TG_OP IN ('UPDATE', 'DELETE') AND reason IS NULL THEN
    RAISE EXCEPTION '% on %.% requires a reason for change: SET LOCAL fjf.change_reason = ''...''',
      TG_OP, TG_TABLE_SCHEMA, TG_TABLE_NAME;
  END IF;

  INSERT INTO audit.change_log (schema_name, table_name, operation, old_row, new_row, change_reason)
  VALUES (TG_TABLE_SCHEMA, TG_TABLE_NAME, TG_OP,
          CASE WHEN TG_OP IN ('UPDATE', 'DELETE') THEN to_jsonb(OLD) END,
          CASE WHEN TG_OP IN ('INSERT', 'UPDATE') THEN to_jsonb(NEW) END,
          reason);
  RETURN NULL;
END $$;

CREATE FUNCTION audit.log_truncate() RETURNS trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, pg_temp AS $$
BEGIN
  INSERT INTO audit.change_log (schema_name, table_name, operation, change_reason)
  VALUES (TG_TABLE_SCHEMA, TG_TABLE_NAME, TG_OP,
          nullif(current_setting('fjf.change_reason', true), ''));
  RETURN NULL;
END $$;

-- Attach both triggers to every table created in a raw_* schema, so that no table can
-- be forgotten. Rows inserted by CREATE TABLE ... AS are written before the trigger
-- exists and are therefore not logged; raw tables are created empty.
CREATE FUNCTION audit.attach_to_new_tables() RETURNS event_trigger
LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, pg_temp AS $$
DECLARE
  obj record;
BEGIN
  FOR obj IN
    SELECT object_identity
    FROM pg_event_trigger_ddl_commands()
    WHERE object_type = 'table' AND schema_name IN ('raw_201', 'raw_301')
  LOOP
    EXECUTE format('CREATE TRIGGER audit_row AFTER INSERT OR UPDATE OR DELETE ON %s
                    FOR EACH ROW EXECUTE FUNCTION audit.log_row_change()', obj.object_identity);
    EXECUTE format('CREATE TRIGGER audit_truncate AFTER TRUNCATE ON %s
                    FOR EACH STATEMENT EXECUTE FUNCTION audit.log_truncate()', obj.object_identity);
  END LOOP;
END $$;

CREATE EVENT TRIGGER audit_raw_tables
  ON ddl_command_end
  WHEN TAG IN ('CREATE TABLE', 'CREATE TABLE AS', 'SELECT INTO')
  EXECUTE FUNCTION audit.attach_to_new_tables();
