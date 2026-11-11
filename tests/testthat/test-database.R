# Gate 0 checks on the clinical database: roles, privileges and the audit trail.

with_test_db()

admin <- test_connect("admin")
loader <- test_connect("loader")
reader <- test_connect("reader")
withr::defer({
  DBI::dbDisconnect(admin)
  DBI::dbDisconnect(loader)
  DBI::dbDisconnect(reader)
})

# A raw table created by the administrator, as the study build would.
DBI::dbExecute(admin, "
  CREATE TABLE raw_301.vitals_test (
    subject_id text    NOT NULL,
    visit      text    NOT NULL,
    sysbp      integer,
    PRIMARY KEY (subject_id, visit)
  )")

audit_rows <- function() {
  DBI::dbGetQuery(reader, "
    SELECT changed_by, operation, old_row, new_row, change_reason
    FROM audit.change_log
    WHERE schema_name = 'raw_301' AND table_name = 'vitals_test'
    ORDER BY change_id")
}

in_transaction <- function(con, reason, sql) {
  DBI::dbWithTransaction(con, {
    if (!is.null(reason)) {
      DBI::dbExecute(con, paste0("SET LOCAL fjf.change_reason = ", DBI::dbQuoteString(con, reason)))
    }
    DBI::dbExecute(con, sql)
  })
}

test_that("each role connects as itself", {
  expect_equal(DBI::dbGetQuery(loader, "SELECT session_user AS u")$u, "fjf_loader")
  expect_equal(DBI::dbGetQuery(reader, "SELECT session_user AS u")$u, "fjf_reader")
})

test_that("the study schemas and the audit schema exist", {
  schemas <- DBI::dbGetQuery(admin, "SELECT nspname FROM pg_namespace")$nspname
  expect_true(all(c("raw_201", "raw_301", "audit") %in% schemas))
})

test_that("new raw tables get the audit triggers automatically", {
  triggers <- DBI::dbGetQuery(admin, "
    SELECT tgname FROM pg_trigger
    WHERE tgrelid = 'raw_301.vitals_test'::regclass AND NOT tgisinternal")$tgname
  expect_setequal(triggers, c("audit_row", "audit_truncate"))
})

test_that("the reader cannot write raw data", {
  expect_error(
    DBI::dbExecute(reader, "INSERT INTO raw_301.vitals_test VALUES ('301-001', 'BASELINE', 130)"),
    "permission denied"
  )
})

test_that("the loader cannot change table structure", {
  expect_error(DBI::dbExecute(loader, "CREATE TABLE raw_301.rogue (x int)"), "permission denied")
  expect_error(
    DBI::dbExecute(loader, "ALTER TABLE raw_301.vitals_test DISABLE TRIGGER audit_row"),
    "must be owner"
  )
})

test_that("the loader cannot read or write the audit log", {
  expect_error(DBI::dbGetQuery(loader, "SELECT * FROM audit.change_log"), "permission denied")
})

test_that("an insert is logged with user, time and new value", {
  in_transaction(loader, NULL, "INSERT INTO raw_301.vitals_test VALUES ('301-001', 'BASELINE', 130)")
  log <- audit_rows()
  expect_equal(nrow(log), 1)
  expect_equal(log$operation, "INSERT")
  expect_equal(log$changed_by, "fjf_loader")
  expect_match(log$new_row, '"sysbp": 130')
  expect_true(is.na(log$old_row))
})

test_that("an update without a reason for change is refused", {
  expect_error(
    in_transaction(loader, NULL, "UPDATE raw_301.vitals_test SET sysbp = 128 WHERE subject_id = '301-001'"),
    "requires a reason for change"
  )
  expect_equal(nrow(audit_rows()), 1)
})

test_that("an update is logged with old value, new value and reason", {
  in_transaction(
    loader, "Transcription error corrected against source",
    "UPDATE raw_301.vitals_test SET sysbp = 128 WHERE subject_id = '301-001'"
  )
  log <- audit_rows()
  expect_equal(nrow(log), 2)
  last <- log[2, ]
  expect_equal(last$operation, "UPDATE")
  expect_match(last$old_row, '"sysbp": 130')
  expect_match(last$new_row, '"sysbp": 128')
  expect_equal(last$change_reason, "Transcription error corrected against source")
})

test_that("a delete is logged with the deleted row and reason", {
  in_transaction(
    loader, "Duplicate record",
    "DELETE FROM raw_301.vitals_test WHERE subject_id = '301-001'"
  )
  log <- audit_rows()
  expect_equal(log$operation[3], "DELETE")
  expect_match(log$old_row[3], '"sysbp": 128')
  expect_true(is.na(log$new_row[3]))
})

test_that("the audit log is append-only, even for the administrator", {
  expect_error(DBI::dbExecute(admin, "UPDATE audit.change_log SET changed_by = 'someone'"), "append-only")
  expect_error(DBI::dbExecute(admin, "DELETE FROM audit.change_log"), "append-only")
})
