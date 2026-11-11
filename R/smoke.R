# Smoke test for phase F0: proves that the environment, the database and the pipeline
# are wired together. It is replaced by real targets from phase F1 onwards.

collect_environment_info <- function(lockfile = here::here("renv.lock")) {
  lock <- renv::lockfile_read(lockfile)
  # renv returns repositories either as a named vector or as a list of records.
  repo <- lock$R$Repositories[[1]]
  list(
    r_version         = paste(R.version$major, R.version$minor, sep = "."),
    platform          = R.version$platform,
    locked_r_version  = lock$R$Version,
    locked_packages   = length(lock$Packages),
    package_snapshot  = if (is.list(repo)) repo$URL else unname(repo),
    lockfile_md5      = unname(tools::md5sum(lockfile))
  )
}

check_database <- function() {
  con <- db_connect("reader")
  on.exit(DBI::dbDisconnect(con))
  schemas <- DBI::dbGetQuery(con, "
    SELECT nspname FROM pg_namespace
    WHERE nspname IN ('raw_201', 'raw_301', 'audit')
    ORDER BY nspname")$nspname
  event_triggers <- DBI::dbGetQuery(con, "SELECT evtname FROM pg_event_trigger")$evtname
  list(
    server_version = DBI::dbGetQuery(con, "SHOW server_version")$server_version,
    connected_as   = DBI::dbGetQuery(con, "SELECT session_user AS u")$u,
    schemas        = schemas,
    audit_enabled  = "audit_raw_tables" %in% event_triggers
  )
}

summarise_smoke_test <- function(environment, database) {
  checks <- c(
    "R matches renv.lock"          = environment$r_version == environment$locked_r_version,
    "Study schemas present"        = setequal(database$schemas, c("audit", "raw_201", "raw_301")),
    "Audit event trigger active"   = isTRUE(database$audit_enabled),
    "Connected as read-only role"  = identical(database$connected_as, "fjf_reader")
  )
  if (!all(checks)) {
    cli::cli_abort(c("Smoke test failed:", stats::setNames(names(checks)[!checks], rep("x", sum(!checks)))))
  }
  cli::cli_alert_success("Smoke test passed: {length(checks)} checks.")
  list(passed = names(checks), environment = environment, database = database)
}
