# Database tests never touch the study database. Each test file that needs a database
# calls with_test_db(), which clones the study database (same schemas, roles, privileges,
# triggers) into a throwaway copy and drops it afterwards.

test_db_name <- function() paste0(Sys.getenv("POSTGRES_DB", "fjf2026"), "_test")

create_test_db <- function() {
  admin <- db_connect("admin", dbname = "postgres")
  on.exit(DBI::dbDisconnect(admin))
  study <- DBI::dbQuoteIdentifier(admin, Sys.getenv("POSTGRES_DB", "fjf2026"))
  test <- DBI::dbQuoteIdentifier(admin, test_db_name())
  DBI::dbExecute(admin, paste("DROP DATABASE IF EXISTS", test, "WITH (FORCE)"))
  DBI::dbExecute(admin, paste("CREATE DATABASE", test, "TEMPLATE", study))
  # Database-level privileges are not copied from a template.
  DBI::dbExecute(admin, paste("REVOKE ALL ON DATABASE", test, "FROM PUBLIC"))
  DBI::dbExecute(admin, paste("GRANT CONNECT ON DATABASE", test, "TO fjf_loader, fjf_reader"))
  invisible(test_db_name())
}

drop_test_db <- function() {
  admin <- db_connect("admin", dbname = "postgres")
  on.exit(DBI::dbDisconnect(admin))
  test <- DBI::dbQuoteIdentifier(admin, test_db_name())
  DBI::dbExecute(admin, paste("DROP DATABASE IF EXISTS", test, "WITH (FORCE)"))
}

with_test_db <- function(env = parent.frame()) {
  testthat::skip_if_not(db_available(), "Database not reachable: run inside docker compose")
  create_test_db()
  withr::defer(drop_test_db(), envir = env)
}

test_connect <- function(role) db_connect(role, dbname = test_db_name())
