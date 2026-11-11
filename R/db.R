# Connections to the clinical database.
#
# Credentials and host come from environment variables (.env locally, GitHub secrets in CI).
# Three roles exist; each connection uses exactly one of them:
#   "reader"  read raw data and the audit trail            (fjf_reader)
#   "loader"  write raw data rows                           (fjf_loader)
#   "admin"   create tables and databases; tests and setup  (POSTGRES_USER)

db_credentials <- function(role = c("reader", "loader", "admin")) {
  role <- match.arg(role)
  creds <- switch(role,
    reader = list(user = "fjf_reader", password = Sys.getenv("FJF_READER_PASSWORD")),
    loader = list(user = "fjf_loader", password = Sys.getenv("FJF_LOADER_PASSWORD")),
    admin  = list(user = Sys.getenv("POSTGRES_USER"), password = Sys.getenv("POSTGRES_PASSWORD"))
  )
  if (!nzchar(creds$user) || !nzchar(creds$password)) {
    cli::cli_abort(c(
      "Missing database credentials for role {.val {role}}.",
      "i" = "Copy {.file .env.example} to {.file .env} and run the code through {.code docker compose run}."
    ))
  }
  creds
}

db_connect <- function(role = c("reader", "loader", "admin"),
                       dbname = Sys.getenv("POSTGRES_DB", "fjf2026")) {
  role <- match.arg(role)
  creds <- db_credentials(role)
  DBI::dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("FJF_DB_HOST", "localhost"),
    port     = as.integer(Sys.getenv("FJF_DB_PORT", "55432")),
    dbname   = dbname,
    user     = creds$user,
    password = creds$password
  )
}

db_available <- function() {
  ok <- tryCatch(
    {
      con <- db_connect("reader")
      DBI::dbDisconnect(con)
      TRUE
    },
    error = function(e) FALSE
  )
  isTRUE(ok)
}
