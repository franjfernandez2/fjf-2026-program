# Writes db/init/04-raw-tables.sql from docs/specs/raw-layer.yml.
#   docker compose run --rm analysis Rscript tools/write-raw-ddl.R
#
# On an existing database volume the init scripts do not run again; apply the file once with
#   docker compose run --rm analysis Rscript tools/write-raw-ddl.R --apply
# which creates the tables as the administrator if they do not exist yet.
for (f in c("protocol.R", "raw_spec.R", "db.R")) source(here::here("R", f))
spec <- read_raw_spec(here::here("docs", "specs", "raw-layer.yml"))
protocols <- list(
  raw_201 = read_protocol(here::here("docs", "protocols", "fjf-2026-201.yml")),
  raw_301 = read_protocol(here::here("docs", "protocols", "fjf-2026-301.yml"))
)
problems <- check_raw_spec(spec, protocols)
if (length(problems) > 0) cli::cli_abort(c("The raw-layer specification is inconsistent:", problems))
sql <- raw_ddl_file(spec, protocols)
out <- here::here("db", "init", "04-raw-tables.sql")
writeLines(sql, out, useBytes = TRUE)
cli::cli_inform("Wrote {.file {out}}.")

if ("--apply" %in% commandArgs(trailingOnly = TRUE)) {
  con <- db_connect("admin")
  on.exit(DBI::dbDisconnect(con))
  have <- DBI::dbGetQuery(con, paste(
    "SELECT count(*) AS n FROM information_schema.tables",
    "WHERE table_schema IN ('raw_201', 'raw_301')"
  ))$n
  if (have > 0) {
    cli::cli_inform("The raw tables already exist ({have}); nothing applied.")
  } else {
    DBI::dbWithTransaction(con, for (stmt in strsplit(sql, ";\n", fixed = TRUE)[[1]]) {
      if (nzchar(trimws(gsub("--[^\n]*", "", stmt)))) DBI::dbExecute(con, stmt)
    })
    cli::cli_inform("Created the raw tables.")
  }
}
