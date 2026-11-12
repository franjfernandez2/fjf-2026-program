# Loads the committed raw data (data/raw) into the database, as the loader role.
#   docker compose run --rm analysis Rscript tools/load-raw-data.R
# The files are checked against their manifest and the raw-layer specification first.
for (f in c("db.R", "raw_spec.R", "raw_load.R")) source(here::here("R", f))
spec <- read_raw_spec(here::here("docs", "specs", "raw-layer.yml"))
raw <- read_raw_files(here::here("data", "raw"), spec)
con <- db_connect("loader")
on.exit(DBI::dbDisconnect(con))
for (study in names(raw)) {
  n <- load_raw_study(con, paste0("raw_", study), raw[[study]])
  cli::cli_inform("raw_{study}: {sum(n)} rows in {length(n)} tables.")
}
