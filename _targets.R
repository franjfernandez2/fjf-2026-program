# The FJF-2026 pipeline. Rebuild everything that is out of date with:
#   docker compose run --rm analysis Rscript -e "targets::tar_make()"
#
# Phase F0 contains only a smoke test. Each later phase adds its targets here.

library(targets)

tar_option_set(packages = c("DBI", "RPostgres", "cli"))

# Shared functions; _dependencies.R only declares packages for renv and is not sourced.
tar_source(setdiff(list.files("R", pattern = "[.]R$", full.names = TRUE), "R/_dependencies.R"))

list(
  # format = "file" makes targets watch the file's contents: a change to renv.lock
  # re-runs everything downstream. Without it, targets would not notice the change.
  tar_target(lockfile, "renv.lock", format = "file"),
  tar_target(environment_info, collect_environment_info(lockfile)),
  tar_target(database_check, check_database(), cue = tar_cue(mode = "always")),
  tar_target(smoke_test, summarise_smoke_test(environment_info, database_check))
)
