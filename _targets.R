# The FJF-2026 pipeline. Rebuild everything that is out of date with:
#   docker compose run --rm analysis Rscript -e "targets::tar_make()"
#
# Each phase adds its targets here: F0 the smoke test, F1 the CDISC pilot benchmark.

library(targets)

tar_option_set(packages = c("DBI", "RPostgres", "cli"))

# Shared functions; _dependencies.R only declares packages for renv and is not sourced.
tar_source(setdiff(list.files("R", pattern = "[.]R$", full.names = TRUE), "R/_dependencies.R"))

list(
  # --- F0: environment smoke test ---
  # format = "file" makes targets watch the file's contents: a change to renv.lock
  # re-runs everything downstream. Without it, targets would not notice the change.
  tar_target(lockfile, "renv.lock", format = "file"),
  tar_target(environment_info, collect_environment_info(lockfile)),
  tar_target(database_check, check_database(), cue = tar_cue(mode = "always")),
  tar_target(smoke_test, summarise_smoke_test(environment_info, database_check)),

  # --- F1: CDISC pilot benchmark (ADR 0015) ---
  # The manifest is the only input under version control. Downloaded files are file
  # targets too, so a deleted or edited file is noticed, downloaded again and re-verified.
  tar_target(pilot_manifest, "data/external/cdiscpilot01-manifest.csv", format = "file"),
  tar_target(pilot_files, fetch_pilot_files(pilot_manifest), format = "file"),
  tar_target(pilot_inventory, inventory_xpt(pilot_files))
)
