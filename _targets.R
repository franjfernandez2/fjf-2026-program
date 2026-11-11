# The FJF-2026 pipeline. Rebuild everything that is out of date with:
#   docker compose run --rm analysis Rscript -e "targets::tar_make()"
#
# Each phase adds its targets here: F0 the smoke test, F1 the CDISC pilot benchmark.

library(targets)

tar_option_set(packages = c("DBI", "RPostgres", "cli"))

# Shared functions; _dependencies.R only declares packages for renv and is not sourced.
tar_source(setdiff(list.files("R", pattern = "[.]R$", full.names = TRUE), "R/_dependencies.R"))
# Study programs: the benchmark's ADaM derivations.
tar_source("studies/benchmark/adam")
# Independent QC programs (double programming).
tar_source("qc/benchmark")

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
  tar_target(pilot_inventory, inventory_xpt(pilot_files)),

  # Specifications (ADR 0016): read from the pilot's own ADaM define.xml (Define-XML 1.0).
  tar_target(pilot_adam_define, pilot_file(pilot_files, "adam/define.xml"), format = "file"),
  tar_target(adam_spec, read_define_v1(pilot_adam_define)),
  tar_target(benchmark_datasets, c("ADSL", "ADAE", "ADTTE", "ADQSADAS")),
  tar_target(benchmark_metacore, define_v1_to_metacore(adam_spec, benchmark_datasets)),

  # Is the pilot consistent with itself? Its official ADaM against its own specification,
  # and its analysis results metadata against its own parameter lists.
  tar_target(
    pilot_spec_findings,
    do.call(rbind, lapply(benchmark_datasets, function(ds) {
      official <- haven::read_xpt(pilot_file(pilot_files, sprintf("adam/%s.xpt", tolower(ds))))
      check_against_spec(official, adam_spec, ds)
    }))
  ),
  tar_target(pilot_arm_findings, check_analysis_results(adam_spec)),

  # ADaM derived from the pilot's SDTM, then checked against the specification and against
  # the official dataset. Every difference is in studies/benchmark/discrepancy-log.csv.
  tar_target(benchmark_sdtm, read_sdtm(pilot_files, c("ae", "dm", "ds", "ex", "mh", "qs", "sc", "sv", "vs"))),
  tar_target(adsl, derive_adsl(benchmark_sdtm, adam_spec)),
  tar_target(adsl_spec_findings, check_against_spec(adsl, adam_spec, "ADSL")),
  tar_target(
    adsl_vs_official,
    compare_with_reference(adsl, haven::read_xpt(pilot_file(pilot_files, "adam/adsl.xpt")), keys = "USUBJID")
  ),
  # Double programming: the QC ADSL must match production with no difference at all.
  tar_target(adsl_qc, qc_adsl(pilot_files, adam_spec)),
  tar_target(adsl_qc_vs_production, compare_with_reference(adsl_qc, adsl, keys = "USUBJID")),
  tar_target(adae, derive_adae(benchmark_sdtm, adsl, adam_spec)),
  tar_target(adae_spec_findings, check_against_spec(adae, adam_spec, "ADAE")),
  tar_target(
    adae_vs_official,
    compare_with_reference(
      adae, haven::read_xpt(pilot_file(pilot_files, "adam/adae.xpt")),
      keys = c("USUBJID", "AESEQ")
    )
  ),
  tar_target(adtte, derive_adtte(adsl, adae, adam_spec)),
  tar_target(adtte_spec_findings, check_against_spec(adtte, adam_spec, "ADTTE")),
  tar_target(
    adtte_vs_official,
    compare_with_reference(
      adtte, haven::read_xpt(pilot_file(pilot_files, "adam/adtte.xpt")),
      keys = c("USUBJID", "PARAMCD")
    )
  ),
  tar_target(adqsadas, derive_adqsadas(benchmark_sdtm, adsl, adam_spec)),
  tar_target(adqsadas_spec_findings, check_against_spec(adqsadas, adam_spec, "ADQSADAS")),
  tar_target(
    adqsadas_vs_official,
    compare_with_reference(
      adqsadas, haven::read_xpt(pilot_file(pilot_files, "adam/adqsadas.xpt")),
      keys = c("USUBJID", "PARAMCD", "AVISIT", "ADT")
    )
  )
)
