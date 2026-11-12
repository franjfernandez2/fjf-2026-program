# The FJF-2026 pipeline. Rebuild everything that is out of date with:
#   docker compose run --rm analysis Rscript -e "targets::tar_make()"
#
# Each phase adds its targets here: F0 the smoke test, F1 the CDISC pilot benchmark,
# F2 the design checks and the sample size.

library(targets)

tar_option_set(packages = c("DBI", "RPostgres", "cli"))

# Shared functions; _dependencies.R only declares packages for renv and is not sourced.
tar_source(setdiff(list.files("R", pattern = "[.]R$", full.names = TRUE), "R/_dependencies.R"))
# Study programs: the benchmark's ADaM derivations.
tar_source("studies/benchmark/adam")
# Independent QC programs (double programming).
tar_source("qc/benchmark")
# Tables, figures and their comparison with the pilot's study report.
tar_source(c("studies/benchmark/tlf", "studies/benchmark/compare"))

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
    compare_with_reference(adsl, haven::read_xpt(pilot_file(pilot_files, "adam/adsl.xpt")), keys = adam_keys()$ADSL)
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
      keys = adam_keys()$ADAE
    )
  ),
  tar_target(adtte, derive_adtte(adsl, adae, adam_spec)),
  tar_target(adtte_spec_findings, check_against_spec(adtte, adam_spec, "ADTTE")),
  tar_target(
    adtte_vs_official,
    compare_with_reference(
      adtte, haven::read_xpt(pilot_file(pilot_files, "adam/adtte.xpt")),
      keys = adam_keys()$ADTTE
    )
  ),
  tar_target(adqsadas, derive_adqsadas(benchmark_sdtm, adsl, adam_spec)),
  tar_target(adqsadas_spec_findings, check_against_spec(adqsadas, adam_spec, "ADQSADAS")),
  tar_target(
    adqsadas_vs_official,
    compare_with_reference(
      adqsadas, haven::read_xpt(pilot_file(pilot_files, "adam/adqsadas.xpt")),
      keys = adam_keys()$ADQSADAS
    )
  ),

  # Outputs, each compared with the table printed in the study report (cell by cell).
  tar_target(pilot_csr, pilot_file(pilot_files, "csr/cdiscpilot01.pdf"), format = "file"),
  tar_target(t_14_2_01, make_t_14_2_01(adsl)),
  tar_target(t_14_2_01_vs_csr, compare_t_14_2_01(t_14_2_01, pilot_csr)),
  tar_target(t_14_5_01, make_t_14_5_01(adsl, adae)),
  tar_target(t_14_5_01_vs_csr, compare_t_14_5_01(t_14_5_01, pilot_csr)),
  tar_target(t_14_3_01_fit, fit_t_14_3_01(adqsadas)),
  tar_target(t_14_3_01, make_t_14_3_01(t_14_3_01_fit)),
  tar_target(t_14_3_01_vs_csr, compare_t_14_3_01(t_14_3_01, pilot_csr)),
  # Figure 14-1 is an image in the report: it is reviewed next to the rendered page.
  tar_target(f_14_1_km, fit_f_14_1(adtte)),
  tar_target(f_14_1, make_f_14_1(f_14_1_km, "studies/benchmark/output/f_14_1.png"), format = "file"),
  tar_target(
    f_14_1_csr_page,
    render_csr_f_14_1(pilot_csr, "studies/benchmark/output/f_14_1_csr.png"),
    format = "file"
  ),

  # SAS transport files, written from the specification with xportr, then read back.
  tar_target(
    adam_xpt,
    c(
      write_adam_xpt(adsl, benchmark_metacore, "ADSL", "data/adam/cdiscpilot01"),
      write_adam_xpt(adae, benchmark_metacore, "ADAE", "data/adam/cdiscpilot01"),
      write_adam_xpt(adtte, benchmark_metacore, "ADTTE", "data/adam/cdiscpilot01"),
      write_adam_xpt(adqsadas, benchmark_metacore, "ADQSADAS", "data/adam/cdiscpilot01")
    ),
    format = "file"
  ),
  tar_target(adam_xpt_vs_official, compare_xpt_with_official(adam_xpt, pilot_files)),

  # Gate 1: every finding above, linked to the discrepancy-log rows that explain it.
  # An empty explained_by is a finding nobody has explained yet (ADR 0017).
  tar_target(discrepancy_log_file, "studies/benchmark/discrepancy-log.csv", format = "file"),
  tar_target(
    benchmark_findings,
    collect_benchmark_findings(
      adam = list(
        ADSL = adsl_vs_official, ADAE = adae_vs_official, ADTTE = adtte_vs_official,
        ADQSADAS = adqsadas_vs_official
      ),
      keys = adam_keys(),
      xpt = adam_xpt_vs_official,
      spec = rbind(adsl_spec_findings, adae_spec_findings, adtte_spec_findings, adqsadas_spec_findings),
      csr = list(`14-2.01` = t_14_2_01_vs_csr, `14-5.01` = t_14_5_01_vs_csr, `14-3.01` = t_14_3_01_vs_csr),
      qc = adsl_qc_vs_production,
      log = read_discrepancy_log(discrepancy_log_file)
    )
  ),

  # --- F2: design of the FJF-2026 studies ---
  tar_target(assumptions_file, "docs/design/assumptions.yml", format = "file"),
  tar_target(design_assumptions, read_assumptions(assumptions_file)),
  tar_target(protocol_301_file, "docs/protocols/fjf-2026-301.yml", format = "file"),
  tar_target(protocol_301, read_protocol(protocol_301_file)),
  tar_target(estimands_301_file, "docs/protocols/fjf-2026-301-estimands.yml", format = "file"),
  tar_target(estimands_301, read_estimands(estimands_301_file)),
  # Every design file must pass its checks before anything is computed from it.
  tar_target(
    design_301_problems,
    c(
      check_assumptions(design_assumptions),
      check_protocol(protocol_301, design_assumptions),
      check_estimands(estimands_301, protocol_301)
    )
  ),
  # Sample size by simulation (ADR 0020), with a public seed. Planning values only.
  tar_target(sample_size_seed, 20261112L),
  tar_target(
    sample_size_301_power,
    {
      stopifnot(length(design_301_problems) == 0)
      simulate_power_301(
        seq(100, 220, by = 10), planning_values_301(design_assumptions),
        reps = 5000, seed = sample_size_seed
      )
    }
  ),
  tar_target(sample_size_301, choose_sample_size(sample_size_301_power, target = 0.9)),
  tar_target(
    sample_size_301_robustness,
    power_robustness_301(sample_size_301, design_assumptions, reps = 5000, seed = sample_size_seed)
  ),

  # SAP of FJF-2026-301, part 1 (efficacy), checked against the estimands and the protocol.
  tar_target(sap_301_file, "docs/sap/fjf-2026-301-sap.yml", format = "file"),
  tar_target(sap_301, read_sap(sap_301_file)),
  tar_target(outputs_301_file, "docs/sap/fjf-2026-301-outputs.yml", format = "file"),
  tar_target(outputs_301, read_outputs(outputs_301_file)),
  tar_target(
    sap_301_problems,
    c(check_sap(sap_301, estimands_301, protocol_301), check_outputs(outputs_301, sap_301))
  ),
  tar_target(traceability_301_table, traceability_301(outputs_301)),
  tar_target(analysis_windows_301_table, analysis_windows_301(protocol_301)),

  # Dry run (SAP, dry run): the SAP's analyses on data generated from the planning values
  # with a public seed. Not study data, and not the simulator (ADR 0018).
  tar_target(dry_run_301_pars, planning_values_dry_run_301(design_assumptions)),
  tar_target(
    dry_run_301,
    {
      stopifnot(length(sap_301_problems) == 0)
      d <- sap_301$dry_run
      simulate_dry_run_301(
        sample_size_301, dry_run_301_pars, protocol_301, d$seed, d$missing_at_visit, d$washout_weeks,
        covariates = d$covariates
      )
    }
  ),
  # One set of HbA1c imputation draws (A1's seed) serves A1, S1-S3, B1 and the subgroups.
  tar_target(
    dry_run_301_hba1c_draws,
    {
      a1 <- sap_item(sap_301, "A1")
      draws_301(hba1c_data_301(dry_run_301, a1$settings$reference), a1$settings$imputations, a1$settings$seed)
    }
  ),
  tar_target(
    dry_run_301_efficacy,
    analyse_efficacy_301(dry_run_301$adeff, sap_301, protocol_301, draws = dry_run_301_hba1c_draws)
  ),
  tar_target(dry_run_301_secondary, analyse_secondary_301(dry_run_301, sap_301, protocol_301, dry_run_301_hba1c_draws)),
  tar_target(
    dry_run_301_multiplicity,
    test_multiplicity_301(multiplicity_inputs_301(dry_run_301_efficacy, dry_run_301_secondary, sap_301), sap_301)
  ),
  # A large trial: does each analysis recover the value of its own estimand?
  tar_target(
    dry_run_301_large,
    {
      d <- sap_301$dry_run
      simulate_dry_run_301(
        d$large_sample_per_arm, dry_run_301_pars, protocol_301, d$seed + 1, d$missing_at_visit, d$washout_weeks,
        covariates = d$covariates
      )
    }
  ),
  tar_target(
    dry_run_301_large_efficacy,
    analyse_efficacy_301(dry_run_301_large$adeff, sap_301, protocol_301, imputations = 20, deltas = c(0, 1, 2))
  ),
  # Many trials of the planned size: the power of the real primary analysis (ADR 0020).
  tar_target(
    dry_run_301_oc,
    {
      oc <- sap_301$dry_run$operating_characteristics
      operating_characteristics_301(
        sample_size_301, dry_run_301_pars, protocol_301, sap_301,
        trials = oc$trials, imputations = oc$imputations, seed = sap_301$dry_run$seed + 1000,
        cores = min(4L, parallel::detectCores())
      )
    }
  ),
  tar_target(dry_run_301_oc_summary, summarise_oc_301(dry_run_301_oc))
)
