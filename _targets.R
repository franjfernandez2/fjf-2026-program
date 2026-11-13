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
tar_source("studies/301/sdtm")
tar_source("qc/301")
tar_source("studies/301/adam")
tar_source("studies/301/analysis")
tar_source("studies/301/tlf")
tar_source("studies/201/analysis")

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
  # What the analyses read: a new version number or status alone reruns nothing.
  tar_target(sap_301_content, sap_analysis_content(sap_301)),
  tar_target(outputs_301_file, "docs/sap/fjf-2026-301-outputs.yml", format = "file"),
  tar_target(outputs_301, read_outputs(outputs_301_file)),
  tar_target(
    sap_301_problems,
    c(check_sap(sap_301_content, estimands_301, protocol_301), check_outputs(outputs_301, sap_301_content))
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
      d <- sap_301_content$dry_run
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
      a1 <- sap_item(sap_301_content, "A1")
      draws_301(hba1c_data_301(dry_run_301, a1$settings$reference), a1$settings$imputations, a1$settings$seed)
    }
  ),
  tar_target(
    dry_run_301_efficacy,
    analyse_efficacy_301(dry_run_301$adeff, sap_301_content, protocol_301, draws = dry_run_301_hba1c_draws)
  ),
  tar_target(
    dry_run_301_secondary,
    analyse_secondary_301(dry_run_301, sap_301_content, protocol_301, dry_run_301_hba1c_draws)
  ),
  tar_target(
    dry_run_301_multiplicity,
    test_multiplicity_301(
      multiplicity_inputs_301(dry_run_301_efficacy, dry_run_301_secondary, sap_301_content), sap_301_content
    )
  ),
  # A large trial: does each analysis recover the value of its own estimand?
  tar_target(
    dry_run_301_large,
    {
      d <- sap_301_content$dry_run
      simulate_dry_run_301(
        d$large_sample_per_arm, dry_run_301_pars, protocol_301, d$seed + 1, d$missing_at_visit, d$washout_weeks,
        covariates = d$covariates
      )
    }
  ),
  tar_target(
    dry_run_301_large_efficacy,
    analyse_efficacy_301(dry_run_301_large$adeff, sap_301_content, protocol_301, imputations = 20, deltas = c(0, 1, 2))
  ),
  # Many trials of the planned size: the power of the real primary analysis (ADR 0020).
  tar_target(
    dry_run_301_oc,
    {
      oc <- sap_301_content$dry_run$operating_characteristics
      operating_characteristics_301(
        sample_size_301, dry_run_301_pars, protocol_301, sap_301_content,
        trials = oc$trials, imputations = oc$imputations, seed = sap_301_content$dry_run$seed + 1000,
        cores = min(4L, parallel::detectCores())
      )
    }
  ),
  tar_target(dry_run_301_oc_summary, summarise_oc_301(dry_run_301_oc)),

  # --- FJF-2026-201 (phase II, dose finding): design, SAP and dry run ---
  tar_target(protocol_201_file, "docs/protocols/fjf-2026-201.yml", format = "file"),
  tar_target(protocol_201, read_protocol(protocol_201_file)),
  tar_target(estimands_201_file, "docs/protocols/fjf-2026-201-estimands.yml", format = "file"),
  tar_target(estimands_201, read_estimands(estimands_201_file)),
  tar_target(sap_201_file, "docs/sap/fjf-2026-201-sap.yml", format = "file"),
  tar_target(sap_201, read_sap(sap_201_file)),
  tar_target(sap_201_content, sap_analysis_content(sap_201)),
  # The phase III dose is read from the 301 design, so the two studies cannot disagree.
  tar_target(reference_dose_201, max(purrr::map_dbl(protocol_301$arms, "dose_mg"))),
  tar_target(
    design_201_problems,
    c(
      check_protocol(protocol_201, design_assumptions),
      check_estimands(estimands_201, protocol_201),
      check_sap_201(sap_201_content, estimands_201, protocol_201)
    )
  ),
  tar_target(
    planning_curve_201_values,
    planning_curve_201(
      design_assumptions, min(setdiff(purrr::map_dbl(protocol_201$arms, "dose_mg"), 0)), reference_dose_201
    )
  ),
  tar_target(
    sample_size_201_power,
    {
      stopifnot(length(design_201_problems) == 0)
      power_mcp_201(design_assumptions, protocol_201, sap_201_content, reference_dose_201)
    }
  ),
  tar_target(sample_size_201, choose_sample_size_201(sample_size_201_power, sap_201_content$sample_size$target_power)),
  tar_target(
    dry_run_201,
    simulate_dry_run_201(
      sample_size_201, design_assumptions, protocol_201, reference_dose_201, sap_201_content$dry_run$seed,
      sap_201_content$dry_run$missing_at_visit
    )
  ),
  tar_target(dry_run_201_analysis, analyse_201(dry_run_201$adeff, sap_201_content, protocol_201)),
  tar_target(
    dry_run_201_oc,
    operating_characteristics_201(
      sample_size_201, design_assumptions, protocol_201, sap_201_content, reference_dose_201,
      trials = sap_201_content$dry_run$operating_characteristics$trials, seed = sap_201_content$dry_run$seed + 1000,
      cores = min(4L, parallel::detectCores())
    )
  ),

  # --- Raw study data (data/raw, generated once from the sealed seed): data quality ---
  # The checks count what an EDC let through; cleaning is F3's job. No treatment arm is used.
  tar_target(raw_spec_file, "docs/specs/raw-layer.yml", format = "file"),
  tar_target(raw_spec, read_raw_spec(raw_spec_file)),
  tar_target(raw_manifest_file, "data/raw/manifest.csv", format = "file"),
  tar_target(raw_data, read_raw_files(dirname(raw_manifest_file), raw_spec)),
  tar_target(
    raw_quality,
    rbind(
      check_raw_quality(raw_data[["201"]], raw_spec, protocol_201, "201"),
      check_raw_quality(raw_data[["301"]], raw_spec, protocol_301, "301")
    )
  ),

  # --- FJF-2026-301: SDTM (F3). Blinded until the database lock (ADR 0027): before it, the
  # pipeline uses a dummy assignment, the real arms shuffled within strata with a public seed.
  tar_target(sdtm_301_spec_file, "docs/specs/sdtm-301.yml", format = "file"),
  tar_target(sdtm_301_spec, yaml::read_yaml(sdtm_301_spec_file)),
  tar_target(dblock_301, database_locked(lock_file("301")), cue = tar_cue(mode = "always")),
  tar_target(
    assignment_301,
    assignment_in_use(raw_data[["301"]]$randomisation, dblock_301, sdtm_301_spec$blinding_seed)
  ),
  tar_target(sdtm_301_dm, map_dm_301(raw_data[["301"]], assignment_301, protocol_301, sdtm_301_spec)),
  tar_target(sdtm_301_se, map_se_301(sdtm_301_dm, protocol_301, sdtm_301_spec)),
  tar_target(sdtm_301_suppdm, map_suppdm_301(raw_data[["301"]], sdtm_301_spec)),
  tar_target(sdtm_301_ie, map_ie_301(raw_data[["301"]], sdtm_301_dm, protocol_301, sdtm_301_spec)),
  tar_target(sdtm_301_ta, map_ta_301(protocol_301, sdtm_301_spec)),
  tar_target(sdtm_301_te, map_te_301(protocol_301, sdtm_301_spec)),
  tar_target(sdtm_301_tv, map_tv_301(protocol_301, sdtm_301_spec)),
  tar_target(sdtm_301_ti, map_ti_301(protocol_301, sdtm_301_spec)),
  tar_target(sdtm_301_ts, map_ts_301(
    protocol_301, sdtm_301_spec, sdtm_301_dm, 2L * sample_size_301, purrr::map_chr(sap_301_content$secondary, "label")
  )),
  tar_target(sdtm_301_ds, map_ds_301(raw_data[["301"]], assignment_301, sdtm_301_dm, sdtm_301_spec, sdtm_301_se)),
  tar_target(sdtm_301_sv, map_sv_301(raw_data[["301"]], sdtm_301_dm, protocol_301, sdtm_301_spec)),
  tar_target(coding_dictionary_file, "docs/specs/coding-dictionary.yml", format = "file"),
  tar_target(coding_dictionary, yaml::read_yaml(coding_dictionary_file)),
  tar_target(
    sdtm_301_ex, map_ex_301(raw_data[["301"]], assignment_301, sdtm_301_dm, protocol_301, sdtm_301_spec, sdtm_301_se)
  ),
  tar_target(sdtm_301_da, map_da_301(raw_data[["301"]], sdtm_301_dm, protocol_301, sdtm_301_spec, sdtm_301_se)),
  tar_target(sdtm_301_cm, map_cm_301(raw_data[["301"]], sdtm_301_dm, coding_dictionary, sdtm_301_spec, sdtm_301_se)),
  tar_target(sdtm_301_ae_supp, map_ae_301(
    raw_data[["301"]], sdtm_301_dm, coding_dictionary, sdtm_301_spec,
    sdtm_301_se
  )),
  tar_target(sdtm_301_ae, sdtm_301_ae_supp$ae),
  tar_target(sdtm_301_suppae, sdtm_301_ae_supp$suppae),
  tar_target(sdtm_301_lb_issues, map_lb_301(
    raw_data[["301"]], raw_spec, sdtm_301_dm, protocol_301, sdtm_301_spec,
    sdtm_301_se
  )),
  tar_target(sdtm_301_lb, sdtm_301_lb_issues$lb),
  tar_target(sdtm_301_vs_issues, map_vs_301(
    raw_data[["301"]], raw_spec, sdtm_301_dm, protocol_301, sdtm_301_spec,
    sdtm_301_se
  )),
  tar_target(sdtm_301_vs, sdtm_301_vs_issues$vs),
  tar_target(sdtm_301_ce_supp, map_ce_301(raw_data[["301"]], sdtm_301_dm, sdtm_301_spec, sdtm_301_se)),
  tar_target(sdtm_301_ce, sdtm_301_ce_supp$ce),
  tar_target(sdtm_301_suppce, sdtm_301_ce_supp$suppce),
  tar_target(
    sdtm_301_data_issues,
    rbind(
      sdtm_301_lb_issues$issues, sdtm_301_vs_issues$issues,
      ex_issues_301(raw_data[["301"]], sdtm_301_dm, sdtm_301_spec)
    )
  ),
  tar_target(
    sdtm_301,
    list(
      DM = sdtm_301_dm, SUPPDM = sdtm_301_suppdm, DS = sdtm_301_ds, SV = sdtm_301_sv, EX = sdtm_301_ex,
      DA = sdtm_301_da,
      CM = sdtm_301_cm, AE = sdtm_301_ae, SUPPAE = sdtm_301_suppae, LB = sdtm_301_lb, VS = sdtm_301_vs,
      CE = sdtm_301_ce, SUPPCE = sdtm_301_suppce, IE = sdtm_301_ie, SE = sdtm_301_se, TA = sdtm_301_ta,
      TE = sdtm_301_te, TV = sdtm_301_tv, TI = sdtm_301_ti, TS = sdtm_301_ts
    )[sdtm_domains(sdtm_301_spec)]
  ),
  tar_target(
    sdtm_301_qc,
    qc_sdtm_301(
      file.path(dirname(raw_manifest_file), "301"), assignment_301, protocol_301, sdtm_301_spec, coding_dictionary
    )
  ),
  tar_target(sdtm_301_qc_comparison, compare_sdtm_301(sdtm_301, sdtm_301_qc, sdtm_301_spec)),
  # The double programming must agree; a difference stops the pipeline until it is explained.
  tar_target(sdtm_301_qc_check, stopifnot("SDTM double programming differs" = all(sdtm_301_qc_comparison$clean))),
  tar_target(
    sdtm_301_files,
    unlist(purrr::imap(sdtm_301, \(d, name) write_sdtm_domain(d, sdtm_301_spec, name, "data/sdtm/301"))),
    format = "file"
  ),
  tar_target(
    sdtm_301_define,
    write_sdtm_define(sdtm_301, sdtm_301_spec, "data/sdtm/301/define.xml", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
    format = "file"
  ),
  # --- FJF-2026-201: SDTM (F4), with the programs of FJF-2026-301 (ADR 0030). Blinded until
  # its own database lock, as FJF-2026-301 (ADR 0027).
  tar_target(sdtm_201_spec_file, "docs/specs/sdtm-201.yml", format = "file"),
  tar_target(sdtm_201_spec, yaml::read_yaml(sdtm_201_spec_file)),
  tar_target(dblock_201, database_locked(lock_file("201")), cue = tar_cue(mode = "always")),
  tar_target(
    assignment_201,
    assignment_in_use(raw_data[["201"]]$randomisation, dblock_201, sdtm_201_spec$blinding_seed)
  ),
  tar_target(sdtm_201_dm, map_dm_301(raw_data[["201"]], assignment_201, protocol_201, sdtm_201_spec)),
  tar_target(sdtm_201_se, map_se_301(sdtm_201_dm, protocol_201, sdtm_201_spec)),
  tar_target(sdtm_201_suppdm, map_suppdm_301(raw_data[["201"]], sdtm_201_spec)),
  tar_target(sdtm_201_ie, map_ie_301(raw_data[["201"]], sdtm_201_dm, protocol_201, sdtm_201_spec)),
  tar_target(sdtm_201_ta, map_ta_301(protocol_201, sdtm_201_spec)),
  tar_target(sdtm_201_te, map_te_301(protocol_201, sdtm_201_spec)),
  tar_target(sdtm_201_tv, map_tv_301(protocol_201, sdtm_201_spec)),
  tar_target(sdtm_201_ti, map_ti_301(protocol_201, sdtm_201_spec)),
  tar_target(sdtm_201_ts, map_ts_301(
    protocol_201, sdtm_201_spec, sdtm_201_dm, length(protocol_201$arms) * sample_size_201,
    unlist(sdtm_201_spec$trial_summary$objective_secondary)
  )),
  tar_target(sdtm_201_ds, map_ds_301(raw_data[["201"]], assignment_201, sdtm_201_dm, sdtm_201_spec, sdtm_201_se)),
  tar_target(sdtm_201_sv, map_sv_301(raw_data[["201"]], sdtm_201_dm, protocol_201, sdtm_201_spec)),
  tar_target(
    sdtm_201_ex, map_ex_301(raw_data[["201"]], assignment_201, sdtm_201_dm, protocol_201, sdtm_201_spec, sdtm_201_se)
  ),
  tar_target(sdtm_201_da, map_da_301(raw_data[["201"]], sdtm_201_dm, protocol_201, sdtm_201_spec, sdtm_201_se)),
  tar_target(sdtm_201_cm, map_cm_301(raw_data[["201"]], sdtm_201_dm, coding_dictionary, sdtm_201_spec, sdtm_201_se)),
  tar_target(sdtm_201_ae_supp, map_ae_301(
    raw_data[["201"]], sdtm_201_dm, coding_dictionary, sdtm_201_spec,
    sdtm_201_se
  )),
  tar_target(sdtm_201_ae, sdtm_201_ae_supp$ae),
  tar_target(sdtm_201_suppae, sdtm_201_ae_supp$suppae),
  tar_target(sdtm_201_lb_issues, map_lb_301(
    raw_data[["201"]], raw_spec, sdtm_201_dm, protocol_201, sdtm_201_spec,
    sdtm_201_se
  )),
  tar_target(sdtm_201_lb, sdtm_201_lb_issues$lb),
  tar_target(sdtm_201_vs_issues, map_vs_301(
    raw_data[["201"]], raw_spec, sdtm_201_dm, protocol_201, sdtm_201_spec,
    sdtm_201_se
  )),
  tar_target(sdtm_201_vs, sdtm_201_vs_issues$vs),
  tar_target(sdtm_201_ce_supp, map_ce_301(raw_data[["201"]], sdtm_201_dm, sdtm_201_spec, sdtm_201_se)),
  tar_target(sdtm_201_ce, sdtm_201_ce_supp$ce),
  tar_target(sdtm_201_suppce, sdtm_201_ce_supp$suppce),
  tar_target(
    sdtm_201_data_issues,
    rbind(
      sdtm_201_lb_issues$issues, sdtm_201_vs_issues$issues,
      ex_issues_301(raw_data[["201"]], sdtm_201_dm, sdtm_201_spec)
    )
  ),
  tar_target(
    sdtm_201,
    list(
      DM = sdtm_201_dm, SUPPDM = sdtm_201_suppdm, DS = sdtm_201_ds, SV = sdtm_201_sv, EX = sdtm_201_ex,
      DA = sdtm_201_da,
      CM = sdtm_201_cm, AE = sdtm_201_ae, SUPPAE = sdtm_201_suppae, LB = sdtm_201_lb, VS = sdtm_201_vs,
      CE = sdtm_201_ce, SUPPCE = sdtm_201_suppce, IE = sdtm_201_ie, SE = sdtm_201_se, TA = sdtm_201_ta,
      TE = sdtm_201_te, TV = sdtm_201_tv, TI = sdtm_201_ti, TS = sdtm_201_ts
    )[sdtm_domains(sdtm_201_spec)]
  ),
  tar_target(
    sdtm_201_qc,
    qc_sdtm_301(
      file.path(dirname(raw_manifest_file), "201"), assignment_201, protocol_201, sdtm_201_spec, coding_dictionary
    )
  ),
  tar_target(sdtm_201_qc_comparison, compare_sdtm_301(sdtm_201, sdtm_201_qc, sdtm_201_spec)),
  # The double programming must agree; a difference stops the pipeline until it is explained.
  tar_target(sdtm_201_qc_check, stopifnot("SDTM double programming differs" = all(sdtm_201_qc_comparison$clean))),
  tar_target(
    sdtm_201_files,
    unlist(purrr::imap(sdtm_201, \(d, name) write_sdtm_domain(d, sdtm_201_spec, name, "data/sdtm/201"))),
    format = "file"
  ),
  tar_target(
    sdtm_201_define,
    write_sdtm_define(sdtm_201, sdtm_201_spec, "data/sdtm/201/define.xml", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
    format = "file"
  ),
  # --- FJF-2026-201: ADaM (F4), with the programs of FJF-2026-301 (ADR 0030).
  tar_target(adam_201_spec_file, "docs/specs/adam-201.yml", format = "file"),
  tar_target(adam_201_spec, yaml::read_yaml(adam_201_spec_file)),
  tar_target(adam_201_adsl_base, derive_adsl_301(sdtm_201, protocol_201, adam_201_spec)),
  tar_target(adam_201_adeff_base, derive_adeff_301(sdtm_201, adam_201_adsl_base, protocol_201, adam_201_spec)),
  tar_target(adam_201_pps, add_pps_301(adam_201_adsl_base, adam_201_adeff_base, protocol_201)),
  tar_target(adam_201_adsl, adam_201_pps$adsl),
  tar_target(adam_201_adeff, adam_201_pps$adeff),
  tar_target(adam_201_adex, derive_adex_301(adam_201_adsl, adam_201_spec)),
  tar_target(adam_201_adae, derive_adae_301(sdtm_201, adam_201_adsl, adam_201_spec)),
  tar_target(adam_201_adhypo, derive_adhypo_301(sdtm_201, adam_201_adsl, adam_201_spec)),
  tar_target(adam_201_adlb, derive_adlb_301(sdtm_201, adam_201_adsl, protocol_201, adam_201_spec)),
  tar_target(adam_201_advs, derive_advs_301(sdtm_201, adam_201_adsl, protocol_201, adam_201_spec)),
  tar_target(adam_201_adtte, derive_adtte_301(adam_201_adsl, adam_201_adhypo, protocol_201, adam_201_spec)),
  tar_target(
    adam_201_files,
    c(
      write_adam_dataset(adam_201_adsl, adam_201_spec, "ADSL", "data/adam/201"),
      write_adam_dataset(adam_201_adeff, adam_201_spec, "ADEFF", "data/adam/201"),
      write_adam_dataset(adam_201_adtte, adam_201_spec, "ADTTE", "data/adam/201"),
      write_adam_dataset(adam_201_adex, adam_201_spec, "ADEX", "data/adam/201"),
      write_adam_dataset(adam_201_adae, adam_201_spec, "ADAE", "data/adam/201"),
      write_adam_dataset(adam_201_adhypo, adam_201_spec, "ADHYPO", "data/adam/201"),
      write_adam_dataset(adam_201_adlb, adam_201_spec, "ADLB", "data/adam/201"),
      write_adam_dataset(adam_201_advs, adam_201_spec, "ADVS", "data/adam/201")
    ),
    format = "file"
  ),
  tar_target(
    adam_201,
    list(
      ADSL = adam_201_adsl, ADEFF = adam_201_adeff, ADTTE = adam_201_adtte, ADEX = adam_201_adex,
      ADAE = adam_201_adae, ADHYPO = adam_201_adhypo, ADLB = adam_201_adlb, ADVS = adam_201_advs
    )
  ),
  # Second programming of ADaM (qc/301, ADR 0030), from the SDTM files written above.
  tar_target(adam_201_qc, {
    sdtm_201_files
    qc_adam_301("data/sdtm/201", protocol_201, adam_201_spec$vital_sign_limits)
  }),
  tar_target(adam_201_qc_comparison, compare_adam_301(adam_201, adam_201_qc)),
  tar_target(adam_201_qc_check, stopifnot(all(adam_201_qc_comparison$clean))),
  # The SAP's analyses (generalised MCP-Mod, model averaging, all observed values) on the study
  # data, with the F2 code unchanged. Until the lock the arms are the dummy assignment.
  tar_target(analysis_201_data, {
    adam_201_qc_check
    analysis_data_201(adam_201_adeff, protocol_201)
  }),
  tar_target(analysis_201, analyse_201(analysis_201_data, sap_201_content, protocol_201)),
  tar_target(adam_301_spec_file, "docs/specs/adam-301.yml", format = "file"),
  tar_target(adam_301_spec, yaml::read_yaml(adam_301_spec_file)),
  tar_target(adam_301_adsl_base, derive_adsl_301(sdtm_301, protocol_301, adam_301_spec)),
  tar_target(adam_301_adeff_base, derive_adeff_301(sdtm_301, adam_301_adsl_base, protocol_301, adam_301_spec)),
  tar_target(adam_301_pps, add_pps_301(adam_301_adsl_base, adam_301_adeff_base, protocol_301)),
  tar_target(adam_301_adsl, adam_301_pps$adsl),
  tar_target(adam_301_adeff, adam_301_pps$adeff),
  tar_target(adam_301_adex, derive_adex_301(adam_301_adsl, adam_301_spec)),
  tar_target(adam_301_adae, derive_adae_301(sdtm_301, adam_301_adsl, adam_301_spec)),
  tar_target(adam_301_adhypo, derive_adhypo_301(sdtm_301, adam_301_adsl, adam_301_spec)),
  tar_target(adam_301_adlb, derive_adlb_301(sdtm_301, adam_301_adsl, protocol_301, adam_301_spec)),
  tar_target(adam_301_advs, derive_advs_301(sdtm_301, adam_301_adsl, protocol_301, adam_301_spec)),
  tar_target(adam_301_adtte, derive_adtte_301(adam_301_adsl, adam_301_adhypo, protocol_301, adam_301_spec)),
  tar_target(
    adam_301_files,
    c(
      write_adam_dataset(adam_301_adsl, adam_301_spec, "ADSL", "data/adam/301"),
      write_adam_dataset(adam_301_adeff, adam_301_spec, "ADEFF", "data/adam/301"),
      write_adam_dataset(adam_301_adtte, adam_301_spec, "ADTTE", "data/adam/301"),
      write_adam_dataset(adam_301_adex, adam_301_spec, "ADEX", "data/adam/301"),
      write_adam_dataset(adam_301_adae, adam_301_spec, "ADAE", "data/adam/301"),
      write_adam_dataset(adam_301_adhypo, adam_301_spec, "ADHYPO", "data/adam/301"),
      write_adam_dataset(adam_301_adlb, adam_301_spec, "ADLB", "data/adam/301"),
      write_adam_dataset(adam_301_advs, adam_301_spec, "ADVS", "data/adam/301")
    ),
    format = "file"
  ),
  tar_target(
    adam_301,
    list(
      ADSL = adam_301_adsl, ADEFF = adam_301_adeff, ADTTE = adam_301_adtte, ADEX = adam_301_adex,
      ADAE = adam_301_adae, ADHYPO = adam_301_adhypo, ADLB = adam_301_adlb, ADVS = adam_301_advs
    )
  ),
  # Second programming of ADaM, from the SDTM files written above.
  tar_target(adam_301_qc, {
    sdtm_301_files
    qc_adam_301("data/sdtm/301", protocol_301, adam_301_spec$vital_sign_limits)
  }),
  tar_target(adam_301_qc_comparison, compare_adam_301(adam_301, adam_301_qc)),
  tar_target(adam_301_qc_check, stopifnot(all(adam_301_qc_comparison$clean))),
  # The SAP's analyses on the study data, with the F2 code unchanged. Until the lock the arms
  # are the dummy assignment (ADR 0027), so these results carry no information on the drug.
  # Only the datasets the analyses read, so a change to a safety dataset does not rerun them.
  tar_target(analysis_301_data, {
    adam_301_qc_check
    analysis_data_301(list(ADSL = adam_301_adsl, ADEFF = adam_301_adeff, ADTTE = adam_301_adtte))
  }),
  tar_target(
    analysis_301_hba1c_draws,
    {
      a1 <- sap_item(sap_301_content, "A1")
      draws_301(hba1c_data_301(analysis_301_data, a1$settings$reference), a1$settings$imputations, a1$settings$seed)
    }
  ),
  tar_target(
    analysis_301_efficacy,
    analyse_efficacy_301(analysis_301_data$adeff, sap_301_content, protocol_301, draws = analysis_301_hba1c_draws)
  ),
  tar_target(
    analysis_301_secondary,
    analyse_secondary_301(analysis_301_data, sap_301_content, protocol_301, analysis_301_hba1c_draws)
  ),
  tar_target(
    analysis_301_multiplicity,
    test_multiplicity_301(
      multiplicity_inputs_301(analysis_301_efficacy, analysis_301_secondary, sap_301_content), sap_301_content
    )
  ),
  # Tables, figures and listings (docs/sap/fjf-2026-301-outputs.yml). Until the lock they show
  # the dummy arms and say so (ADR 0027).
  tar_target(
    analysis_301_lsmeans,
    analysis_lsmeans_301(
      analysis_301_data, analysis_301_hba1c_draws, analysis_301_efficacy, analysis_301_secondary, sap_301_content,
      protocol_301
    )
  ),
  tar_target(tlf_301_analysis, list(
    efficacy = analysis_301_efficacy, secondary = analysis_301_secondary, multiplicity = analysis_301_multiplicity,
    lsmeans = analysis_301_lsmeans, sap = sap_301_content, protocol = protocol_301
  )),
  tar_target(tlf_301_tables, tables_301(adam_301, tlf_301_analysis, outputs_301)),
  tar_target(
    tlf_301_files,
    write_tlf_301(tlf_301_tables, adam_301, tlf_301_analysis, outputs_301, dblock_301, "studies/301/output/tlf"),
    format = "file"
  ),
  tar_target(tlf_301_qc, qc_tlf_301(adam_301, tlf_301_analysis)),
  tar_target(tlf_301_qc_comparison, compare_tlf_301(tlf_301_tables, tlf_301_qc)),
  tar_target(tlf_301_qc_check, stopifnot(tlf_301_qc_comparison$clean)),
  tar_target(
    adam_301_define,
    write_adam_define(adam_301, adam_301_spec, "data/adam/301/define.xml", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
    format = "file"
  ),
  tar_target(
    adam_201_define,
    write_adam_define(adam_201, adam_201_spec, "data/adam/201/define.xml", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
    format = "file"
  )
)
