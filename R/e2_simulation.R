# Extension E2: simulation study of the efficacy analyses of FJF-2026-301 under missing data
# (docs/sap/e2-simulation-plan.yml, ADR 0036). Trials come from the dry-run generator
# (R/dry_run_301.R) with the PLANNING values and public seeds; the sealed truth and the
# simulator are never used (ADR 0018). The generator's random withdrawal is switched off and
# replaced by the mechanisms of the plan. The analyses are those of the SAP, unchanged.

#' Read the plan of the simulation study.
#' @param path Path to docs/sap/e2-simulation-plan.yml.
read_e2_plan <- function(path) yaml::read_yaml(path)

#' The cells of the plan: every mechanism at every rate.
#'
#' @param plan The result of [read_e2_plan()].
#' @return A data frame: cell (number), id (mechanism and rate), mechanism, rate, baseline,
#'   outcome (log-odds per standard deviation) and arm ("both" or "active").
e2_cells <- function(plan) {
  grid <- expand.grid(
    r = seq_along(plan$dropout$rates), m = seq_along(plan$dropout$mechanisms), KEEP.OUT.ATTRS = FALSE
  )
  cells <- lapply(seq_len(nrow(grid)), function(i) {
    rate <- plan$dropout$rates[[grid$r[i]]]
    mech <- plan$dropout$mechanisms[[grid$m[i]]]
    data.frame(
      id = paste(mech$id, rate$id, sep = "_"), mechanism = mech$id, rate = rate$rate,
      baseline = mech$baseline, outcome = mech$outcome, arm = mech$arm, stringsAsFactors = FALSE
    )
  })
  cells <- do.call(rbind, cells)
  cbind(cell = seq_len(nrow(cells)), cells)
}

#' Probability that each patient leaves the study, calibrated to a share.
#'
#' The probability is a logistic function of `lin`; its intercept is found so that the mean
#' probability is `rate`.
#' @param lin Linear predictor without intercept.
#' @param rate Share of patients who leave.
e2_leaving_probability <- function(lin, rate) {
  gap <- function(a) mean(stats::plogis(a + lin)) - rate
  a <- stats::uniroot(gap, c(-30, 10), tol = 1e-10)$root
  stats::plogis(a + lin)
}

#' One trial with patients leaving the study by a mechanism of the plan.
#'
#' Patients who leave have no HbA1c after a week drawn uniformly over the study. Who leaves
#' depends on baseline HbA1c (observed) and/or on the change at the primary visit in the full
#' data, which is not observed for someone who leaves (MNAR). Everything else (rescue,
#' discontinuation, values missing at random at single visits) is as in the dry run.
#'
#' @param n_per_arm Patients per arm.
#' @param pars Planning values, as from [planning_values_dry_run_301()].
#' @param protocol The result of [read_protocol()].
#' @param sap The result of [read_sap()] (gives the dry run's settings).
#' @param cell One row of [e2_cells()].
#' @param seed Public seed.
#' @return A list: `adeff` (HbA1c, with `CHG` missing after leaving), `full` (patient, arm and
#'   the full-data change at the primary visit) and `left` (share who left).
simulate_e2_trial <- function(n_per_arm, pars, protocol, sap, cell, seed) {
  pars$study_withdrawal <- 0
  dry <- sap$dry_run
  d <- simulate_dry_run_301(n_per_arm, pars, protocol, seed, dry$missing_at_visit, dry$washout_weeks)
  hb <- d$adeff[d$adeff$PARAMCD == "HBA1C", ]
  primary <- protocol$primary_visit_week
  full <- hb[hb$AVISITN == primary, c("USUBJID", "ARMCD", "BASE", "CHG_FULL")]
  z <- function(x) as.numeric(scale(x))
  lin <- cell$baseline * z(full$BASE) + cell$outcome * z(full$CHG_FULL)
  if (cell$arm == "active") lin <- lin * (full$ARMCD == "FJF20")
  withr::with_seed(seed + 5000000L, {
    leaves <- stats::runif(nrow(full)) < e2_leaving_probability(lin, cell$rate)
    week <- stats::runif(nrow(full), 0, primary)
  })
  leave_week <- stats::setNames(ifelse(leaves, week, Inf), full$USUBJID)
  gone <- hb$AVISITN > leave_week[as.character(hb$USUBJID)]
  hb$CHG[gone] <- NA
  hb$AVAL[gone] <- NA
  list(adeff = hb, full = full, left = mean(leaves))
}

#' The target: the treatment-policy difference at the primary visit in the full data.
#'
#' The mean of `CHG_FULL` by arm in one very large trial of the generator. The full data do
#' not depend on how patients leave, so one value serves every cell.
#'
#' @inheritParams simulate_e2_trial
#' @param per_arm Patients per arm of the large trial.
#' @return A list: value (FJF-2026 minus placebo) and its standard error.
e2_truth <- function(per_arm, pars, protocol, sap, seed) {
  pars$study_withdrawal <- 0
  dry <- sap$dry_run
  d <- simulate_dry_run_301(per_arm, pars, protocol, seed, dry$missing_at_visit, dry$washout_weeks)$adeff
  d <- d[d$PARAMCD == "HBA1C" & d$AVISITN == protocol$primary_visit_week, ]
  a <- d$CHG_FULL[d$ARMCD == "FJF20"]
  p <- d$CHG_FULL[d$ARMCD == "PBO"]
  list(value = mean(a) - mean(p), se = sqrt(stats::var(a) / length(a) + stats::var(p) / length(p)))
}

#' The analyses of the plan on one trial, and the full-data estimate as a reference.
#'
#' @param trial The result of [simulate_e2_trial()].
#' @param sap The result of [read_sap()].
#' @param protocol The result of [read_protocol()].
#' @param imputations Imputations per trial.
#' @param seed Seed of the imputation.
#' @return A tibble: analysis, estimate, se, lower, upper, p_value (NA when the fit failed).
analyse_e2_trial <- function(trial, sap, protocol, imputations, seed) {
  a1 <- sap_item(sap, "A1")
  reference <- a1$settings$reference
  visit <- sprintf("Week %d", protocol$primary_visit_week)
  empty <- tibble::tibble(estimate = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_, p_value = NA_real_)
  data <- prepare_efficacy_301(trial$adeff, reference)
  keep <- c("estimate", "se", "lower", "upper", "p_value")
  attempt <- function(f) tryCatch(f()[, keep], error = function(e) empty)
  mmrm <- attempt(function() fit_mmrm_301(data, visit, unlist(sap_item(sap, "A2")$settings$covariance_fallback)))
  draws <- tryCatch(draws_301(data, imputations, seed), error = function(e) NULL)
  mi <- function(strategy) {
    if (is.null(draws)) {
      return(empty)
    }
    attempt(function() pooled_ancova_301(draws, data, strategy, reference, visit, seed))
  }
  f <- trial$full
  test <- stats::t.test(f$CHG_FULL[f$ARMCD == "FJF20"], f$CHG_FULL[f$ARMCD == reference])
  full <- tibble::tibble(
    estimate = unname(test$estimate[1] - test$estimate[2]), se = unname(test$stderr),
    lower = test$conf.int[1], upper = test$conf.int[2], p_value = test$p.value
  )
  dplyr::bind_rows(
    dplyr::mutate(full, analysis = "FULL"),
    dplyr::mutate(mmrm, analysis = "MMRM"),
    dplyr::mutate(mi("JR"), analysis = "MI_J2R"),
    dplyr::mutate(mi("MAR"), analysis = "MI_MAR")
  ) |>
    dplyr::relocate("analysis")
}

#' Run the trials of one cell.
#'
#' Trial i of cell c uses seed `plan$seed + 1000 * c + i`, so a cell does not depend on the
#' others or on the number of cores.
#'
#' @param cell One row of [e2_cells()].
#' @inheritParams simulate_e2_trial
#' @param plan The result of [read_e2_plan()].
#' @param n_per_arm Patients per arm.
#' @param trials,imputations Number of trials and of imputations (default: the plan's).
#' @param cores Parallel workers.
#' @return A tibble with one row per trial and analysis, and the share who left.
run_e2_cell <- function(cell, plan, n_per_arm, pars, protocol, sap, trials = NULL, imputations = NULL, cores = 1) {
  trials <- trials %||% plan$trials_per_cell
  imputations <- imputations %||% plan$imputations
  one <- function(i) {
    seed <- as.integer(plan$seed + 1000L * cell$cell + i)
    trial <- simulate_e2_trial(n_per_arm, pars, protocol, sap, cell, seed)
    left <- trial$left
    analyse_e2_trial(trial, sap, protocol, imputations, seed) |>
      dplyr::mutate(trial = i, left = left)
  }
  runs <- if (cores > 1) parallel::mclapply(seq_len(trials), one, mc.cores = cores) else lapply(seq_len(trials), one)
  failed <- vapply(runs, inherits, TRUE, "try-error")
  if (any(failed)) stop("trials failed in cell ", cell$id)
  number <- cell$cell
  label <- cell$id
  dplyr::bind_rows(runs) |> dplyr::mutate(cell = number, cell_id = label, .before = 1)
}

#' Run every cell of the plan.
#' @inheritParams run_e2_cell
#' @return The trials of every cell, bound.
run_e2 <- function(plan, n_per_arm, pars, protocol, sap, trials = NULL, imputations = NULL, cores = 1) {
  cells <- e2_cells(plan)
  dplyr::bind_rows(lapply(seq_len(nrow(cells)), function(i) {
    run_e2_cell(cells[i, ], plan, n_per_arm, pars, protocol, sap, trials, imputations, cores)
  }))
}

#' Measures of the plan, with their Monte Carlo errors, by cell and analysis.
#'
#' Bias is the mean estimate minus the target; its Monte Carlo error is the empirical standard
#' deviation over the square root of the number of trials. Coverage is the share of 95%
#' intervals that contain the target; power is the share with p < 0.05 and an estimate below 0.
#' Trials where the analysis failed are counted and left out.
#'
#' @param trials The result of [run_e2()].
#' @param truth The result of [e2_truth()].
#' @param cells The result of [e2_cells()].
#' @return A tibble, one row per cell and analysis.
summarise_e2 <- function(trials, truth, cells) {
  trials |>
    dplyr::group_by(.data$cell, .data$cell_id, .data$analysis) |>
    dplyr::summarise(
      trials = dplyr::n(),
      failed = sum(is.na(.data$estimate)),
      left = mean(.data$left),
      mean_estimate = mean(.data$estimate, na.rm = TRUE),
      bias = .data$mean_estimate - truth$value,
      empirical_sd = stats::sd(.data$estimate, na.rm = TRUE),
      bias_mcse = .data$empirical_sd / sqrt(sum(!is.na(.data$estimate))),
      mean_se = mean(.data$se, na.rm = TRUE),
      coverage = mean(.data$lower <= truth$value & truth$value <= .data$upper, na.rm = TRUE),
      coverage_mcse = sqrt(.data$coverage * (1 - .data$coverage) / sum(!is.na(.data$estimate))),
      power = mean(.data$p_value < 0.05 & .data$estimate < 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(cells[, c("cell", "mechanism", "rate")], by = "cell") |>
    dplyr::relocate("mechanism", "rate", .after = "cell_id")
}
