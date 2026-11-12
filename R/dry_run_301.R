# Dry-run data for the FJF-2026-301 SAP. Longitudinal HbA1c generated from the PLANNING
# values with a public seed, so the SAP's analyses can be run and checked before any study
# data exist. This is not the simulator (simulator/, ADR 0018): it never reads the sealed
# truth, models no laboratory, rescue rule or data errors, and its data are not study data.

#' Planning values the dry run needs, from the design assumptions.
#'
#' @param assumptions The result of [read_assumptions()].
#' @return A named list of numbers: the sample-size values plus baseline and correlation.
planning_values_dry_run_301 <- function(assumptions) {
  p <- assumptions$parameters
  extra <- c(
    "hba1c_baseline_mean", "hba1c_baseline_sd", "visit_correlation",
    "weight_change_placebo", "weight_effect_target_dose", "weight_change_sd"
  )
  stopifnot(all(extra %in% p$id))
  c(planning_values_301(assumptions), as.list(stats::setNames(p$planning[match(extra, p$id)], extra)))
}

#' Weeks of the scheduled post-baseline visits of an assessment, from the protocol.
#'
#' @param protocol The result of [read_protocol()].
#' @param assessment Assessment code in the protocol, for example "hba1c" or "weight".
#' @return Numeric vector of weeks, up to the primary visit.
assessment_visit_weeks <- function(protocol, assessment) {
  v <- protocol$visits
  has <- vapply(v$assessments, \(a) assessment %in% a, logical(1))
  v$week[has & v$week > 0 & v$week <= protocol$primary_visit_week]
}

#' Weeks of the scheduled post-baseline HbA1c visits.
#'
#' @param protocol The result of [read_protocol()].
#' @return Numeric vector of weeks, up to the primary visit.
hba1c_visit_weeks <- function(protocol) assessment_visit_weeks(protocol, "hba1c")

#' Covariates of the dry run when the SAP file gives none (illustrative values).
dry_run_default_covariates <- list(
  age_mean = 57, age_sd = 10, female_share = 0.45, bmi_mean = 31, bmi_sd = 5,
  weight_mean = 87, weight_sd = 17
)

#' Simulate one dry-run trial of FJF-2026-301.
#'
#' Each patient's HbA1c change follows the placebo profile, plus the drug effect while on
#' FJF-2026, plus the rescue benefit after rescue. Both build up as 1 - exp(-week / 6),
#' scaled to their full value at week 26. After discontinuation the drug effect fades
#' linearly over `washout_weeks`; the rescue benefit builds up over the same time. Residuals
#' are exchangeable between visits with the planning correlation. Patients who leave the
#' study have no values afterwards, and a few single values are missing at random.
#'
#' @param n_per_arm Patients per arm.
#' @param pars Planning values, as from [planning_values_dry_run_301()].
#' @param protocol The result of [read_protocol()].
#' @param seed Public seed.
#' @param missing_at_visit Probability that a single scheduled value is missing.
#' @param washout_weeks Weeks over which the effect fades after discontinuation.
#' @param covariates Means and spreads of age, sex, BMI and baseline weight. They feed the
#'   subgroups and the weight analysis, and have no effect on any outcome.
#' @return A list of ADaM-like datasets:
#'   - `adsl`, one row per patient: arm, stratum, AGE, SEX, BMIBL, WEIGHTBL;
#'   - `adeff`, one row per patient, parameter (`PARAMCD` "HBA1C" or "WEIGHT") and scheduled
#'     visit, with `CHG` missing when not measured. `CHG_FULL` is the value had nothing been
#'     missing, which only a dry run can know: its week-26 difference between arms is the
#'     treatment-policy value that A1 estimates;
#'   - `adtte`, time to first rescue in days (`AVAL`), censored (`CNSR` = 1) at withdrawal or
#'     week 26;
#'   - `subjects`, the patient-level intercurrent events in weeks.
#'   Rescue changes HbA1c, not weight. Weight is measured at the protocol's weight visits.
simulate_dry_run_301 <- function(n_per_arm, pars, protocol, seed, missing_at_visit = 0.03, washout_weeks = 12,
                                 covariates = dry_run_default_covariates) {
  weeks <- hba1c_visit_weeks(protocol)
  wweeks <- assessment_visit_weeks(protocol, "weight")
  primary <- protocol$primary_visit_week
  range <- unlist(protocol$eligibility$hba1c_screening_pct)
  stopifnot(length(range) == 2)
  lo <- range[1]
  hi <- range[2]
  withr::with_seed(seed, {
    n <- 2 * n_per_arm
    armcd <- rep(c("PBO", "FJF20"), each = n_per_arm)
    # Baseline within the eligibility range (rejection sampling).
    base <- stats::rnorm(n * 3, pars$hba1c_baseline_mean, pars$hba1c_baseline_sd)
    base <- round(base[base >= lo & base <= hi][seq_len(n)], 1)
    active <- armcd == "FJF20"
    disc_week <- ifelse(
      stats::runif(n) < ifelse(active, pars$discontinuation_active, pars$discontinuation_placebo),
      stats::runif(n, 0, primary), Inf
    )
    rescue_week <- ifelse(
      stats::runif(n) < ifelse(active, pars$rescue_active, pars$rescue_placebo),
      stats::runif(n, 2, primary), Inf
    )
    withdrawal_week <- ifelse(stats::runif(n) < pars$study_withdrawal, stats::runif(n, 0, primary), Inf)
    # Exchangeable residuals: a shared patient term plus a visit term.
    rho <- pars$visit_correlation
    shared <- stats::rnorm(n, 0, pars$change_sd * sqrt(rho))
    own <- matrix(stats::rnorm(n * length(weeks), 0, pars$change_sd * sqrt(1 - rho)), n)
    random_missing <- matrix(stats::runif(n * length(weeks)) < missing_at_visit, n)
    # Drawn after everything above, so adding them did not change the HbA1c data.
    w_shared <- stats::rnorm(n, 0, pars$weight_change_sd * sqrt(rho))
    w_own <- matrix(stats::rnorm(n * length(wweeks), 0, pars$weight_change_sd * sqrt(1 - rho)), n)
    w_missing <- matrix(stats::runif(n * length(wweeks)) < missing_at_visit, n)
    cv <- covariates
    age_range <- unlist(protocol$eligibility$age_years)
    age <- pmin(pmax(round(stats::rnorm(n, cv$age_mean, cv$age_sd)), age_range[1]), age_range[2])
    sex <- ifelse(stats::runif(n) < cv$female_share, "F", "M")
    bmi <- pmin(pmax(round(stats::rnorm(n, cv$bmi_mean, cv$bmi_sd), 1), 20), protocol$eligibility$bmi_max %||% 45)
    weight_bl <- round(stats::rnorm(n, cv$weight_mean, cv$weight_sd), 1)
  })
  build_up <- function(w) (1 - exp(-w / 6)) / (1 - exp(-primary / 6))
  fade <- function(w, t) ifelse(w <= t, 1, pmax(0, 1 - (w - t) / washout_weeks))
  ids <- sprintf("FJF-2026-301-DRY-%04d", seq_len(n))
  stratum <- ifelse(base > 8.5, ">8.5", "<=8.5")
  rows <- purrr::map(seq_along(weeks), function(j) {
    w <- weeks[j]
    drug <- ifelse(active, pars$effect_target_dose * build_up(pmin(w, disc_week)) * fade(w, disc_week), 0)
    rescue <- ifelse(w > rescue_week, pars$rescue_benefit * pmin(1, (w - rescue_week) / washout_weeks), 0)
    full <- pars$placebo_change_week24 * build_up(w) + drug + rescue + shared + own[, j]
    chg <- full
    chg[w > withdrawal_week | random_missing[, j]] <- NA
    tibble::tibble(
      USUBJID = ids,
      ARMCD = armcd,
      STRATUM = stratum,
      PARAMCD = "HBA1C",
      BASE = base,
      AVISITN = w,
      AVISIT = sprintf("Week %d", w),
      CHG = round(chg, 2),
      AVAL = round(base + chg, 2),
      ONTRTFL = ifelse(w <= disc_week, "Y", "N"),
      PRERESFL = ifelse(w <= rescue_week, "Y", "N"),
      CHG_FULL = round(full, 2)
    )
  })
  weight_rows <- purrr::map(seq_along(wweeks), function(j) {
    w <- wweeks[j]
    drug <- ifelse(active, pars$weight_effect_target_dose * build_up(pmin(w, disc_week)) * fade(w, disc_week), 0)
    full <- pars$weight_change_placebo * build_up(w) + drug + w_shared + w_own[, j]
    chg <- full
    chg[w > withdrawal_week | w_missing[, j]] <- NA
    tibble::tibble(
      USUBJID = ids, ARMCD = armcd, STRATUM = stratum, PARAMCD = "WEIGHT", BASE = weight_bl,
      AVISITN = w, AVISIT = sprintf("Week %d", w), CHG = round(chg, 1), AVAL = round(weight_bl + chg, 1),
      ONTRTFL = ifelse(w <= disc_week, "Y", "N"), PRERESFL = ifelse(w <= rescue_week, "Y", "N"),
      CHG_FULL = round(full, 1)
    )
  })
  subjects <- tibble::tibble(
    USUBJID = ids, ARMCD = armcd,
    disc_week = disc_week, rescue_week = rescue_week, withdrawal_week = withdrawal_week
  )
  adsl <- tibble::tibble(
    USUBJID = ids, ARMCD = armcd, STRATUM = stratum, AGE = age, SEX = sex, BMIBL = bmi, WEIGHTBL = weight_bl
  )
  end <- pmin(withdrawal_week, primary)
  adtte <- tibble::tibble(
    USUBJID = ids, ARMCD = armcd, STRATUM = stratum, PARAMCD = "RESCUE",
    AVAL = round(pmin(rescue_week, end) * 7),
    CNSR = ifelse(rescue_week <= end, 0L, 1L)
  )
  adeff <- dplyr::bind_rows(rows, weight_rows) |> dplyr::arrange(.data$USUBJID, .data$PARAMCD, .data$AVISITN)
  list(adsl = adsl, adeff = adeff, adtte = adtte, subjects = subjects)
}
