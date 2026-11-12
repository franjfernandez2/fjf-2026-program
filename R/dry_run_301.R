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
  extra <- c("hba1c_baseline_mean", "hba1c_baseline_sd", "visit_correlation")
  stopifnot(all(extra %in% p$id))
  c(planning_values_301(assumptions), as.list(stats::setNames(p$planning[match(extra, p$id)], extra)))
}

#' Weeks of the scheduled post-baseline HbA1c visits, from the protocol.
#'
#' @param protocol The result of [read_protocol()].
#' @return Numeric vector of weeks, up to the primary visit.
hba1c_visit_weeks <- function(protocol) {
  v <- protocol$visits
  has <- vapply(v$assessments, \(a) "hba1c" %in% a, logical(1))
  v$week[has & v$week > 0 & v$week <= protocol$primary_visit_week]
}

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
#' @return A list: `adeff`, one row per patient and HbA1c visit (ADaM-like, `CHG` is `NA`
#'   when missing), and `subjects`, the patient-level intercurrent events in weeks. `adeff`
#'   also has `CHG_FULL`, the value had nothing been missing, which only a dry run can know:
#'   its week-26 difference between arms is the treatment-policy value that A1 estimates.
simulate_dry_run_301 <- function(n_per_arm, pars, protocol, seed, missing_at_visit = 0.03, washout_weeks = 12) {
  weeks <- hba1c_visit_weeks(protocol)
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
  })
  build_up <- function(w) (1 - exp(-w / 6)) / (1 - exp(-primary / 6))
  fade <- function(w, t) ifelse(w <= t, 1, pmax(0, 1 - (w - t) / washout_weeks))
  rows <- purrr::map(seq_along(weeks), function(j) {
    w <- weeks[j]
    drug <- ifelse(active, pars$effect_target_dose * build_up(pmin(w, disc_week)) * fade(w, disc_week), 0)
    rescue <- ifelse(w > rescue_week, pars$rescue_benefit * pmin(1, (w - rescue_week) / washout_weeks), 0)
    full <- pars$placebo_change_week24 * build_up(w) + drug + rescue + shared + own[, j]
    chg <- full
    chg[w > withdrawal_week | random_missing[, j]] <- NA
    tibble::tibble(
      USUBJID = sprintf("FJF-2026-301-DRY-%04d", seq_len(n)),
      ARMCD = armcd,
      STRATUM = ifelse(base > 8.5, ">8.5", "<=8.5"),
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
  subjects <- tibble::tibble(
    USUBJID = sprintf("FJF-2026-301-DRY-%04d", seq_len(n)), ARMCD = armcd,
    disc_week = disc_week, rescue_week = rescue_week, withdrawal_week = withdrawal_week
  )
  adeff <- dplyr::bind_rows(rows) |> dplyr::arrange(.data$USUBJID, .data$AVISITN)
  list(adeff = adeff, subjects = subjects)
}
