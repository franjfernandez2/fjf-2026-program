# Sample size of FJF-2026-301 by simulation (ADR 0020). The planning model generates the
# week-26 change in HbA1c under the treatment-policy estimand and analyses it the way the
# primary analysis does, in a simplified form. It uses planning values only, never the
# simulator or the sealed truth.

#' Planning values needed by the 301 sample size, from the design assumptions.
#'
#' @param assumptions The result of [read_assumptions()].
#' @return A named list of numbers.
planning_values_301 <- function(assumptions) {
  p <- assumptions$parameters
  ids <- c(
    "placebo_change_week24", "change_sd", "effect_target_dose", "discontinuation_placebo",
    "discontinuation_active", "rescue_placebo", "rescue_active", "rescue_benefit", "study_withdrawal"
  )
  stopifnot(all(ids %in% p$id))
  as.list(stats::setNames(p$planning[match(ids, p$id)], ids))
}

#' Simulate one 301 trial under the planning model and test the primary hypothesis.
#'
#' Each patient's week-26 change is the placebo change plus noise, plus the drug effect if
#' the patient is on FJF-2026 and has not stopped it, plus the rescue benefit if rescued.
#' Patients who leave the study have no week-26 value; it is imputed from the observed
#' placebo values (jump to reference) and the arms are compared by a two-sample t-test.
#'
#' @param n_per_arm Patients per arm.
#' @param pars Planning values, as from [planning_values_301()].
#' @return The two-sided p-value and the estimated difference (FJF-2026 minus placebo).
simulate_301_trial <- function(n_per_arm, pars) {
  arm <- rep(c(0, 1), each = n_per_arm)
  disc <- stats::rbinom(2 * n_per_arm, 1, ifelse(arm == 1, pars$discontinuation_active, pars$discontinuation_placebo))
  rescue <- stats::rbinom(2 * n_per_arm, 1, ifelse(arm == 1, pars$rescue_active, pars$rescue_placebo))
  y <- pars$placebo_change_week24 + stats::rnorm(2 * n_per_arm, 0, pars$change_sd) +
    pars$effect_target_dose * (arm == 1 & disc == 0) + pars$rescue_benefit * rescue
  lost <- stats::rbinom(2 * n_per_arm, 1, pars$study_withdrawal) == 1
  ref <- y[arm == 0 & !lost]
  y[lost] <- mean(ref) + stats::sd(ref) * stats::rnorm(sum(lost))
  y1 <- y[arm == 1]
  y0 <- y[arm == 0]
  diff <- mean(y1) - mean(y0)
  se <- sqrt(stats::var(y1) / n_per_arm + stats::var(y0) / n_per_arm)
  c(p_value = 2 * stats::pt(-abs(diff / se), df = 2 * n_per_arm - 2), difference = diff)
}

#' Estimated power over a grid of sample sizes, with its Monte Carlo standard error.
#'
#' @param n_grid Patients per arm to try.
#' @param pars Planning values.
#' @param reps Simulated trials per sample size.
#' @param seed Public seed, recorded with the result.
#' @param alpha Two-sided significance level.
#' @return A tibble with n_per_arm, power, mcse, mean_difference, reps and seed.
simulate_power_301 <- function(n_grid, pars, reps, seed, alpha = 0.05) {
  withr::with_seed(seed, {
    purrr::map_dfr(n_grid, function(n) {
      res <- vapply(seq_len(reps), function(i) simulate_301_trial(n, pars), numeric(2))
      success <- res["p_value", ] < alpha & res["difference", ] < 0
      power <- mean(success)
      tibble::tibble(
        n_per_arm = n, power = power, mcse = sqrt(power * (1 - power) / reps),
        mean_difference = mean(res["difference", ]), reps = reps, seed = seed
      )
    })
  })
}

#' Smallest sample size whose power reaches the target, allowing for Monte Carlo error.
#'
#' The lower 95% limit of the simulated power (power minus 1.96 Monte Carlo standard
#' errors) must reach the target, so a lucky simulation cannot choose too small a study.
#'
#' @param power_table The result of [simulate_power_301()].
#' @param target Target power.
#' @return Patients per arm, or `NA` if none in the grid reaches it.
choose_sample_size <- function(power_table, target = 0.9) {
  lower <- power_table$power - stats::qnorm(0.975) * power_table$mcse
  ok <- power_table$n_per_arm[lower >= target]
  if (length(ok) == 0) NA_real_ else min(ok)
}

#' Power at a fixed sample size when single planning values move to the edge of their range.
#'
#' @param n_per_arm Chosen patients per arm.
#' @param assumptions The result of [read_assumptions()].
#' @param reps Simulated trials per scenario.
#' @param seed Public seed.
#' @return A tibble with one row per scenario.
power_robustness_301 <- function(n_per_arm, assumptions, reps, seed) {
  base <- planning_values_301(assumptions)
  p <- assumptions$parameters
  edge <- function(id, side) if (side == "low") p$truth_low[p$id == id] else p$truth_high[p$id == id]
  scenarios <- list(
    "Planning values" = list(),
    "Smaller effect (-0.30)" = list(effect_target_dose = edge("effect_target_dose", "high")),
    "Larger SD (1.1)" = list(change_sd = edge("change_sd", "high")),
    "More discontinuation on FJF-2026 (13%)" = list(discontinuation_active = edge("discontinuation_active", "high")),
    "More rescue on placebo (20%)" = list(rescue_placebo = edge("rescue_placebo", "high")),
    "More study withdrawal (10%)" = list(study_withdrawal = edge("study_withdrawal", "high"))
  )
  purrr::imap_dfr(scenarios, function(change, label) {
    pars <- utils::modifyList(base, change)
    simulate_power_301(n_per_arm, pars, reps, seed) |> dplyr::mutate(scenario = label, .before = 1)
  })
}
