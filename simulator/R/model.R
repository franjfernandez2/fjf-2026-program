# The data-generating model: the true dose-response, how effects build up over time, and
# the rates of intercurrent events over a study's length.

#' The true dose-response curve.
#'
#' An Emax curve (no Hill factor). ED50 follows from the share of the reference dose's
#' effect that the lowest dose gives; Emax from the effect of the reference dose. Effects
#' are full effects, reached at week 26. They build up over time as [build_up()].
#'
#' @param truth An `fjf_truth`.
#' @param lowest_dose,reference_dose Doses in mg (5 and 20 in this programme).
#' @return A list: `ed50`, `emax`, `tau` (weeks of the build-up), `placebo` (week-26 change).
true_curve <- function(truth, lowest_dose = 5, reference_dose = 20) {
  r <- truth$lowest_dose_effect_ratio
  stopifnot(r * reference_dose > lowest_dose, r < 1)
  ed50 <- lowest_dose * reference_dose * (1 - r) / (r * reference_dose - lowest_dose)
  list(
    ed50 = ed50,
    emax = truth$effect_target_dose * (ed50 + reference_dose) / reference_dose,
    tau = build_up_tau(truth$effect_ratio_week12),
    placebo = truth$placebo_change_week24
  )
}

#' Effect of a dose under the curve, at full build-up.
#'
#' @param curve The result of [true_curve()].
#' @param dose Dose in mg.
#' @return Difference from placebo in HbA1c change.
dose_effect <- function(curve, dose) curve$emax * dose / (curve$ed50 + dose)

#' Share of the week-26 effect reached by week `week`: (1 - exp(-w / tau)) / (1 - exp(-26 / tau)).
#'
#' @param week Weeks on treatment.
#' @param tau Time constant in weeks.
#' @return A share, 0 at week 0 and 1 at week 26.
build_up <- function(week, tau) (1 - exp(-pmax(week, 0) / tau)) / (1 - exp(-26 / tau))

#' The time constant that makes the effect at week 12 a given share of the week-26 effect.
#'
#' @param ratio Share at week 12, above 12/26.
#' @return tau in weeks.
build_up_tau <- function(ratio) {
  stopifnot(ratio > 12 / 26, ratio < 1)
  stats::uniroot(\(tau) build_up(12, tau) - ratio, c(0.05, 500), tol = 1e-10)$root
}

#' Probability of an event over `weeks`, from its probability over 26 weeks, with a
#' constant hazard.
#'
#' @param p Probability over 26 weeks.
#' @param weeks Length of the study.
#' @return Probability over `weeks`.
rate_over <- function(p, weeks) 1 - (1 - p)^(weeks / 26)
