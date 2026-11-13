# Clinical utility index of FJF-2026-201 (SAP X1, exploratory).
#
# The SAP fixes the two components, their scaling to [0, 1] and their weights (0.7 and 0.3);
# this file fixes the details it leaves open (ADR 0031):
# - HbA1c effect: the fitted effect of the selected dose-response model (A1) at each dose;
#   without a selected model, the MMRM estimate of each dose minus placebo.
# - Rate of adverse events leading to discontinuation: the share of the safety set with a
#   treatment-emergent adverse event whose action was "DRUG WITHDRAWN", by the dose taken.
# - Scaling: linear over the doses of the study, 1 for the best and 0 for the worst dose of
#   each component (a larger HbA1c fall is better, a lower rate is better). A component that
#   is the same at every dose scores 1 everywhere, so it does not separate the doses.

#' Scale values linearly to [0, 1] over the doses: 1 for the best, 0 for the worst.
#'
#' @param x Values, one per dose.
#' @param higher_better Whether a larger value is better.
#' @return Scaled values.
scale_utility_201 <- function(x, higher_better) {
  span <- max(x) - min(x)
  if (span == 0) {
    return(rep(1, length(x)))
  }
  if (higher_better) (x - min(x)) / span else (max(x) - x) / span
}

#' The clinical utility index by dose.
#'
#' @param adam Named list of the ADaM datasets (ADSL, ADAE).
#' @param analysis Output of [analyse_201()].
#' @param sap The result of [read_sap()] for FJF-2026-201.
#' @return A tibble: dose, effect (HbA1c, vs placebo), effect_source, n (safety set),
#'   n_disc (patients with an adverse event leading to discontinuation), rate, u_effect,
#'   u_safety, cui.
clinical_utility_201 <- function(adam, analysis, sap) {
  doses <- analysis$means$doses
  a1 <- analysis$a1
  fitted <- !is.null(a1$effects)
  effect <- if (fitted) unname(a1$effects) else unname(analysis$means$means - analysis$means$means[doses == 0])
  saf <- adam$ADSL[adam$ADSL$SAFFL %in% "Y", ]
  ae <- adam$ADAE[adam$ADAE$SAFFL %in% "Y" & adam$ADAE$TRTEMFL %in% "Y" & adam$ADAE$AEACN %in% "DRUG WITHDRAWN", ]
  dose_of <- stats::setNames(saf$TRT01AN, saf$USUBJID)
  disc <- unique(ae$USUBJID)
  n <- vapply(doses, function(d) sum(saf$TRT01AN == d), 0L)
  n_disc <- vapply(doses, function(d) sum(dose_of[disc] == d, na.rm = TRUE), 0L)
  rate <- ifelse(n > 0, n_disc / n, NA_real_)
  if (anyNA(rate)) stop("a dose of the study has no patient in the safety set")
  # HbA1c falls with a better dose (direction "decreasing"): the benefit is minus the effect.
  benefit <- if (sap$primary$mcp$direction == "decreasing") -effect else effect
  w <- clinical_utility_weights_201(sap)
  u_effect <- scale_utility_201(benefit, higher_better = TRUE)
  u_safety <- scale_utility_201(rate, higher_better = FALSE)
  tibble::tibble(
    dose = doses, effect = effect, effect_source = if (fitted) a1$model else "MMRM",
    n = n, n_disc = n_disc, rate = rate, u_effect = u_effect, u_safety = u_safety,
    cui = w[["effect"]] * u_effect + w[["safety"]] * u_safety
  )
}

#' The weights of the two components, read from the SAP text of X1 ("weighted 0.7 and 0.3").
#'
#' @param sap The result of [read_sap()] for FJF-2026-201.
#' @return A named numeric vector: effect, safety.
clinical_utility_weights_201 <- function(sap) {
  x1 <- purrr::keep(sap$exploratory, function(x) x$id == "X1")
  if (length(x1) != 1) stop("the SAP has no single exploratory item X1")
  w <- regmatches(x1[[1]]$method, regexec("weighted ([0-9]*[.]?[0-9]+) and ([0-9]*[.]?[0-9]+)", x1[[1]]$method))[[1]]
  if (length(w) != 3) stop("the weights of X1 are not in the SAP text")
  w <- as.numeric(w[2:3])
  if (abs(sum(w) - 1) > 1e-12) stop("the weights of X1 do not add up to 1")
  c(effect = w[1], safety = w[2])
}
