# Common pieces of the tables and figures of FJF-2026-201 (docs/sap/fjf-2026-201-outputs.yml).
# The layout helpers are those of FJF-2026-301 (studies/301/tlf/tlf_common.R).

#' Printed label of a dose.
dose_label_201 <- function(dose) ifelse(dose == 0, "Placebo", sprintf("%s mg", format(dose, trim = TRUE)))

#' Printed name of a candidate model of MCP-Mod.
model_label_201 <- function(model) {
  unname(c(linear = "Linear", emax = "Emax", sigEmax = "Sigmoid Emax", quadratic = "Quadratic")[model])
}

#' The MMRM estimates at the primary visit: each dose's mean change and its difference from
#' placebo, with standard errors and 95% confidence intervals (normal approximation, as in
#' MCP-Mod, which uses the same means and covariance).
#'
#' @param means The `means` element of [analyse_201()]: doses, means, S.
#' @return A tibble: dose, mean, se, diff, diff_se, lower, upper (difference NA for placebo).
mmrm_estimates_201 <- function(means) {
  s <- means$S
  p <- which(means$doses == 0)
  diff_se <- sqrt(diag(s) + s[p, p] - 2 * s[, p])
  z <- stats::qnorm(0.975)
  diff <- unname(means$means - means$means[p])
  out <- tibble::tibble(
    dose = means$doses, mean = unname(means$means), se = sqrt(diag(s)),
    diff = diff, diff_se = diff_se, lower = diff - z * diff_se, upper = diff + z * diff_se
  )
  out[p, c("diff", "diff_se", "lower", "upper")] <- NA_real_
  out
}

#' Dose of each arm label of an analysis set.
#' @param adsl ADSL rows of the analysis set.
#' @param arms Output of [arms_301()].
#' @param var "TRT01P" or "TRT01A".
arm_doses_201 <- function(adsl, arms, var) {
  unname(stats::setNames(adsl[[paste0(var, "N")]], adsl[[var]])[arms$label])
}
