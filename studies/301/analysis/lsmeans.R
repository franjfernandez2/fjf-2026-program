# Least-squares means of each arm, for the efficacy tables (T-14.2.1, T-14.2.2, T-14.2.5).
#
# The SAP's analysis functions (R/efficacy_301.R, R/secondary_301.R) return the difference
# between the arms; the tables also print each arm's LS mean. These functions redo the same
# imputation and analysis, with the same draws and seeds, keep the arm means, and stop if
# their difference is not the analysis's, so a table cannot print means from another fit.
# They are separate so that the F2 functions, and the SAP's dry run built on them, stay as
# they were.

#' Arm LS means of a multiple-imputation ANCOVA at the primary visit.
#'
#' @param draws Imputation draws ([draws_301()]).
#' @param data The data the draws were made from ([prepare_efficacy_301()]).
#' @param strategy Strategy after withdrawal.
#' @param reference Reference arm code.
#' @param visit Primary visit label.
#' @param seed Seed of the imputation step (the analysis's).
#' @param estimate The analysis's estimate of the difference, to check against.
#' @return A tibble: ARMCD, lsmean, se.
lsmeans_mi_301 <- function(draws, data, strategy, reference, visit, seed, estimate) {
  arms <- levels(data$ARMCD)
  refs <- stats::setNames(rep(reference, length(arms)), arms)
  imputed <- withr::with_seed(
    seed, rbmi::impute(draws, references = refs, update_strategy = withdrawal_ice_301(data, strategy))
  )
  vars <- rbmi::set_vars(
    subjid = "USUBJID", visit = "AVISIT", group = "ARMCD", outcome = "CHG", covariates = c("BASE", "STRATUM")
  )
  pooled <- as.data.frame(rbmi::pool(rbmi::analyse(imputed, rbmi::ancova, vars = vars, visits = visit)))
  get <- function(prefix) pooled[pooled$parameter == paste0(prefix, visit), ]
  if (!isTRUE(all.equal(get("trt_")$est, estimate, tolerance = 1e-10))) {
    stop("the LS means do not come from the analysis's fit")
  }
  tibble::tibble(
    ARMCD = arms, lsmean = c(get("lsm_ref_")$est, get("lsm_alt_")$est), se = c(get("lsm_ref_")$se, get("lsm_alt_")$se)
  )
}

#' Arm LS means of the MMRM at the primary visit (analysis A2).
#'
#' @param data Data of the hypothetical estimand ([hypothetical_data_301()]).
#' @param visit Primary visit label.
#' @param covariance The covariance structure the analysis used.
#' @param estimate The analysis's estimate of the difference.
#' @return A tibble: ARMCD, lsmean, se.
lsmeans_mmrm_301 <- function(data, visit, covariance, estimate) {
  observed <- data[!is.na(data$CHG), ]
  f <- stats::as.formula(sprintf("CHG ~ BASE * AVISIT + STRATUM + ARMCD * AVISIT + %s(AVISIT | USUBJID)", covariance))
  fit <- mmrm::mmrm(f, data = observed, method = "Kenward-Roger")
  means <- as.data.frame(summary(emmeans::emmeans(fit, ~ ARMCD | AVISIT)))
  means <- means[means$AVISIT == visit, ]
  if (!isTRUE(all.equal(means$emmean[2] - means$emmean[1], estimate, tolerance = 1e-8))) {
    stop("the LS means do not come from the analysis's fit")
  }
  tibble::tibble(ARMCD = as.character(means$ARMCD), lsmean = means$emmean, se = means$SE)
}

#' Arm LS means of analyses A1, A2 and B2.
#'
#' @param data Output of [analysis_data_301()].
#' @param hba1c_draws The HbA1c draws of A1.
#' @param efficacy Output of [analyse_efficacy_301()].
#' @param secondary Output of [analyse_secondary_301()].
#' @param sap The SAP ([read_sap()]).
#' @param protocol The protocol ([read_protocol()]).
#' @return A tibble: analysis, ARMCD, lsmean, se.
analysis_lsmeans_301 <- function(data, hba1c_draws, efficacy, secondary, sap, protocol) {
  visit <- sprintf("Week %d", protocol$primary_visit_week)
  a1 <- sap_item(sap, "A1")
  reference <- a1$settings$reference
  result <- function(id) efficacy$results[efficacy$results$analysis == id, ]
  hba1c <- prepare_efficacy_301(data$adeff, reference)
  a1_means <- lsmeans_mi_301(
    hba1c_draws, hba1c, a1$settings$strategy_after_withdrawal, reference, visit, a1$settings$seed,
    result("A1")$estimate
  )
  a2_means <- lsmeans_mmrm_301(hypothetical_data_301(hba1c), visit, result("A2")$covariance, result("A2")$estimate)
  b2 <- sap_item(sap, "B2")
  weight <- prepare_efficacy_301(data$adeff, reference, "WEIGHT")
  weight_draws <- draws_301(weight, b2$settings$imputations, b2$settings$seed)
  b2_means <- lsmeans_mi_301(
    weight_draws, weight, b2$settings$strategy_after_withdrawal, reference, visit, b2$settings$seed,
    secondary$results$estimate[secondary$results$analysis == "B2"]
  )
  dplyr::bind_rows(
    dplyr::mutate(a1_means, analysis = "A1", .before = 1),
    dplyr::mutate(a2_means, analysis = "A2", .before = 1),
    dplyr::mutate(b2_means, analysis = "B2", .before = 1)
  )
}
