# Efficacy analyses of the primary endpoint of FJF-2026-301 (SAP part 1). Every setting is
# read from the SAP file, so the code runs what the document says. The input is an
# ADaM-like efficacy dataset with one row per patient and scheduled HbA1c visit: USUBJID,
# ARMCD (PBO, FJF20), STRATUM, BASE, AVISITN, AVISIT, CHG (NA when missing), ONTRTFL and
# PRERESFL ("Y" or "N").

#' Put the efficacy dataset in the form the models need.
#'
#' @param adeff Efficacy dataset.
#' @param reference Arm code of the reference arm, which becomes the first factor level.
#' @param paramcd Parameter to keep, when the dataset has a PARAMCD column.
#' @return A data frame with factors for subject, arm, visit (in week order) and stratum.
prepare_efficacy_301 <- function(adeff, reference = "PBO", paramcd = "HBA1C") {
  if ("PARAMCD" %in% names(adeff)) adeff <- adeff[adeff$PARAMCD == paramcd, ]
  arms <- unique(adeff$ARMCD)
  stopifnot(reference %in% arms, length(arms) == 2)
  visits <- unique(adeff$AVISIT[order(adeff$AVISITN)])
  adeff |>
    dplyr::mutate(
      USUBJID = factor(.data$USUBJID),
      ARMCD = factor(.data$ARMCD, levels = c(reference, setdiff(arms, reference))),
      AVISIT = factor(.data$AVISIT, levels = visits),
      STRATUM = factor(.data$STRATUM)
    ) |>
    dplyr::arrange(.data$USUBJID, .data$AVISIT) |>
    as.data.frame()
}

#' Data of the hypothetical estimand: values after discontinuation or rescue set to missing.
#'
#' @param data The result of [prepare_efficacy_301()].
#' @return The same data, with `CHG` missing where E2's question excludes the value.
hypothetical_data_301 <- function(data) {
  data$CHG[data$ONTRTFL != "Y" | data$PRERESFL != "Y"] <- NA
  data
}

#' Patients who leave the study: the first visit of their trailing block of missing values.
#'
#' Values missing between observed ones are not intercurrent events; rbmi imputes them under
#' missing at random. A trailing block means the patient left the study.
#'
#' @param data The result of [prepare_efficacy_301()].
#' @param strategy rbmi strategy after withdrawal ("JR", "CR", "MAR", ...).
#' @return A data frame with USUBJID, AVISIT and strategy, one row per patient who left.
withdrawal_ice_301 <- function(data, strategy) {
  visits <- levels(data$AVISIT)
  data |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::summarise(
      first_missing = {
        observed <- which(!is.na(.data$CHG[order(.data$AVISIT)]))
        last <- if (length(observed) == 0) 0L else max(observed)
        if (last == length(visits)) NA_integer_ else last + 1L
      },
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$first_missing)) |>
    dplyr::transmute(
      USUBJID = .data$USUBJID,
      AVISIT = factor(visits[.data$first_missing], levels = visits),
      strategy = strategy
    ) |>
    as.data.frame()
}

#' Multiple imputation draws for analysis A1 and its sensitivity analyses.
#'
#' The imputation model does not depend on the strategy after withdrawal (no value after
#' withdrawal is observed), so one set of draws serves A1, S1, S2 and S3.
#'
#' @param data The result of [prepare_efficacy_301()].
#' @param imputations Number of imputations.
#' @param seed Seed of the imputation.
#' @return An rbmi draws object.
draws_301 <- function(data, imputations, seed) {
  vars <- rbmi::set_vars(
    subjid = "USUBJID", visit = "AVISIT", group = "ARMCD", outcome = "CHG",
    covariates = c("BASE*AVISIT", "STRATUM"), strategy = "strategy"
  )
  withr::with_seed(seed, rbmi::draws(
    data, withdrawal_ice_301(data, "MAR"), vars,
    rbmi::method_approxbayes(covariance = "us", n_samples = imputations),
    quiet = TRUE
  ))
}

#' Impute, analyse at the primary visit and pool by Rubin's rules.
#'
#' @param draws The result of [draws_301()].
#' @param data The data the draws were made from.
#' @param strategy Strategy after withdrawal.
#' @param reference Reference arm code (both arms follow it under JR and CR).
#' @param visit Label of the primary visit.
#' @param seed Seed of the imputation step. rbmi::impute() draws random numbers too, so it
#'   needs its own seed: without one, the result would depend on whatever state the random
#'   number generator was in.
#' @param deltas Optional shifts (HbA1c points) added to imputed values of the active arm.
#' @return A tibble with one row per delta: delta, estimate, se, lower, upper, p_value.
pooled_ancova_301 <- function(draws, data, strategy, reference, visit, seed, deltas = 0) {
  arms <- levels(data$ARMCD)
  refs <- stats::setNames(rep(reference, length(arms)), arms)
  imputed <- withr::with_seed(
    seed, rbmi::impute(draws, references = refs, update_strategy = withdrawal_ice_301(data, strategy))
  )
  vars <- rbmi::set_vars(
    subjid = "USUBJID", visit = "AVISIT", group = "ARMCD", outcome = "CHG", covariates = c("BASE", "STRATUM")
  )
  purrr::map_dfr(deltas, function(d) {
    shift <- NULL
    if (d != 0) {
      shift <- rbmi::delta_template(imputed)
      shift$delta <- ifelse(shift$is_missing & shift$ARMCD != reference, d, 0)
      shift <- shift[, c("USUBJID", "AVISIT", "delta")]
    }
    fitted <- rbmi::analyse(imputed, rbmi::ancova, delta = shift, vars = vars, visits = visit)
    pooled <- as.data.frame(rbmi::pool(fitted))
    row <- pooled[pooled$parameter == paste0("trt_", visit), ]
    tibble::tibble(
      delta = d, estimate = row$est, se = row$se, lower = row$lci, upper = row$uci, p_value = row$pval
    )
  })
}

#' Mixed model for repeated measures, difference between arms at the primary visit.
#'
#' The covariance structures are tried in the SAP's order; the first that converges is used.
#'
#' @param data The result of [prepare_efficacy_301()], already restricted for the estimand.
#' @param visit Label of the primary visit.
#' @param covariances mmrm covariance structures, in order of preference.
#' @return A tibble: estimate, se, df, lower, upper, p_value, covariance.
fit_mmrm_301 <- function(data, visit, covariances = "us") {
  observed <- data[!is.na(data$CHG), ]
  fit <- NULL
  for (cov in covariances) {
    f <- stats::as.formula(sprintf("CHG ~ BASE * AVISIT + STRATUM + ARMCD * AVISIT + %s(AVISIT | USUBJID)", cov))
    fit <- tryCatch(mmrm::mmrm(f, data = observed, method = "Kenward-Roger"), error = function(e) NULL)
    if (!is.null(fit)) break
  }
  if (is.null(fit)) stop("no covariance structure converged: ", paste(covariances, collapse = ", "))
  means <- emmeans::emmeans(fit, ~ ARMCD | AVISIT)
  diff <- as.data.frame(summary(emmeans::contrast(means, list(difference = c(-1, 1))), infer = TRUE))
  row <- diff[diff$AVISIT == visit, ]
  tibble::tibble(
    estimate = row$estimate, se = row$SE, df = row$df,
    lower = row$lower.CL, upper = row$upper.CL, p_value = row$p.value, covariance = cov
  )
}

#' Smallest delta of a tipping-point table at which the upper 95% limit reaches 0.
#'
#' @param tipping A tibble with columns delta and upper, as from [pooled_ancova_301()].
#' @return The tipping point, or `NA` if the conclusion holds over the whole grid.
tipping_point <- function(tipping) {
  tipped <- tipping$delta[tipping$upper >= 0]
  if (length(tipped) == 0) NA_real_ else min(tipped)
}

#' Run every efficacy analysis of SAP part 1.
#'
#' @param adeff Efficacy dataset.
#' @param sap The result of [read_sap()].
#' @param protocol The result of [read_protocol()].
#' @param imputations Number of imputations; by default the SAP's.
#' @param deltas Tipping-point grid; by default the SAP's. A coarser grid saves time on
#'   large dry runs, where each delta re-analyses every imputed dataset.
#' @param draws Draws of the HbA1c imputation model, if already made by [draws_301()] with
#'   the SAP's seed; otherwise they are made here.
#' @return A list: `results`, one row per analysis (A1, A2, S1, S2), and `tipping`, the
#'   tipping-point table of S3, with its tipping point in `tipping_point`.
analyse_efficacy_301 <- function(adeff, sap, protocol, imputations = NULL, deltas = NULL, draws = NULL) {
  a1 <- sap_item(sap, "A1")
  visit <- sprintf("Week %d", protocol$primary_visit_week)
  reference <- a1$settings$reference
  data <- prepare_efficacy_301(adeff, reference)
  draws <- draws %||% draws_301(data, imputations %||% a1$settings$imputations, a1$settings$seed)
  mi <- function(id) {
    item <- sap_item(sap, id)
    strategy <- item$settings$strategy_after_withdrawal
    pooled_ancova_301(draws, data, strategy, reference, visit, a1$settings$seed) |>
      dplyr::mutate(analysis = id, strategy = strategy, .before = 1) |>
      dplyr::select(-"delta")
  }
  results <- dplyr::bind_rows(
    mi("A1"),
    fit_mmrm_301(hypothetical_data_301(data), visit, unlist(sap_item(sap, "A2")$settings$covariance_fallback)) |>
      dplyr::mutate(analysis = "A2", strategy = "MMRM", .before = 1),
    mi("S1"),
    mi("S2")
  )
  s3 <- sap_item(sap, "S3")
  grid <- s3$settings$deltas
  tipping <- pooled_ancova_301(
    draws, data, s3$settings$strategy_after_withdrawal, reference, visit, a1$settings$seed,
    deltas = deltas %||% seq(grid$from, grid$to, by = grid$by)
  )
  list(results = results, tipping = tipping, tipping_point = tipping_point(tipping))
}

#' The simplified primary analysis of the sample size (ADR 0020), on a longitudinal dataset.
#'
#' Week-26 values missing after withdrawal are imputed once from the observed placebo
#' week-26 values, and the arms are compared by a two-sample t-test.
#'
#' @param data The result of [prepare_efficacy_301()].
#' @param visit Label of the primary visit.
#' @param reference Reference arm code.
#' @return A tibble: estimate, p_value.
simplified_analysis_301 <- function(data, visit, reference) {
  w <- data[data$AVISIT == visit, ]
  ref <- w$CHG[w$ARMCD == reference & !is.na(w$CHG)]
  miss <- is.na(w$CHG)
  w$CHG[miss] <- mean(ref) + stats::sd(ref) * stats::rnorm(sum(miss))
  test <- stats::t.test(w$CHG[w$ARMCD != reference], w$CHG[w$ARMCD == reference])
  tibble::tibble(estimate = unname(test$estimate[1] - test$estimate[2]), p_value = test$p.value)
}

#' Operating characteristics of the primary analysis on dry-run trials.
#'
#' Each trial is generated from the planning values and analysed twice: by the real primary
#' analysis (A1, with fewer imputations) and by the simplified analysis that chose the
#' sample size. Trials are run in parallel; each has its own seed, so the result does not
#' depend on the number of cores.
#'
#' @param n_per_arm Patients per arm.
#' @param pars Planning values, as from [planning_values_dry_run_301()].
#' @param protocol The result of [read_protocol()].
#' @param sap The result of [read_sap()].
#' @param trials,imputations Number of trials and of imputations per trial.
#' @param seed Public seed; trial i uses seed + i.
#' @param cores Parallel workers.
#' @return A tibble with one row per trial and analysis: trial, analysis, estimate, se, p_value.
operating_characteristics_301 <- function(n_per_arm, pars, protocol, sap, trials, imputations, seed, cores = 1) {
  a1 <- sap_item(sap, "A1")
  dry <- sap$dry_run
  visit <- sprintf("Week %d", protocol$primary_visit_week)
  one <- function(i) {
    adeff <- simulate_dry_run_301(
      n_per_arm, pars, protocol, seed + i, dry$missing_at_visit, dry$washout_weeks
    )$adeff
    data <- prepare_efficacy_301(adeff, a1$settings$reference)
    draws <- draws_301(data, imputations, seed + i)
    real <- pooled_ancova_301(
      draws, data, a1$settings$strategy_after_withdrawal, a1$settings$reference, visit, seed + i
    )
    simple <- withr::with_seed(seed + i, simplified_analysis_301(data, visit, a1$settings$reference))
    dplyr::bind_rows(
      tibble::tibble(
        trial = i, analysis = "A1 (multiple imputation, J2R)", estimate = real$estimate,
        se = real$se, p_value = real$p_value
      ),
      tibble::tibble(
        trial = i, analysis = "Sample-size model (t-test)", estimate = simple$estimate,
        se = NA_real_, p_value = simple$p_value
      )
    )
  }
  runs <- if (cores > 1) parallel::mclapply(seq_len(trials), one, mc.cores = cores) else lapply(seq_len(trials), one)
  dplyr::bind_rows(runs)
}

#' Summary of the operating characteristics: power with its Monte Carlo error.
#'
#' Besides the share of trials that succeed, it gives the power implied by the mean and the
#' spread of the estimates (normal approximation). That is much less noisy than counting
#' successes in a few hundred trials, and is what the comparison with the sample size uses.
#'
#' @param oc The result of [operating_characteristics_301()].
#' @param alpha Two-sided level.
#' @return A tibble with one row per analysis.
summarise_oc_301 <- function(oc, alpha = 0.05) {
  oc |>
    dplyr::group_by(.data$analysis) |>
    dplyr::summarise(
      trials = dplyr::n(),
      power = mean(.data$p_value < alpha & .data$estimate < 0),
      mcse = sqrt(.data$power * (1 - .data$power) / .data$trials),
      mean_estimate = mean(.data$estimate),
      empirical_sd = stats::sd(.data$estimate),
      mean_se = mean(.data$se),
      implied_power = stats::pnorm(abs(.data$mean_estimate) / .data$empirical_sd - stats::qnorm(1 - alpha / 2)),
      .groups = "drop"
    )
}
