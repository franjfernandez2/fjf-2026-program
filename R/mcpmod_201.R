# Study FJF-2026-201 (phase II, dose finding): planning dose-response, dry-run data, the
# generalised MCP-Mod analysis of its SAP, and its sample size. Settings are read from the
# SAP file. The dry run uses the PLANNING values only, never the simulator or the sealed
# truth (ADR 0018, ADR 0021).

#' Planning Emax curve of the change in HbA1c at the end of a study, by dose.
#'
#' The phase III dose effect (`effect_target_dose`, at 24-26 weeks) and the ratio of the
#' lowest phase II dose effect to it (`lowest_dose_effect_ratio`) fix the ED50 and the Emax.
#' A 12-week study scales both, and the placebo change, by `effect_ratio_week12`.
#'
#' @param assumptions The result of [read_assumptions()].
#' @param lowest_dose,reference_dose The lowest phase II dose and the phase III dose (mg).
#' @param week12 Scale to week 12 (`TRUE`) or keep the 24-26 week values.
#' @return A list: e0 (placebo change), emax, ed50.
planning_curve_201 <- function(assumptions, lowest_dose, reference_dose, week12 = TRUE) {
  p <- assumptions$parameters
  pv <- function(id) p$planning[p$id == id]
  r <- pv("lowest_dose_effect_ratio")
  stopifnot(r * reference_dose > lowest_dose, r < 1)
  ed50 <- lowest_dose * reference_dose * (1 - r) / (r * reference_dose - lowest_dose)
  scale <- if (week12) pv("effect_ratio_week12") else 1
  list(
    e0 = pv("placebo_change_week24") * scale,
    emax = pv("effect_target_dose") * (ed50 + reference_dose) / reference_dose * scale,
    ed50 = ed50
  )
}

#' Effect of a dose under an Emax curve (difference from placebo).
#'
#' @param curve The result of [planning_curve_201()].
#' @param dose Dose in mg.
#' @return The effect, in HbA1c percentage points.
emax_effect <- function(curve, dose) curve$emax * dose / (curve$ed50 + dose)

#' Probability of an event within `weeks`, from its probability within `from_weeks`.
#'
#' Assumes a constant hazard over time.
#'
#' @param p Probability over `from_weeks`.
#' @param weeks,from_weeks Durations in weeks.
#' @return The probability over `weeks`.
rescale_probability <- function(p, weeks, from_weeks = 26) 1 - (1 - p)^(weeks / from_weeks)

#' Simulate one dry-run trial of FJF-2026-201.
#'
#' As the FJF-2026-301 dry run (`simulate_dry_run_301()`), for five arms: each dose's effect
#' follows the planning Emax curve and builds up as 1 - exp(-week / 6), scaled to its full
#' value at week 12. Intercurrent events use the 26-week planning rates rescaled to 12 weeks;
#' the rescue rate moves from placebo's towards the active rate in proportion to the dose's
#' effect.
#'
#' @param n_per_arm Patients per arm.
#' @param assumptions The result of [read_assumptions()].
#' @param protocol The result of [read_protocol()] for FJF-2026-201.
#' @param reference_dose The phase III dose (mg).
#' @param seed Public seed.
#' @param missing_at_visit Probability that a single scheduled value is missing.
#' @param washout_weeks Weeks over which the effect fades after discontinuation.
#' @return A list: `adeff` (ADaM-like, with ARMCD and DOSE) and `curve`.
simulate_dry_run_201 <- function(n_per_arm, assumptions, protocol, reference_dose, seed,
                                 missing_at_visit = 0.03, washout_weeks = 12) {
  pars <- planning_values_dry_run_301(assumptions)
  doses <- purrr::map_dbl(protocol$arms, "dose_mg")
  codes <- purrr::map_chr(protocol$arms, "code")
  curve <- planning_curve_201(assumptions, min(doses[doses > 0]), reference_dose)
  weeks <- hba1c_visit_weeks(protocol)
  primary <- protocol$primary_visit_week
  range <- unlist(protocol$eligibility$hba1c_screening_pct)
  rate <- function(p) rescale_probability(p, primary)
  withr::with_seed(seed, {
    n <- length(doses) * n_per_arm
    arm <- rep(seq_along(doses), each = n_per_arm)
    dose <- doses[arm]
    base <- stats::rnorm(n * 3, pars$hba1c_baseline_mean, pars$hba1c_baseline_sd)
    base <- round(base[base >= range[1] & base <= range[2]][seq_len(n)], 1)
    effect_share <- emax_effect(curve, dose) / emax_effect(curve, reference_dose)
    p_disc <- ifelse(dose > 0, rate(pars$discontinuation_active), rate(pars$discontinuation_placebo))
    p_rescue <- rate(pars$rescue_placebo) + (rate(pars$rescue_active) - rate(pars$rescue_placebo)) * effect_share
    disc_week <- ifelse(stats::runif(n) < p_disc, stats::runif(n, 0, primary), Inf)
    rescue_week <- ifelse(stats::runif(n) < p_rescue, stats::runif(n, 2, primary), Inf)
    withdrawal_week <- ifelse(stats::runif(n) < rate(pars$study_withdrawal), stats::runif(n, 0, primary), Inf)
    rho <- pars$visit_correlation
    shared <- stats::rnorm(n, 0, pars$change_sd * sqrt(rho))
    own <- matrix(stats::rnorm(n * length(weeks), 0, pars$change_sd * sqrt(1 - rho)), n)
    random_missing <- matrix(stats::runif(n * length(weeks)) < missing_at_visit, n)
  })
  build_up <- function(w) (1 - exp(-w / 6)) / (1 - exp(-primary / 6))
  fade <- function(w, t) ifelse(w <= t, 1, pmax(0, 1 - (w - t) / washout_weeks))
  ids <- sprintf("FJF-2026-201-DRY-%04d", seq_len(n))
  rows <- purrr::map(seq_along(weeks), function(j) {
    w <- weeks[j]
    drug <- emax_effect(curve, dose) * build_up(pmin(w, disc_week)) * fade(w, disc_week)
    rescue <- ifelse(w > rescue_week, pars$rescue_benefit * pmin(1, (w - rescue_week) / washout_weeks), 0)
    full <- curve$e0 * build_up(w) + drug + rescue + shared + own[, j]
    chg <- full
    chg[w > withdrawal_week | random_missing[, j]] <- NA
    tibble::tibble(
      USUBJID = ids, ARMCD = codes[arm], DOSE = dose, STRATUM = ifelse(base > 8.5, ">8.5", "<=8.5"),
      PARAMCD = "HBA1C", BASE = base, AVISITN = w, AVISIT = sprintf("Week %d", w),
      CHG = round(chg, 2), AVAL = round(base + chg, 2),
      ONTRTFL = ifelse(w <= disc_week, "Y", "N"), PRERESFL = ifelse(w <= rescue_week, "Y", "N"),
      CHG_FULL = round(full, 2)
    )
  })
  adeff <- dplyr::bind_rows(rows) |> dplyr::arrange(.data$USUBJID, .data$AVISITN)
  list(adeff = adeff, curve = curve)
}

#' Least-squares means by arm at a visit from an MMRM, with their covariance matrix.
#'
#' @param data Efficacy data with USUBJID, ARMCD, DOSE, STRATUM, BASE, AVISIT and CHG.
#' @param visit Label of the visit.
#' @param covariances mmrm covariance structures, in order of preference.
#' @return A list: doses, means (by dose, in increasing order), S (their covariance) and
#'   the covariance structure used.
mmrm_means_201 <- function(data, visit, covariances = "us") {
  arms <- unique(data[order(data$DOSE), c("ARMCD", "DOSE")])
  d <- data[!is.na(data$CHG), ]
  d$ARMCD <- factor(d$ARMCD, levels = arms$ARMCD)
  d$AVISIT <- factor(d$AVISIT, levels = unique(data$AVISIT[order(data$AVISITN)]))
  d$USUBJID <- factor(d$USUBJID)
  d$STRATUM <- factor(d$STRATUM)
  fit <- NULL
  for (cov in covariances) {
    f <- stats::as.formula(sprintf("CHG ~ BASE * AVISIT + STRATUM + ARMCD * AVISIT + %s(AVISIT | USUBJID)", cov))
    fit <- tryCatch(mmrm::mmrm(f, data = d, method = "Kenward-Roger"), error = function(e) NULL)
    if (!is.null(fit)) break
  }
  if (is.null(fit)) stop("no covariance structure converged: ", paste(covariances, collapse = ", "))
  em <- emmeans::emmeans(fit, ~ ARMCD | AVISIT, at = list(AVISIT = visit))
  list(
    doses = arms$DOSE,
    means = stats::setNames(summary(em)$emmean, arms$DOSE),
    S = unname(stats::vcov(em)),
    covariance = cov
  )
}

#' Candidate dose-response models of the SAP, as DoseFinding models.
#'
#' @param sap The result of [read_sap()] for FJF-2026-201.
#' @param doses Doses of the study (mg), placebo included.
#' @param max_effect Maximum effect over the dose range used to scale the models.
#' @return A DoseFinding `Mods` object.
candidate_models_201 <- function(sap, doses, max_effect) {
  args <- list()
  for (m in sap$primary$mcp$candidate_models) {
    # list() keeps a NULL value (linear has no guesstimate); [[<- NULL would drop it.
    args[m$model] <- list(switch(m$model,
      linear = NULL,
      emax = m$ed50,
      sigEmax = c(m$ed50, m$h),
      quadratic = m$delta,
      exponential = m$delta,
      stop("unknown candidate model ", m$model)
    ))
  }
  do.call(DoseFinding::Mods, c(args, list(
    doses = doses, placEff = 0, maxEff = max_effect, direction = sap$primary$mcp$direction
  )))
}

#' Target dose: the smallest dose with at least `p` of the maximum effect and a clinically
#' relevant effect.
#'
#' @param effects Effects (difference from placebo) at the doses, placebo included.
#' @param doses Doses (mg), increasing, placebo first.
#' @param p Share of the maximum effect.
#' @param relevance Smallest clinically relevant effect, as a positive number.
#' @param direction "decreasing" when a benefit is a negative effect.
#' @return The target dose, or `NA` if no dose qualifies.
target_dose_201 <- function(effects, doses, p, relevance, direction = "decreasing") {
  e <- if (direction == "decreasing") -effects else effects
  active <- doses > 0
  best <- max(e[active])
  ok <- active & e >= p * best & e >= relevance
  if (!any(ok)) NA_real_ else min(doses[ok])
}

#' The generalised MCP-Mod analysis of the SAP on estimated means.
#'
#' @param means,cov_means Estimated means by dose and their covariance (as from [mmrm_means_201()]).
#' @param doses Doses (mg).
#' @param sap The result of [read_sap()] for FJF-2026-201.
#' @return A list: `mct` (tibble of contrast tests), `signal`, `model` (selected model or
#'   `NA`), `effects` (fitted effects at the doses), `target_dose` and `gaic`.
mcpmod_201 <- function(means, cov_means, doses, sap) {
  mcp <- sap$primary$mcp
  mods <- candidate_models_201(sap, doses, max_effect = -1)
  test <- DoseFinding::MCTtest(
    doses, means,
    S = cov_means, type = "general", models = mods, alpha = mcp$alpha_one_sided
  )
  adj <- attr(test$tStat, "pVal")
  mct <- tibble::tibble(model = names(test$tStat), t_stat = unname(test$tStat), adjusted_p = unname(adj))
  significant <- mct$model[mct$adjusted_p < mcp$alpha_one_sided]
  out <- list(
    mct = mct, signal = length(significant) > 0, model = NA_character_, effects = NULL,
    target_dose = NA_real_, gaic = NULL
  )
  if (!out$signal) {
    return(out)
  }
  bnds <- DoseFinding::defBnds(max(doses))
  fits <- purrr::map(stats::setNames(significant, significant), function(m) {
    tryCatch(
      DoseFinding::fitMod(doses, means, S = cov_means, model = m, type = "general", bnds = bnds[[m]]),
      error = function(e) NULL
    )
  }) |> purrr::compact()
  if (length(fits) == 0) {
    return(out)
  }
  gaic <- vapply(fits, DoseFinding::gAIC, numeric(1))
  best <- names(which.min(gaic))
  effects <- stats::predict(fits[[best]], predType = "effect-curve", doseSeq = doses)
  td <- sap$primary$mod$target_dose
  out$model <- best
  out$gaic <- tibble::tibble(model = names(gaic), gaic = unname(gaic))
  out$effects <- stats::setNames(effects, doses)
  out$target_dose <- target_dose_201(effects, doses, td$p, td$clinical_relevance, mcp$direction)
  out
}

#' Model averaging (S1): bootstrap fits of every candidate model, weighted by gAIC.
#'
#' @inheritParams mcpmod_201
#' @return A list: `effects` (median bootstrap effect at each dose) and `target_dose`.
model_averaging_201 <- function(means, cov_means, doses, sap) {
  s1 <- purrr::keep(sap$sensitivity, \(x) x$id == "S1")[[1]]
  models <- purrr::map_chr(sap$primary$mcp$candidate_models, "model")
  ma <- withr::with_seed(s1$settings$seed, DoseFinding::maFitMod(
    doses, means,
    S = cov_means, models = models, nSim = s1$settings$bootstrap, bnds = DoseFinding::defBnds(max(doses))
  ))
  pred <- stats::predict(ma, doseSeq = doses, summaryFct = NULL)
  effects <- apply(pred - pred[, 1], 2, stats::median)
  td <- sap$primary$mod$target_dose
  list(
    effects = stats::setNames(effects, doses),
    target_dose = target_dose_201(effects, doses, td$p, td$clinical_relevance, sap$primary$mcp$direction)
  )
}

#' Every efficacy analysis of the FJF-2026-201 SAP on one dataset.
#'
#' @param adeff Efficacy dataset (as from [simulate_dry_run_201()]).
#' @param sap The result of [read_sap()] for FJF-2026-201.
#' @param protocol The result of [read_protocol()] for FJF-2026-201.
#' @param averaging Run the model-averaging sensitivity analysis (slower).
#' @return A list: `means` (A1's estimates), `a1` (the MCP-Mod result), `s1` and `s2`.
analyse_201 <- function(adeff, sap, protocol, averaging = TRUE) {
  visit <- sprintf("Week %d", protocol$primary_visit_week)
  covs <- unlist(sap$primary$covariance_fallback)
  hyp <- adeff
  hyp$CHG[hyp$ONTRTFL != "Y" | hyp$PRERESFL != "Y"] <- NA
  est <- mmrm_means_201(hyp, visit, covs)
  all_obs <- mmrm_means_201(adeff, visit, covs)
  list(
    means = est,
    a1 = mcpmod_201(est$means, est$S, est$doses, sap),
    s1 = if (averaging) model_averaging_201(est$means, est$S, est$doses, sap) else NULL,
    s2 = mcpmod_201(all_obs$means, all_obs$S, all_obs$doses, sap)
  )
}

#' Share of patients with no week-12 value under the hypothetical estimand, planned.
#'
#' Discontinuation, rescue or withdrawal before the primary visit (12-week rates, averaged
#' over the arms), or a single missing value.
#'
#' @param assumptions The result of [read_assumptions()].
#' @param protocol The result of [read_protocol()] for FJF-2026-201.
#' @param missing_at_visit Probability that a single scheduled value is missing.
#' @return A proportion.
planned_missing_201 <- function(assumptions, protocol, missing_at_visit) {
  pars <- planning_values_dry_run_301(assumptions)
  w <- protocol$primary_visit_week
  k <- length(protocol$arms)
  rate <- function(p) rescale_probability(p, w)
  disc <- (rate(pars$discontinuation_placebo) + (k - 1) * rate(pars$discontinuation_active)) / k
  rescue <- (rate(pars$rescue_placebo) + (k - 1) * rate(pars$rescue_active)) / k
  1 - (1 - disc) * (1 - rescue) * (1 - rate(pars$study_withdrawal)) * (1 - missing_at_visit)
}

#' Power of the MCP step over a grid of sample sizes (DoseFinding::powMCT).
#'
#' Power is computed under each candidate model and under the planning Emax curve, all with
#' the planning maximum effect over the dose range. The number of patients is reduced by
#' the planned share with no week-12 value, which is conservative: the MMRM also uses the
#' earlier visits of those patients.
#'
#' @param assumptions The result of [read_assumptions()].
#' @param protocol The result of [read_protocol()] for FJF-2026-201.
#' @param sap The result of [read_sap()] for FJF-2026-201.
#' @param reference_dose The phase III dose (mg).
#' @return A tibble: n_per_arm, n_effective, scenario, power.
power_mcp_201 <- function(assumptions, protocol, sap, reference_dose) {
  doses <- purrr::map_dbl(protocol$arms, "dose_mg")
  curve <- planning_curve_201(assumptions, min(doses[doses > 0]), reference_dose)
  max_effect <- emax_effect(curve, max(doses))
  sigma <- assumptions$parameters$planning[assumptions$parameters$id == "change_sd"]
  missing <- planned_missing_201(assumptions, protocol, sap$dry_run$missing_at_visit)
  mods <- candidate_models_201(sap, doses, max_effect)
  planning <- DoseFinding::Mods(
    emax = curve$ed50, doses = doses, placEff = 0, maxEff = max_effect, direction = sap$primary$mcp$direction
  )
  contrasts <- DoseFinding::optContr(mods, w = rep(1, length(doses)))
  g <- sap$sample_size$grid
  purrr::map_dfr(seq(g$from, g$to, by = g$by), function(n) {
    n_eff <- floor(n * (1 - missing))
    alpha <- sap$primary$mcp$alpha_one_sided
    p_cand <- DoseFinding::powMCT(contrasts, alpha = alpha, altModels = mods, n = n_eff, sigma = sigma)
    p_plan <- DoseFinding::powMCT(contrasts, alpha = alpha, altModels = planning, n = n_eff, sigma = sigma)
    tibble::tibble(
      n_per_arm = n, n_effective = n_eff,
      scenario = c(names(p_cand), "planning Emax curve"), power = c(unname(p_cand), unname(p_plan))
    )
  })
}

#' Smallest sample size whose power reaches the target under every scenario.
#'
#' @param power_table The result of [power_mcp_201()].
#' @param target Target power.
#' @return Patients per arm, or `NA`.
choose_sample_size_201 <- function(power_table, target) {
  worst <- tapply(power_table$power, power_table$n_per_arm, min)
  ok <- as.numeric(names(worst))[worst >= target]
  if (length(ok) == 0) NA_real_ else min(ok)
}

#' Operating characteristics of the 201 design on dry-run trials.
#'
#' Each trial is generated from the planning values and analysed by A1 (MCP-Mod, without
#' model averaging). Each trial has its own seed, so results do not depend on the cores.
#'
#' @inheritParams simulate_dry_run_201
#' @param sap The result of [read_sap()] for FJF-2026-201.
#' @param trials Number of trials.
#' @param cores Parallel workers.
#' @return A tibble with one row per trial: trial, signal, model, target_dose and the
#'   estimated effect at each dose (columns effect_<dose>).
operating_characteristics_201 <- function(n_per_arm, assumptions, protocol, sap, reference_dose, trials, seed,
                                          cores = 1) {
  one <- function(i) {
    dry <- simulate_dry_run_201(
      n_per_arm, assumptions, protocol, reference_dose, seed + i, sap$dry_run$missing_at_visit
    )
    res <- analyse_201(dry$adeff, sap, protocol, averaging = FALSE)
    raw <- res$means$means - res$means$means[1]
    tibble::tibble(
      trial = i, signal = res$a1$signal, model = res$a1$model, target_dose = res$a1$target_dose,
      !!!stats::setNames(as.list(unname(raw)), paste0("effect_", res$means$doses))
    )
  }
  runs <- if (cores > 1) parallel::mclapply(seq_len(trials), one, mc.cores = cores) else lapply(seq_len(trials), one)
  dplyr::bind_rows(runs)
}
