# Key secondary endpoints, subgroups and multiplicity of FJF-2026-301 (SAP part 2). Settings
# are read from the SAP file, as for the primary analyses (R/efficacy_301.R).

#' Subgroup variables known to the code, with their derivation from ADSL.
#'
#' @param adsl Subject-level dataset with USUBJID, AGE, SEX and BMIBL.
#' @return A data frame with USUBJID and one column per subgroup variable (except STRATUM,
#'   which is already in the efficacy data).
subgroup_data_301 <- function(adsl) {
  data.frame(
    USUBJID = adsl$USUBJID,
    AGEGR = ifelse(adsl$AGE < 65, "<65", ">=65"),
    SEX = adsl$SEX,
    BMIGR = ifelse(adsl$BMIBL < 30, "<30", ">=30")
  )
}

#' HbA1c data for the imputation, with the subgroup variables carried along.
#'
#' rbmi keeps extra columns in every imputed dataset, so the subgroup analysis finds them
#' there; joining by subject inside the analysis step would not be safe.
#'
#' @param dry A list with `adsl` and `adeff`.
#' @param reference Reference arm code.
#' @return The result of [prepare_efficacy_301()] with AGEGR, SEX and BMIGR added.
hba1c_data_301 <- function(dry, reference) {
  data <- prepare_efficacy_301(dry$adeff, reference, "HBA1C")
  groups <- subgroup_data_301(dry$adsl)
  i <- match(as.character(data$USUBJID), groups$USUBJID)
  stopifnot(!anyNA(i))
  cbind(data, groups[i, setdiff(names(groups), "USUBJID")])
}

#' Subgroup ids the code can derive.
subgroup_ids_301 <- c("STRATUM", "AGEGR", "SEX", "BMIGR")

# The coefficient of the active arm in a model whose arm factor is ARMCD.
arm_term <- function(model) {
  term <- grep("^ARMCD", names(stats::coef(model)), value = TRUE)
  term[!grepl(":", term)][1]
}

#' Responder analysis of one imputed dataset (B1), for rbmi::analyse().
#'
#' @param data One imputed dataset.
#' @param visit Label of the primary visit.
#' @param threshold HbA1c threshold (%); a responder is below it.
#' @param ... Ignored.
#' @return A list of estimates for rbmi: log odds ratio, difference and arm proportions.
responder_fun_301 <- function(data, visit, threshold, ...) {
  w <- data[data$AVISIT == visit, ]
  w$responder <- as.integer(w$BASE + w$CHG < threshold)
  model <- stats::glm(responder ~ ARMCD + BASE + STRATUM, family = stats::binomial(), data = w)
  term <- arm_term(model)
  arms <- levels(w$ARMCD)
  p <- vapply(arms, \(a) mean(w$responder[w$ARMCD == a]), numeric(1))
  n <- vapply(arms, \(a) sum(w$ARMCD == a), numeric(1))
  se_p <- sqrt(p * (1 - p) / n)
  list(
    log_or = list(est = unname(stats::coef(model)[term]), se = sqrt(stats::vcov(model)[term, term]), df = NA),
    difference = list(est = unname(p[2] - p[1]), se = sqrt(sum(se_p^2)), df = NA),
    proportion_reference = list(est = unname(p[1]), se = unname(se_p[1]), df = NA),
    proportion_active = list(est = unname(p[2]), se = unname(se_p[2]), df = NA)
  )
}

#' Subgroup analysis of one imputed dataset, for rbmi::analyse().
#'
#' Within each level, the ANCOVA of A1 (without the stratum when it is the subgroup). Then
#' the arm by subgroup interaction, a single coefficient since every subgroup is binary.
#'
#' @param data One imputed dataset, with the subgroup columns (see [hba1c_data_301()]).
#' @param visit Label of the primary visit.
#' @param subgroups The SAP's subgroup variables.
#' @param ... Ignored.
#' @return A list of estimates for rbmi, named `level_<i>_<j>` and `interaction_<i>`.
subgroup_fun_301 <- function(data, visit, subgroups, ...) {
  w <- data[data$AVISIT == visit, ]
  out <- list()
  for (i in seq_along(subgroups)) {
    sg <- subgroups[[i]]
    covariates <- if (sg$id == "STRATUM") "BASE" else c("BASE", "STRATUM")
    for (j in seq_along(sg$levels)) {
      sub <- w[w[[sg$id]] == sg$levels[[j]], ]
      model <- stats::lm(stats::reformulate(c("ARMCD", covariates), "CHG"), data = sub)
      term <- arm_term(model)
      out[[sprintf("level_%d_%d", i, j)]] <- list(
        est = unname(stats::coef(model)[term]), se = sqrt(stats::vcov(model)[term, term]), df = model$df.residual
      )
    }
    w$group <- factor(w[[sg$id]], levels = unlist(sg$levels))
    model <- stats::lm(stats::reformulate(c("ARMCD * group", covariates), "CHG"), data = w)
    term <- grep("^ARMCD.*:group", names(stats::coef(model)), value = TRUE)
    out[[sprintf("interaction_%d", i)]] <- list(
      est = unname(stats::coef(model)[term]), se = sqrt(stats::vcov(model)[term, term]), df = model$df.residual
    )
  }
  out
}

#' Cox model of the time to first rescue (B3), stratified by the stratum.
#'
#' @param adtte Time-to-event dataset with ARMCD, STRATUM, AVAL (days) and CNSR.
#' @param reference Reference arm code.
#' @return A tibble: estimate (hazard ratio), lower, upper, p_value, events by arm.
fit_cox_301 <- function(adtte, reference) {
  d <- adtte
  d$ARMCD <- factor(d$ARMCD, levels = c(reference, setdiff(unique(d$ARMCD), reference)))
  model <- survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ ARMCD + survival::strata(STRATUM), data = d)
  co <- summary(model)$coefficients
  b <- co[1, "coef"]
  se <- co[1, "se(coef)"]
  tibble::tibble(
    estimate = exp(b), se = se, lower = exp(b - stats::qnorm(0.975) * se), upper = exp(b + stats::qnorm(0.975) * se),
    p_value = co[1, "Pr(>|z|)"],
    events_reference = sum(d$CNSR == 0 & d$ARMCD == reference),
    events_active = sum(d$CNSR == 0 & d$ARMCD != reference)
  )
}

#' Kaplan-Meier estimates of the time to first rescue, by arm, for the figure.
#'
#' @param adtte Time-to-event dataset.
#' @return A tibble: ARMCD, time (days), survival.
km_301 <- function(adtte) {
  fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ ARMCD, data = adtte)
  arm <- rep(sub("ARMCD=", "", names(fit$strata)), fit$strata)
  starts <- tibble::tibble(ARMCD = unique(arm), time = 0, survival = 1)
  dplyr::bind_rows(starts, tibble::tibble(ARMCD = arm, time = fit$time, survival = fit$surv))
}

# Pooled rbmi results as a data frame indexed by parameter.
pool_df <- function(analysis) {
  x <- as.data.frame(rbmi::pool(analysis))
  rownames(x) <- x$parameter
  x
}

#' Every key secondary and subgroup analysis of SAP part 2.
#'
#' @param dry A list with `adsl`, `adeff` and `adtte` (as from [simulate_dry_run_301()]).
#' @param sap The result of [read_sap()].
#' @param protocol The result of [read_protocol()].
#' @param hba1c_draws Draws of the HbA1c imputation model, the ones A1 uses, made from
#'   [hba1c_data_301()] so that the imputed datasets carry the subgroup variables.
#' @param imputations Imputations for body weight; by default the SAP's.
#' @return A list: `results` (one row per analysis, estimate on its reporting scale, with
#'   its null value and favourable direction), `subgroups` and `km`.
analyse_secondary_301 <- function(dry, sap, protocol, hba1c_draws, imputations = NULL) {
  visit <- sprintf("Week %d", protocol$primary_visit_week)
  a1 <- sap_item(sap, "A1")
  reference <- a1$settings$reference
  hba1c <- hba1c_data_301(dry, reference)
  refs <- stats::setNames(rep(reference, 2), levels(hba1c$ARMCD))
  imputed <- withr::with_seed(
    a1$settings$seed,
    rbmi::impute(
      hba1c_draws,
      references = refs, update_strategy = withdrawal_ice_301(hba1c, a1$settings$strategy_after_withdrawal)
    )
  )

  b1 <- sap_item(sap, "B1")
  r <- pool_df(rbmi::analyse(imputed, responder_fun_301, visit = visit, threshold = b1$settings$threshold))
  b1_row <- tibble::tibble(
    analysis = "B1", parameter = "Odds ratio", estimate = exp(r["log_or", "est"]), se = r["log_or", "se"],
    lower = exp(r["log_or", "lci"]), upper = exp(r["log_or", "uci"]), p_value = r["log_or", "pval"],
    null = 1, favourable = b1$favourable,
    proportion_reference = r["proportion_reference", "est"], proportion_active = r["proportion_active", "est"],
    difference = r["difference", "est"], difference_lower = r["difference", "lci"],
    difference_upper = r["difference", "uci"]
  )

  b2 <- sap_item(sap, "B2")
  weight <- prepare_efficacy_301(dry$adeff, reference, "WEIGHT")
  weight_draws <- draws_301(weight, imputations %||% b2$settings$imputations, b2$settings$seed)
  w <- pooled_ancova_301(
    weight_draws, weight, b2$settings$strategy_after_withdrawal, reference, visit, b2$settings$seed
  )
  b2_row <- tibble::tibble(
    analysis = "B2", parameter = "Difference (kg)", estimate = w$estimate, se = w$se,
    lower = w$lower, upper = w$upper, p_value = w$p_value, null = 0, favourable = b2$favourable
  )

  b3 <- sap_item(sap, "B3")
  cox <- fit_cox_301(dry$adtte, reference)
  b3_row <- tibble::tibble(
    analysis = "B3", parameter = "Hazard ratio", estimate = cox$estimate, se = cox$se,
    lower = cox$lower, upper = cox$upper, p_value = cox$p_value, null = 1, favourable = b3$favourable,
    events_reference = cox$events_reference, events_active = cox$events_active
  )

  subgroups <- sap$subgroups$variables
  s <- pool_df(rbmi::analyse(imputed, subgroup_fun_301, visit = visit, subgroups = subgroups))
  counts <- hba1c[hba1c$AVISIT == visit, ]
  sub_table <- purrr::imap_dfr(subgroups, function(sg, i) {
    purrr::imap_dfr(sg$levels, function(level, j) {
      key <- sprintf("level_%d_%d", i, j)
      tibble::tibble(
        subgroup = sg$label, level = level, n = sum(counts[[sg$id]] == level),
        estimate = s[key, "est"], lower = s[key, "lci"], upper = s[key, "uci"],
        interaction_p = s[sprintf("interaction_%d", i), "pval"]
      )
    })
  })

  list(
    results = dplyr::bind_rows(b1_row, b2_row, b3_row),
    subgroups = sub_table,
    km = km_301(dry$adtte)
  )
}

#' The rows the multiplicity graph tests: A1 and the key secondary analyses.
#'
#' @param efficacy The result of [analyse_efficacy_301()].
#' @param secondary The result of [analyse_secondary_301()].
#' @param sap The result of [read_sap()].
#' @return A tibble: analysis, estimate, p_value, null, favourable.
multiplicity_inputs_301 <- function(efficacy, secondary, sap) {
  a1 <- efficacy$results[efficacy$results$analysis == "A1", c("analysis", "estimate", "p_value")]
  a1$null <- 0
  a1$favourable <- sap_item(sap, "A1")$favourable
  dplyr::bind_rows(a1, secondary$results[, c("analysis", "estimate", "p_value", "null", "favourable")])
}

#' One-sided p-value in the favourable direction, from a two-sided one.
#'
#' @param p Two-sided p-value.
#' @param estimate Estimate on its reporting scale.
#' @param null Value of no effect (0 for differences, 1 for ratios).
#' @param favourable "lower" or "higher".
#' @return The one-sided p-value.
one_sided_p <- function(p, estimate, null, favourable) {
  good <- ifelse(favourable == "lower", estimate < null, estimate > null)
  ifelse(good, p / 2, 1 - p / 2)
}

#' The multiplicity graph of the SAP, as a graphicalMCP graph.
#'
#' @param sap The result of [read_sap()].
#' @return A graphicalMCP `initial_graph`.
multiplicity_graph_301 <- function(sap) {
  m <- sap$multiplicity
  ids <- purrr::map_chr(m$hypotheses, "id")
  weights <- stats::setNames(purrr::map_dbl(m$hypotheses, "weight"), ids)
  transitions <- do.call(rbind, lapply(ids, \(h) as.numeric(unlist(m$transitions[[h]]))))
  dimnames(transitions) <- list(ids, ids)
  graphicalMCP::graph_create(weights, transitions)
}

#' Test the primary and key secondary hypotheses through the multiplicity graph.
#'
#' @param results Rows with analysis, estimate, p_value, null and favourable, covering every
#'   analysis named in the graph.
#' @param sap The result of [read_sap()].
#' @return A tibble: hypothesis, label, analysis, one-sided and adjusted p-values, rejected.
test_multiplicity_301 <- function(results, sap) {
  m <- sap$multiplicity
  hyp <- tibble::tibble(
    hypothesis = purrr::map_chr(m$hypotheses, "id"),
    label = purrr::map_chr(m$hypotheses, "label"),
    analysis = purrr::map_chr(m$hypotheses, "analysis")
  )
  rows <- results[match(hyp$analysis, results$analysis), ]
  stopifnot(!anyNA(rows$p_value))
  p <- stats::setNames(one_sided_p(rows$p_value, rows$estimate, rows$null, rows$favourable), hyp$hypothesis)
  test <- graphicalMCP::graph_test_shortcut(multiplicity_graph_301(sap), p, alpha = m$alpha_one_sided)
  hyp |>
    dplyr::mutate(
      p_one_sided = unname(p),
      adjusted_p = unname(test$outputs$adjusted_p[.data$hypothesis]),
      rejected = unname(test$outputs$rejected[.data$hypothesis])
    )
}
