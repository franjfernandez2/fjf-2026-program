# Safety data: adverse events, hypoglycaemia and safety laboratory values. Drawn after the
# efficacy data, so adding them did not change any efficacy value.

#' Excess risk of the active drug at a dose: proportional to the dose, 20 mg as reference.
#'
#' @param base Risk on placebo.
#' @param at_20 Risk at 20 mg.
#' @param dose Dose in mg.
#' @return Risk at the dose, in [0, 0.99].
risk_at_dose <- function(base, at_20, dose) pmin(0.99, pmax(0, base + (at_20 - base) * dose / 20))

#' eGFR by the CKD-EPI 2021 equation (no race term).
#'
#' @param scr Serum creatinine (mg/dL).
#' @param age Age in years.
#' @param sex "M" or "F".
#' @return eGFR in mL/min/1.73 m2.
ckd_epi_2021 <- function(scr, age, sex) {
  kappa <- ifelse(sex == "F", 0.7, 0.9)
  alpha <- ifelse(sex == "F", -0.241, -0.302)
  142 * pmin(scr / kappa, 1)^alpha * pmax(scr / kappa, 1)^-1.200 * 0.9938^age * ifelse(sex == "F", 1.012, 1)
}

#' Simulate the safety data of the randomised patients.
#'
#' @param design The result of [read_design()].
#' @param truth An `fjf_truth`.
#' @param study "201" or "301".
#' @param pt,visits From [simulate_visits()].
#' @param rescue_drug Rescue drug of each patient (NA if not rescued), from the medication table.
#' @return A list: `adverse_events`, `hypoglycaemia` and `labs` (safety rows of the laboratory
#'   table), as data frames without record ids; and `latent`, the patient-level flags.
simulate_safety <- function(design, truth, study, pt, visits, rescue_drug) {
  p <- design$protocols[[study]]
  ss <- safety_settings
  n <- nrow(pt)
  weeks <- p$primary_visit_week
  dose <- pt$dose_mg
  last_week <- max(purrr::map_dbl(p$visits, "week"))
  stay_days <- ifelse(is.finite(pt$withdrawal_week), floor(pt$withdrawal_week * 7), 7 * last_week)
  end_date <- pt$randomisation_date + stay_days
  span <- as.numeric(end_date - pt$randomisation_date)
  over <- function(prob) rate_over(prob, weeks)

  # Patient-level events.
  any_ae <- stats::runif(n) < over(risk_at_dose(
    truth$ae_any_placebo, truth$ae_any_placebo * truth$ae_relative_risk_active, dose
  ))
  n_extra <- stats::rpois(n, ss$extra_events)
  gi <- stats::runif(n) < over(risk_at_dose(0, truth$gi_excess_active, dose))
  serious <- stats::runif(n) < over(truth$serious_ae)
  alt_high <- stats::runif(n) < over(risk_at_dose(ss$alt_placebo, truth$alt_3uln_active, dose))
  l1_active <- truth$hypoglycaemia_level1_active
  l2_active <- truth$hypoglycaemia_level2_active
  l1 <- stats::runif(n) < over(risk_at_dose(truth$hypoglycaemia_level1_placebo, l1_active, dose))
  l2 <- stats::runif(n) < over(risk_at_dose(l2_active / 3, l2_active, dose))
  l3 <- stats::runif(n) < over(ss$hypo_level3)
  after_rescue <- !is.na(rescue_drug) & rescue_drug %in% c("GLICLAZIDE", "INSULIN GLARGINE") &
    stats::runif(n) < ss$hypo_after_rescue

  term_of <- function(dict, k, weights = NULL) {
    pref <- sample(names(dict), k, replace = TRUE, prob = weights)
    vapply(pref, \(x) sample(dict[[x]], 1), character(1), USE.NAMES = FALSE)
  }
  day_in <- function(i, from = 0, to = span[i]) floor(stats::runif(length(i), from, pmax(from, to)))
  ae_rows <- function(i, term, start_day, kind) {
    if (length(i) == 0) {
      return(NULL)
    }
    data.frame(i = i, verbatim_term = term, start = pt$randomisation_date[i] + start_day, kind = kind)
  }

  # Background events, a gastrointestinal excess early on treatment, serious events, the
  # event behind each discontinuation for an adverse event, hepatic and hypoglycaemia events.
  bg <- rep(which(any_ae), 1 + n_extra[any_ae])
  ae <- list(ae_rows(bg, term_of(ss$background_terms, length(bg), ss$background_weights), day_in(bg), "background"))
  g <- which(gi)
  ae[[2]] <- ae_rows(g, term_of(ss$gi_terms, length(g)), day_in(g, 0, pmin(28, span[g])), "gi")
  s <- which(serious)
  ae[[3]] <- ae_rows(s, term_of(ss$serious_terms, length(s)), day_in(s), "serious")
  d <- which(is.finite(pt$disc_week) & pt$disc_reason == "ADVERSE EVENT")
  last_dose_day <- as.numeric(pt$last_dose_date[d] - pt$randomisation_date[d])
  d_term <- ifelse(dose[d] > 0 & stats::runif(length(d)) < 0.6,
    term_of(ss$gi_terms, length(d)), term_of(ss$background_terms, length(d), ss$background_weights)
  )
  ae[[4]] <- ae_rows(d, d_term, pmax(0, last_dose_day - sample(0:7, length(d), replace = TRUE)), "discontinuation")

  # Safety laboratory values.
  v <- visits[visits$done & (visits$has_labs | visits$has_egfr), ]
  lab_pat <- lapply(ss$labs, \(l) stats::rnorm(n, 0, l$patient_sd))
  cr <- ss$creatinine
  cr_mean <- ifelse(pt$sex == "F", cr$F, cr$M)
  cr_pat <- stats::rnorm(n, 0, cr$patient_sd)
  # Eligibility needs eGFR of at least 45: redraw the patient level where it is lower.
  for (k in 1:50) {
    low <- ckd_epi_2021(exp(cr_mean + cr_pat), pt$age, pt$sex) < 47
    if (!any(low)) break
    cr_pat[low] <- stats::rnorm(sum(low), 0, cr$patient_sd)
  }
  cr_pat[low] <- 0
  post <- which(v$week > 0 & v$has_labs)
  alt_row <- vapply(seq_len(n), function(i) {
    rows <- post[v$pt[post] == i]
    if (!alt_high[i] || length(rows) == 0) NA_integer_ else rows[sample.int(length(rows), 1)]
  }, integer(1))
  lab_rows <- list()
  for (test in names(ss$labs)) {
    l <- ss$labs[[test]]
    vv <- v[v$has_labs, ]
    value <- exp(l$log_mean + lab_pat[[test]][vv$pt] + stats::rnorm(nrow(vv), 0, l$visit_sd))
    hit <- which(v$has_labs) %in% stats::na.omit(alt_row)
    if (test == "ALT") value[hit] <- l$ref[2] * stats::runif(sum(hit), 3.1, 6)
    if (test == "AST") value[hit] <- l$ref[2] * stats::runif(sum(hit), 2, 5)
    digits <- if (test == "BILI") 1 else 0
    lab_rows[[test]] <- data.frame(
      subject_id = vv$subject_id, visit_code = vv$code, collection_date = crf_date(vv$date), test_code = test,
      result = formatC(value, format = "f", digits = digits), unit = l$unit, ref_low = l$ref[1], ref_high = l$ref[2],
      fasting = NA_character_, .date = vv$date
    )
  }
  ve <- v[v$has_egfr, ]
  noise <- stats::rnorm(nrow(ve), 0, cr$visit_sd)
  # At screening the patient met the eGFR criterion, so the value is not above their level.
  noise[ve$week < 0] <- pmin(noise[ve$week < 0], 0)
  scr <- exp(cr_mean[ve$pt] + cr_pat[ve$pt] + noise)
  female <- pt$sex[ve$pt] == "F"
  lab_rows$CREAT <- data.frame(
    subject_id = ve$subject_id, visit_code = ve$code, collection_date = crf_date(ve$date), test_code = "CREAT",
    result = formatC(scr, format = "f", digits = 2), unit = "mg/dL",
    ref_low = ifelse(female, cr$ref_F[1], cr$ref_M[1]), ref_high = ifelse(female, cr$ref_F[2], cr$ref_M[2]),
    fasting = NA_character_, .date = ve$date
  )
  lab_rows$EGFR <- data.frame(
    subject_id = ve$subject_id, visit_code = ve$code, collection_date = crf_date(ve$date), test_code = "EGFR",
    result = sprintf("%d", round(ckd_epi_2021(round(scr, 2), pt$age[ve$pt], pt$sex[ve$pt]))),
    unit = "mL/min/1.73m2", ref_low = 60, ref_high = NA_real_, fasting = NA_character_, .date = ve$date
  )
  labs <- do.call(rbind, unname(lab_rows))

  # Half the hepatic elevations are also reported as adverse events.
  h <- which(!is.na(alt_row))
  h <- h[stats::runif(length(h)) < 0.5]
  h_day <- as.numeric(v$date[alt_row[h]] - pt$randomisation_date[h])
  ae[[5]] <- ae_rows(h, sample(ss$hepatic_terms, length(h), replace = TRUE), h_day, "hepatic")

  # Hypoglycaemia episodes; level 2 and 3 episodes are also adverse events.
  ep <- list()
  one <- function(i, level, k) {
    if (length(i) == 0) {
      return(NULL)
    }
    i <- rep(i, k)
    glucose <- switch(level,
      `1` = sample(54:69, length(i), replace = TRUE),
      `2` = sample(38:53, length(i), replace = TRUE),
      `3` = sample(30:50, length(i), replace = TRUE)
    )
    data.frame(i = i, level = level, glucose = glucose, day = day_in(i))
  }
  ep[[1]] <- one(which(l1), "1", 1 + stats::rpois(sum(l1), 1))
  ep[[2]] <- one(which(l2), "2", 1)
  ep[[3]] <- one(which(l3), "3", 1)
  r <- which(after_rescue)
  if (length(r) > 0) {
    rescue_day <- as.numeric(pt$rescue_start_date[r] - pt$randomisation_date[r])
    ep[[4]] <- data.frame(
      i = r, level = "1", glucose = sample(54:69, length(r), replace = TRUE),
      day = floor(stats::runif(length(r), rescue_day, pmax(rescue_day, span[r])))
    )
  }
  ep <- do.call(rbind, ep)
  if (is.null(ep)) ep <- data.frame(i = integer(), level = character(), glucose = integer(), day = numeric())
  ep <- ep[order(ep$i, ep$day), ]
  hypo <- data.frame(
    subject_id = pt$subject_id[ep$i], event_seq = stats::ave(ep$i, ep$i, FUN = seq_along),
    event_date = crf_date(pt$randomisation_date[ep$i] + ep$day), glucose_mg_dl = ep$glucose, level = ep$level,
    assistance = ifelse(ep$level == "3", "Y", "N")
  )
  big <- ep[ep$level %in% c("2", "3"), ]
  ae[[6]] <- ae_rows(big$i, sample(ss$hypo_terms, nrow(big), replace = TRUE), big$day, paste0("hypo", big$level))

  # Attributes of each adverse event.
  ae <- do.call(rbind, ae)
  ae <- ae[order(ae$i, ae$start), ]
  m <- nrow(ae)
  sev <- pick(m, ss$severity)
  sev[ae$kind == "serious"] <- sample(c("MODERATE", "SEVERE"), sum(ae$kind == "serious"), replace = TRUE)
  sev[ae$kind == "hypo3"] <- "SEVERE"
  duration <- ceiling(stats::rgamma(m, 2, scale = 5))
  ongoing <- stats::runif(m) < ss$ongoing
  related_p <- ifelse(ae$kind %in% c("gi", "hepatic"), ss$related_gi, ss$related_other)
  on_drug <- ae$start <= pt$last_dose_date[ae$i]
  action <- ifelse(!on_drug, "NOT APPLICABLE",
    ifelse(stats::runif(m) < ss$interrupted, "DRUG INTERRUPTED", "DOSE NOT CHANGED")
  )
  action[ae$kind == "discontinuation"] <- "DRUG WITHDRAWN"
  end <- ae$start + duration
  end[ongoing] <- NA
  adverse_events <- data.frame(
    subject_id = pt$subject_id[ae$i], ae_seq = stats::ave(ae$i, ae$i, FUN = seq_along),
    verbatim_term = ae$verbatim_term, start_date = crf_date(ae$start), end_date = crf_date(end),
    severity = sev, serious = ifelse(ae$kind %in% c("serious", "hypo3"), "Y", "N"),
    related = ifelse(stats::runif(m) < related_p, "Y", "N"), action_taken = action,
    outcome = ifelse(ongoing, "NOT RECOVERED", "RECOVERED")
  )
  list(
    adverse_events = adverse_events, hypoglycaemia = hypo, labs = labs,
    latent = data.frame(
      subject_id = pt$subject_id, any_ae = any_ae, gi = gi, serious = serious, alt_high = alt_high & !is.na(alt_row),
      l1 = l1, l2 = l2, l3 = l3
    )
  )
}
