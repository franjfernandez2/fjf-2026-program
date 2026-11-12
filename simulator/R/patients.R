# Patients of one study: screening, randomisation, characteristics and intercurrent events.
# Everything here is drawn with the study's own random stream, in a fixed order, so the
# same seed always gives the same patients.

pick <- function(n, probs) sample(names(probs), n, replace = TRUE, prob = probs)

#' Screen patients until `n` are randomised, and randomise them.
#'
#' Patients are screened in date order at unevenly recruiting sites. A patient is eligible if
#' the screening HbA1c is in the protocol range and no other criterion fails. Eligible
#' patients are randomised in stratified permuted blocks of twice the number of arms.
#'
#' @param design The result of [read_design()].
#' @param truth An `fjf_truth`.
#' @param study "201" or "301".
#' @param n_per_arm Patients per arm.
#' @return A list: `screened` (one row per screened patient) and `patients` (one row per
#'   randomised patient, with arm, dose, stratum, baseline values, characteristics and
#'   intercurrent events in weeks).
simulate_patients <- function(design, truth, study, n_per_arm) {
  p <- design$protocols[[study]]
  st <- study_settings[[study]]
  sh <- shared_settings
  codes <- purrr::map_chr(p$arms, "code")
  doses <- purrr::map_dbl(p$arms, "dose_mg")
  k <- length(codes)
  n <- n_per_arm * k
  range <- unlist(p$eligibility$hba1c_screening_pct)

  # Screening, in batches until enough patients are eligible.
  pool <- 3 * n
  hba1c_screen <- round(stats::rnorm(pool, truth$hba1c_baseline_mean, truth$hba1c_baseline_sd), 1)
  other_fail <- stats::runif(pool) < sh$other_screen_failure
  in_range <- hba1c_screen >= range[1] & hba1c_screen <= range[2]
  eligible <- in_range & !other_fail
  stopifnot(sum(eligible) >= n)
  last <- which(cumsum(eligible) == n)[1]
  idx <- seq_len(last)
  site_weights <- stats::rgamma(st$sites, shape = 2)
  site <- sample.int(st$sites, last, replace = TRUE, prob = site_weights)
  screen_day <- sort(stats::runif(last, 0, st$recruitment_days))
  seq_in_site <- stats::ave(seq_len(last), site, FUN = seq_along)
  exclusion <- unlist(p$eligibility$exclusion)
  failed <- ifelse(
    eligible[idx], NA_character_,
    ifelse(!in_range[idx], unlist(p$eligibility$inclusion)[3], sample(exclusion, last, replace = TRUE))
  )
  screened <- data.frame(
    subject_id = sprintf("%s-%03d-%04d", study, site, seq_in_site),
    site_id = sprintf("%s-%03d", study, site),
    screening_date = as.Date(st$first_screening) + floor(screen_day),
    eligible = eligible[idx],
    failed_criterion = failed,
    hba1c_screen = hba1c_screen[idx]
  )
  screened$consent_date <- screened$screening_date - sample(0:3, last, replace = TRUE)

  # Randomisation: stratified permuted blocks.
  pt <- screened[screened$eligible, ]
  pt$stratum <- ifelse(pt$hba1c_screen > 8.5, ">8.5", "<=8.5")
  pt$randomisation_date <- pt$screening_date + 14 + sample(-7:7, n, replace = TRUE)
  pt <- pt[order(pt$randomisation_date, pt$subject_id), ]
  pt$arm_code <- NA_character_
  pt$randomisation_number <- NA_character_
  for (s in c("<=8.5", ">8.5")) {
    rows <- which(pt$stratum == s)
    blocks <- ceiling(length(rows) / (2 * k))
    list_arms <- unlist(lapply(seq_len(blocks), \(b) sample(rep(codes, 2))))
    pt$arm_code[rows] <- list_arms[seq_along(rows)]
    pt$randomisation_number[rows] <- sprintf("%d%04d", match(s, c("<=8.5", ">8.5")), seq_along(rows))
  }
  # The trial stops at n patients; arms can differ by less than a block.
  pt$dose_mg <- doses[match(pt$arm_code, codes)]
  pt$baseline_hba1c <- round(pt$hba1c_screen + stats::rnorm(n, 0, sh$baseline_noise_sd), 1)

  # Characteristics (no outcome depends on them).
  age_range <- unlist(p$eligibility$age_years)
  pt$age <- pmin(pmax(round(stats::rnorm(n, sh$age[["mean"]], sh$age[["sd"]])), age_range[1]), age_range[2])
  pt$birth_date <- pt$screening_date - round(pt$age * 365.25 + stats::runif(n, 0, 364))
  pt$sex <- ifelse(stats::runif(n) < sh$female_share, "F", "M")
  pt$race <- pick(n, sh$race)
  pt$ethnicity <- pick(n, sh$ethnicity)
  h <- sh$height_cm
  pt$height_cm <- round(ifelse(pt$sex == "M", stats::rnorm(n, h$M[1], h$M[2]), stats::rnorm(n, h$F[1], h$F[2])))
  years <- pmax(0.6, stats::rgamma(n, sh$diabetes_years[["shape"]], scale = sh$diabetes_years[["scale"]]))
  pt$diagnosis_date <- pt$screening_date - round(years * 365.25)
  pt$metformin_mg <- as.integer(pick(n, sh$metformin_mg))
  pt$baseline_weight <- round(pmin(pmax(stats::rnorm(n, sh$weight_kg[["mean"]], sh$weight_kg[["sd"]]), 50), 180), 1)

  # Intercurrent events, in weeks since randomisation (Inf: did not happen).
  weeks <- p$primary_visit_week
  active <- pt$dose_mg > 0
  rate <- function(id) rate_over(truth[[id]], weeks)
  p_disc <- ifelse(active, rate("discontinuation_active"), rate("discontinuation_placebo"))
  p_rescue <- ifelse(active, rate("rescue_active"), rate("rescue_placebo"))
  pt$disc_week <- ifelse(stats::runif(n) < p_disc, stats::runif(n, 0, p$treatment_weeks), Inf)
  pt$disc_reason <- pick(n, sh$treatment_discontinuation_reasons)
  pt$rescue_draw <- stats::runif(n) < p_rescue
  pt$rescue_u <- stats::runif(n)
  # Residual of the HbA1c change shared by all visits (exchangeable correlation).
  rho <- truth$visit_correlation
  pt$shared <- stats::rnorm(n, 0, truth$change_sd * sqrt(rho))
  # Withdrawal from the study. A share of withdrawals is missing not at random: those
  # patients are chosen more often among the poorer responders (larger shared residual).
  n_wd <- stats::rbinom(1, n, rate("study_withdrawal"))
  n_mnar <- round(n_wd * truth$mnar_share_of_dropouts)
  w_mnar <- exp(2 * pt$shared / (truth$change_sd * sqrt(rho)))
  mnar <- if (n_mnar > 0) sample.int(n, n_mnar, prob = w_mnar) else integer()
  rest <- setdiff(seq_len(n), mnar)
  mar <- if (n_wd - n_mnar > 0) rest[sample.int(length(rest), n_wd - n_mnar)] else integer()
  pt$withdrawal_week <- Inf
  pt$withdrawal_week[c(mnar, mar)] <- stats::runif(n_wd, 0, weeks)
  pt$withdrawal_reason <- pick(n, sh$study_withdrawal_reasons)
  pt$withdrawal_mnar <- seq_len(n) %in% mnar
  # Stopping the study stops the treatment.
  pt$disc_week <- pmin(pt$disc_week, pt$withdrawal_week)
  pt$disc_reason[pt$withdrawal_week <= pt$disc_week] <- pt$withdrawal_reason[pt$withdrawal_week <= pt$disc_week]
  rownames(pt) <- NULL
  list(screened = screened, patients = pt)
}
