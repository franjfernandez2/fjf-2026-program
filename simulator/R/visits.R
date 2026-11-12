# Visits and the efficacy measurements taken at them: HbA1c, fasting plasma glucose and
# weight, with the rescue rule and the intercurrent events applied.

#' Simulate the visits and efficacy values of the randomised patients.
#'
#' HbA1c change at week t: placebo profile + drug effect while on treatment (fading
#' linearly over the washout after stopping) + rescue benefit after rescue (building up over
#' the same time) + a residual shared by the patient's visits + a visit residual. Effects
#' build up as [build_up()]. Rescue follows the patient's drawn rescue event: at the chosen
#' visit the fasting glucose is above the protocol threshold and is confirmed at an
#' unscheduled visit; at the other visits under the rule it is below the threshold.
#'
#' @param design The result of [read_design()].
#' @param truth An `fjf_truth`.
#' @param study "201" or "301".
#' @param pt The `patients` element of [simulate_patients()].
#' @return A list: `visits` (one row per patient and visit, scheduled or unscheduled) and
#'   `patients` (with `rescue_week` and the dates of the intercurrent events added).
simulate_visits <- function(design, truth, study, pt) {
  p <- design$protocols[[study]]
  sh <- shared_settings
  v <- protocol_visits(p)
  n <- nrow(pt)
  nv <- nrow(v)
  curve <- true_curve(truth)
  primary <- p$primary_visit_week
  tw <- p$treatment_weeks
  rho <- truth$visit_correlation

  long <- data.frame(
    row = seq_len(n * nv),
    pt = rep(seq_len(n), each = nv),
    code = rep(v$code, n),
    week = rep(v$week, n),
    window = rep(v$window_days, n)
  )
  long$assessments <- rep(v$assessments, n)
  # Random draws, all of them for every row and in a fixed order.
  jitter <- round(stats::rnorm(nrow(long), 0, long$window / 2))
  missed <- stats::runif(nrow(long)) < sh$missing_at_visit
  own <- stats::rnorm(nrow(long), 0, truth$change_sd * sqrt(1 - rho))
  fpg_noise <- stats::rnorm(nrow(long), 0, sh$fpg[["sd"]])
  fpg_margin <- stats::runif(nrow(long), 1, 25)
  w_shared <- stats::rnorm(n, 0, truth$weight_change_sd * sqrt(rho))
  w_own <- stats::rnorm(nrow(long), 0, truth$weight_change_sd * sqrt(1 - rho))
  w_screen <- stats::rnorm(n, 0, 0.8)
  confirm_days <- sample(2:7, n, replace = TRUE)
  start_days <- sample(0:3, n, replace = TRUE)
  rescue_fpg <- stats::runif(n, 5, 60)
  confirm_fpg <- stats::runif(n, 1, 50)

  jitter <- pmax(pmin(jitter, long$window), -long$window)
  rand <- pt$randomisation_date[long$pt]
  long$date <- rand + 7 * long$week + jitter
  long$date[long$week < 0] <- pt$screening_date[long$pt[long$week < 0]]
  long$date[long$week == 0] <- rand[long$week == 0]
  long$t <- as.numeric(long$date - rand) / 7
  long$done <- !(missed & long$week > 0)
  # Nothing after withdrawal; the follow-up visit only for patients who finish the study.
  withdrawn <- pt$withdrawal_week[long$pt]
  keep <- long$t <= withdrawn & !(long$week > primary & is.finite(withdrawn))
  long <- long[keep, ]
  own <- own[keep]
  fpg_noise <- fpg_noise[keep]
  fpg_margin <- fpg_margin[keep]
  w_own <- w_own[keep]

  # Rescue: at a visit under the rule, before the primary visit.
  long$threshold <- fpg_threshold(p, long$week)
  for (a in c("hba1c", "fpg", "weight", "vitals", "dispense", "accountability", "labs", "egfr", "ae", "hypo")) {
    long[[paste0("has_", a)]] <- vapply(long$assessments, \(x) a %in% x, logical(1))
  }
  long$assessments <- NULL
  candidate <- long$done & long$has_fpg & !is.na(long$threshold) & long$week >= 2 & long$week < primary
  pt$rescue_week <- Inf
  pt$rescue_visit_row <- NA_integer_
  for (i in which(pt$rescue_draw)) {
    rows <- which(long$pt == i & candidate)
    if (length(rows) == 0) next
    r <- rows[ceiling(pt$rescue_u[i] * length(rows))]
    pt$rescue_visit_row[i] <- r
    pt$rescue_week[i] <- long$t[r] + (confirm_days[i] + start_days[i]) / 7
  }
  pt$confirm_date <- as.Date(NA)
  ok <- !is.na(pt$rescue_visit_row)
  pt$confirm_date[ok] <- long$date[pt$rescue_visit_row[ok]] + confirm_days[ok]
  pt$rescue_start_date <- pt$confirm_date + start_days
  # A confirmation visit after withdrawal does not happen, and nor does the rescue.
  late <- ok & as.numeric(pt$confirm_date - pt$randomisation_date) / 7 > pt$withdrawal_week
  pt$rescue_week[late] <- Inf
  pt$rescue_visit_row[late] <- NA
  pt$confirm_date[late] <- NA
  pt$rescue_start_date[late] <- NA

  # HbA1c change at the actual week of each visit.
  i <- long$pt
  t <- pmax(long$t, 0)
  disc <- pt$disc_week[i]
  fade <- ifelse(t <= disc, 1, pmax(0, 1 - (t - disc) / sh$washout_weeks))
  drug <- dose_effect(curve, pt$dose_mg[i]) * build_up(pmin(t, disc), curve$tau) * fade
  rw <- pt$rescue_week[i]
  rescue <- ifelse(t > rw, truth$rescue_benefit * pmin(1, (t - rw) / sh$washout_weeks), 0)
  chg <- curve$placebo * build_up(t, curve$tau) + drug + rescue + pt$shared[i] + own
  hba1c <- ifelse(long$week < 0, pt$hba1c_screen[i], pt$baseline_hba1c[i] + ifelse(long$week == 0, 0, chg))
  long$hba1c <- round(hba1c, 1)

  # Fasting plasma glucose, consistent with the rescue events.
  fpg <- sh$fpg[["slope"]] * hba1c + sh$fpg[["intercept"]] + fpg_noise
  is_rescue_visit <- seq_len(nrow(long)) %in% pt$rescue_visit_row
  before_rescue <- long$t < rw
  cap <- !is.na(long$threshold) & before_rescue & !is_rescue_visit
  fpg[cap] <- pmin(fpg[cap], long$threshold[cap] - fpg_margin[cap])
  at_rand <- long$week == 0
  fpg[at_rand] <- pmin(fpg[at_rand], 270 - fpg_margin[at_rand])
  fpg[is_rescue_visit] <- long$threshold[is_rescue_visit] + rescue_fpg[i[is_rescue_visit]]
  long$fpg <- round(fpg)

  # Weight: as HbA1c, without a rescue effect; the dose effect has the shape of the curve.
  w_effect <- truth$weight_effect_target_dose * dose_effect(curve, pt$dose_mg[i]) / dose_effect(curve, 20)
  w_chg <- truth$weight_change_placebo * build_up(t, curve$tau) +
    w_effect * build_up(pmin(t, disc), curve$tau) * fade + w_shared[i] + w_own
  weight <- ifelse(long$week < 0, pt$baseline_weight[i] + w_screen[i],
    ifelse(long$week == 0, pt$baseline_weight[i], pt$baseline_weight[i] + w_chg)
  )
  long$weight <- round(weight, 1)

  # Unscheduled confirmation visits.
  conf <- which(!is.na(pt$confirm_date))
  if (length(conf) > 0) {
    uns <- long[pt$rescue_visit_row[conf], ]
    uns$row <- NA_integer_
    uns$code <- "UNS"
    uns$date <- pt$confirm_date[conf]
    uns$t <- as.numeric(uns$date - pt$randomisation_date[conf]) / 7
    uns$fpg <- round(uns$threshold + confirm_fpg[conf])
    uns$hba1c <- NA_real_
    uns$weight <- NA_real_
    flags <- grep("^has_", names(uns), value = TRUE)
    uns[flags] <- FALSE
    uns$has_fpg <- TRUE
    uns$week <- NA_real_
    uns$window <- NA_real_
    uns$threshold <- NA_real_
    long <- rbind(long, uns)
  }
  long <- long[order(long$pt, long$date, long$code == "UNS"), ]
  long$subject_id <- pt$subject_id[long$pt]

  # Dates of the intercurrent events.
  pt$last_dose_date <- pt$randomisation_date + ifelse(is.finite(pt$disc_week), floor(pt$disc_week * 7), 7 * tw)
  pt$withdrawal_date <- pt$randomisation_date + ifelse(is.finite(pt$withdrawal_week), floor(pt$withdrawal_week * 7), NA)
  rownames(long) <- NULL
  list(visits = long, patients = pt)
}
