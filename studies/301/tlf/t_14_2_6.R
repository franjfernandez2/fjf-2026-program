# T-14.2.6 Time to first rescue medication (B3). Source: ADTTE (PARAMCD TTRESC) and analysis B3.

#' Kaplan-Meier proportion with an event by a day, by arm.
#' @param d ADTTE records of one parameter.
#' @param arm Arm label of each record.
#' @param day Day.
km_event_by_301 <- function(d, arm, day) {
  fit <- survival::survfit(survival::Surv(d$AVAL, 1 - d$CNSR) ~ arm)
  s <- summary(fit, times = day, extend = TRUE)
  stats::setNames(1 - s$surv, if (is.null(s$strata)) unique(arm) else sub("^arm=", "", s$strata))
}

#' @inheritParams t_14_2_1_301
t_14_2_6_301 <- function(adam, analysis, outputs) {
  adsl <- adam$ADSL[adam$ADSL$FASFL == "Y", ]
  arms <- arms_301(adsl, "TRT01P")
  d <- adam$ADTTE[adam$ADTTE$PARAMCD == "TTRESC" & adam$ADTTE$FASFL == "Y", ]
  col <- arm_key_301(d$TRTP, arms)
  denom <- stats::setNames(arms$n, arms$key)
  day <- 7 * analysis$protocol$primary_visit_week
  km <- km_event_by_301(d, d$TRTP, day)
  r <- analysis$secondary$results[analysis$secondary$results$analysis == "B3", ]
  active <- arms$key[1]
  one <- function(text) stats::setNames(list(text), active)
  rows <- bind_rows_301(
    categorical_rows_301(ifelse(d$CNSR == 0, "event", "censored"), col, arms$key, denom,
      c("Patients with rescue medication, n (%)" = "event", "Censored, n (%)" = "censored"),
      indent = 0L
    ),
    table_rows_301(
      sprintf("Kaplan-Meier proportion with rescue by day %d (%%)", day),
      stats::setNames(as.list(fmt_num(100 * km[arms$label], 1)), arms$key)
    ),
    heading_301("Analysis B3"),
    table_rows_301("Hazard ratio (95% CI)", one(est_ci_301(r$estimate, r$lower, r$upper, 2)), 1L),
    table_rows_301("p-value (two-sided)", one(p_301(r$p_value)), 1L)
  )
  tlf_table_301("T-14.2.6", outputs, arm_columns_301(arms), rows,
    footnotes = c(
      "Time from randomisation to the first rescue medication;",
      "patients without rescue are censored at their last contact.",
      "Analysis B3: Cox proportional-hazards model stratified by stratum; hazard ratio FJF-2026 over placebo."
    )
  )
}
