# T-14.2.4 Patients with HbA1c below 7.0% at week 26 (B1).
# Source: ADEFF (observed values, CRIT1FL) and analysis B1 (multiple imputation, jump to reference).

#' @inheritParams t_14_2_1_301
t_14_2_4_301 <- function(adam, analysis, outputs) {
  adsl <- adam$ADSL[adam$ADSL$FASFL == "Y", ]
  arms <- arms_301(adsl, "TRT01P")
  visit <- primary_visit_301(analysis)
  d <- adam$ADEFF
  d <- d[d$FASFL == "Y" & d$PARAMCD == "HBA1C" & d$AVISIT == visit & d$ANL01FL %in% "Y", ]
  d <- d[is.na(d$DTYPE) & !is.na(d$AVAL), ]
  col <- arm_key_301(d$TRTP, arms)
  observed <- stats::setNames(lapply(arms$key, function(k) {
    yes <- d$CRIT1FL[col == k] %in% "Y"
    sprintf("%d/%d (%s%%)", sum(yes), length(yes), fmt_num(100 * mean(yes), 1))
  }), arms$key)
  r <- analysis$secondary$results[analysis$secondary$results$analysis == "B1", ]
  active <- arms$key[1]
  placebo <- arms$key[2]
  estimated <- stats::setNames(
    list(fmt_num(100 * r$proportion_active, 1), fmt_num(100 * r$proportion_reference, 1)), c(active, placebo)
  )
  one <- function(text) stats::setNames(list(text), active)
  rows <- bind_rows_301(
    table_rows_301(sprintf("Observed at %s, n/N (%%)", tolower(visit)), observed),
    heading_301("Analysis B1"),
    table_rows_301("Estimated proportion (%)", estimated, 1L),
    table_rows_301("Odds ratio (95% CI)", one(est_ci_301(r$estimate, r$lower, r$upper, 2)), 1L),
    table_rows_301(
      "Difference in proportions, % (95% CI)",
      one(est_ci_301(100 * r$difference, 100 * r$difference_lower, 100 * r$difference_upper, 1)), 1L
    ),
    table_rows_301("p-value (two-sided)", one(p_301(r$p_value)), 1L)
  )
  b1 <- sap_item(analysis$sap, "B1")
  tlf_table_301("T-14.2.4", outputs, arm_columns_301(arms), rows,
    footnotes = c(
      sprintf("Response: HbA1c below %s%% at %s.", b1$settings$threshold, tolower(visit)),
      paste(
        "Analysis B1: HbA1c imputed as for A1 (jump to reference), logistic regression of response with baseline",
        "HbA1c and stratum in each imputed dataset, combined by Rubin's rules."
      )
    )
  )
}
