# T-14.2.3 Sensitivity analyses of the primary estimand (A1, S1, S2 and the tipping point of S3).
# Source: the efficacy analyses.

#' @inheritParams t_14_2_1_301
t_14_2_3_301 <- function(adam, analysis, outputs) {
  res <- analysis$efficacy$results
  row_of <- function(id, label) {
    r <- res[res$analysis == id, ]
    table_rows_301(label, list(
      strategy = strategy_label_301(r$strategy),
      diff = est_ci_301(r$estimate, r$lower, r$upper, 2),
      p = p_301(r$p_value)
    ))
  }
  tip <- analysis$efficacy$tipping
  tp <- analysis$efficacy$tipping_point
  tipping_text <- if (is.na(tp)) {
    sprintf("Not reached up to %s", fmt_num(max(tip$delta), 2))
  } else if (tp == min(tip$delta)) {
    "Not applicable: the 95% CI includes 0 without shift"
  } else {
    fmt_num(tp, 2)
  }
  rows <- bind_rows_301(
    row_of("A1", "A1 Primary analysis"),
    row_of("S1", "S1 Missing at random"),
    row_of("S2", "S2 Copy reference"),
    table_rows_301("S3 Tipping point (HbA1c %)", list(strategy = "delta-adjusted MAR", diff = tipping_text))
  )
  columns <- c(
    strategy = "Values missing|after withdrawal", diff = "Difference from|placebo (95% CI)", p = "p-value"
  )
  tlf_table_301("T-14.2.3", outputs, columns, rows,
    footnotes = c(
      "All analyses: ANCOVA of the change at week 26 with baseline HbA1c and stratum, multiple imputation,",
      "Rubin's rules.",
      "S3: imputed values of the FJF-2026 arm shifted by delta; the tipping point is the smallest delta at which the",
      "upper limit of the 95% CI reaches 0 (Figure 14.2.1)."
    )
  )
}
