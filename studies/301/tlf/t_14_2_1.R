# T-14.2.1 Change from baseline in HbA1c at week 26, treatment-policy estimand (primary, A1).
# Source: ADEFF (observed values) and analysis A1 (multiple imputation, jump to reference).
# change_table_301() also makes T-14.2.2 and T-14.2.5.

#' A table of change from baseline at the primary visit: observed values by arm, then the
#' arms' LS means and their difference from the analysis.
#'
#' @param adam Named list of the ADaM datasets.
#' @param outputs The outputs specification.
#' @param id Output id.
#' @param paramcd ADEFF parameter.
#' @param result The analysis's row (estimate, lower, upper, p_value).
#' @param lsmeans The analysis's arm LS means (ARMCD, lsmean, se).
#' @param records Function of ADEFF selecting the observed records summarised.
#' @param visit Primary visit label.
#' @param digits Decimals of the data.
#' @param footnotes Footnotes.
change_table_301 <- function(adam, outputs, id, paramcd, result, lsmeans, records, visit, digits, footnotes) {
  adsl <- adam$ADSL[adam$ADSL$FASFL == "Y", ]
  arms <- arms_301(adsl, "TRT01P")
  adeff <- adam$ADEFF
  adeff <- adeff[adeff$FASFL == "Y" & adeff$PARAMCD == paramcd & is.na(adeff$DTYPE) & !is.na(adeff$AVAL), ]
  adeff <- adeff[records(adeff), ]
  col <- arm_key_301(adeff$TRTP, arms)
  base <- adeff[adeff$AVISIT == "Baseline", ]
  at_visit <- adeff[adeff$AVISIT == visit, ]
  summary_block <- function(label, d, var) {
    rows <- continuous_rows_301(d[[var]], arm_key_301(d$TRTP, arms), arms$key, digits)
    bind_rows_301(heading_301(label), rows[rows$label %in% c("n", "Mean (SD)"), ])
  }
  code_of <- stats::setNames(adsl$ARMCD, adsl$TRT01P)[arms$label]
  means <- lsmeans[match(code_of, lsmeans$ARMCD), ]
  ls_text <- sprintf("%s (%s)", fmt_num(means$lsmean, digits + 1), fmt_num(means$se, digits + 2))
  ls_cells <- stats::setNames(as.list(ls_text), arms$key)
  active <- arms$key[1]
  model_rows <- bind_rows_301(
    heading_301(sprintf("Analysis at %s", tolower(visit))),
    table_rows_301("LS mean change (SE)", ls_cells, 1L),
    table_rows_301(
      "Difference from placebo (95% CI)",
      stats::setNames(list(est_ci_301(result$estimate, result$lower, result$upper, digits + 1)), active), 1L
    ),
    table_rows_301("p-value (two-sided)", stats::setNames(list(p_301(result$p_value)), active), 1L)
  )
  rows <- bind_rows_301(
    summary_block("Baseline", base, "AVAL"),
    summary_block(visit, at_visit, "AVAL"),
    summary_block(sprintf("Change from baseline to %s", tolower(visit)), at_visit, "CHG"),
    model_rows
  )
  tlf_table_301(id, outputs, arm_columns_301(arms), rows, footnotes)
}

#' The primary visit's label.
primary_visit_301 <- function(analysis) sprintf("Week %d", analysis$protocol$primary_visit_week)

#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results: efficacy, secondary, multiplicity, lsmeans, sap, protocol.
#' @param outputs The outputs specification.
t_14_2_1_301 <- function(adam, analysis, outputs) {
  a1 <- sap_item(analysis$sap, "A1")
  change_table_301(
    adam, outputs, "T-14.2.1", "HBA1C",
    result = analysis$efficacy$results[analysis$efficacy$results$analysis == "A1", ],
    lsmeans = analysis$lsmeans[analysis$lsmeans$analysis == "A1", ],
    records = function(d) d$ANL01FL %in% "Y", visit = primary_visit_301(analysis), digits = 1,
    footnotes = c(
      "Observed values: all values, including those after treatment discontinuation or rescue medication.",
      sprintf(paste(
        "Analysis A1: missing values after withdrawal imputed by jump to reference (%d imputations), ANCOVA of",
        "change with baseline HbA1c and stratum, combined by Rubin's rules."
      ), a1$settings$imputations),
      "LS: least-squares. SD: standard deviation. SE: standard error. HbA1c in %."
    )
  )
}
