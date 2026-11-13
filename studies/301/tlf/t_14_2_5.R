# T-14.2.5 Change from baseline in body weight at week 26 (B2).
# Source: ADEFF (observed weight) and analysis B2.

#' @inheritParams t_14_2_1_301
t_14_2_5_301 <- function(adam, analysis, outputs) {
  b2 <- sap_item(analysis$sap, "B2")
  change_table_301(
    adam, outputs, "T-14.2.5", "WEIGHT",
    result = analysis$secondary$results[analysis$secondary$results$analysis == "B2", ],
    lsmeans = analysis$lsmeans[analysis$lsmeans$analysis == "B2", ],
    records = function(d) d$ANL01FL %in% "Y", visit = primary_visit_301(analysis), digits = 1,
    footnotes = c(
      "Observed values: all values, including those after treatment discontinuation or rescue medication.",
      sprintf(paste(
        "Analysis B2: missing values after withdrawal imputed by %s (%d imputations), ANCOVA of change with",
        "baseline weight and stratum, combined by Rubin's rules."
      ), strategy_label_301(b2$settings$strategy_after_withdrawal), b2$settings$imputations),
      "LS: least-squares. SD: standard deviation. SE: standard error. Weight in kg."
    )
  )
}
