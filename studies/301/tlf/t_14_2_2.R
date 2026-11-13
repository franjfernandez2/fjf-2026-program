# T-14.2.2 Change from baseline in HbA1c at week 26, hypothetical estimand (A2, MMRM).
# Source: ADEFF (values on treatment and before rescue) and analysis A2.

#' @inheritParams t_14_2_1_301
t_14_2_2_301 <- function(adam, analysis, outputs) {
  result <- analysis$efficacy$results[analysis$efficacy$results$analysis == "A2", ]
  covariance <- c(
    us = "unstructured", toeph = "heterogeneous Toeplitz", ar1h = "heterogeneous AR(1)",
    cs = "compound symmetry"
  )[[result$covariance]]
  change_table_301(
    adam, outputs, "T-14.2.2", "HBA1C",
    result = result, lsmeans = analysis$lsmeans[analysis$lsmeans$analysis == "A2", ],
    records = function(d) d$ANL01FL %in% "Y" & (d$AVISIT == "Baseline" | (d$ONTRTFL %in% "Y" & d$PRERESFL %in% "Y")),
    visit = primary_visit_301(analysis), digits = 1,
    footnotes = c(
      "Observed values: values on treatment and before rescue medication only.",
      sprintf(paste(
        "Analysis A2: mixed model for repeated measures of change, with baseline HbA1c by visit, stratum and",
        "treatment by visit; %s covariance; Kenward-Roger degrees of freedom (%.1f)."
      ), covariance, result$df),
      "LS: least-squares. SD: standard deviation. SE: standard error. HbA1c in %."
    )
  )
}
