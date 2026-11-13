# T-14.2.1 Change from baseline in HbA1c at week 12 by dose, hypothetical estimand (A1).
# Source: ADEFF (observed values on treatment and before rescue) and the MMRM of A1.

#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results: mcpmod ([analyse_201()]), cui, sap, protocol.
#' @param outputs The outputs specification.
t_14_2_1_201 <- function(adam, analysis, outputs) {
  adsl <- adam$ADSL[adam$ADSL$FASFL == "Y", ]
  arms <- arms_301(adsl, "TRT01P")
  visit <- primary_visit_301(analysis)
  e <- adam$ADEFF
  e <- e[e$FASFL == "Y" & e$PARAMCD == "HBA1C" & e$ANL01FL %in% "Y" & is.na(e$DTYPE) & !is.na(e$AVAL), ]
  base <- e[e$AVISIT == "Baseline", ]
  at_visit <- e[e$AVISIT == visit & e$ONTRTFL %in% "Y" & e$PRERESFL %in% "Y", ]
  block <- function(label, d, var) {
    rows <- continuous_rows_301(d[[var]], arm_key_301(d$TRTP, arms), arms$key, 1)
    bind_rows_301(heading_301(label), rows[rows$label %in% c("n", "Mean (SD)"), ])
  }
  est <- mmrm_estimates_201(analysis$mcpmod$means)
  est <- est[match(arm_doses_201(adsl, arms, "TRT01P"), est$dose), ]
  cells <- function(x) stats::setNames(as.list(ifelse(is.na(x), "", x)), arms$key)
  rows <- bind_rows_301(
    block("Baseline", base, "AVAL"),
    block(sprintf("Change from baseline to %s, on treatment and before rescue", tolower(visit)), at_visit, "CHG"),
    heading_301(sprintf("MMRM at %s", tolower(visit))),
    table_rows_301("Mean change (SE)", cells(sprintf("%s (%s)", fmt_num(est$mean, 2), fmt_num(est$se, 3))), 1L),
    table_rows_301(
      "Difference from placebo (95% CI)",
      cells(ifelse(is.na(est$diff), NA, est_ci_301(est$diff, est$lower, est$upper, 2))), 1L
    )
  )
  tlf_table_301("T-14.2.1", outputs, arm_columns_301(arms), rows,
    footnotes = c(
      "Values after treatment discontinuation or rescue medication are excluded (hypothetical estimand E1).",
      sprintf(paste(
        "MMRM: change on baseline, stratum, visit, arm, arm by visit and baseline by visit; %s covariance;",
        "95%% CI by the normal approximation, as in MCP-Mod."
      ), toupper(analysis$mcpmod$means$covariance)),
      "SD: standard deviation. SE: standard error. HbA1c in %."
    )
  )
}
