# F-14.3.1 Kaplan-Meier curves of the time to first level 2 or 3 hypoglycaemia.
# Source: ADTTE (PARAMCD TTHYPO23), safety set.

#' @inheritParams f_14_2_1_301
f_14_3_1_301 <- function(adam, analysis, outputs, path, locked) {
  d <- adam$ADTTE[adam$ADTTE$PARAMCD == "TTHYPO23" & adam$ADTTE$SAFFL %in% "Y", ]
  plot <- km_plot_301(
    d, d$TRTA, "incidence", "Days from first dose", "Cumulative incidence of level 2 or 3 hypoglycaemia"
  )
  save_figure_301(plot, "F-14.3.1", outputs, path, locked,
    footnotes = c(
      "Patients without an episode are censored at their last contact.", "Numbers under the curves: patients at risk."
    )
  )
}
