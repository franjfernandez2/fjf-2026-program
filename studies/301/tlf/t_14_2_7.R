# T-14.2.7 Primary and key secondary hypotheses under the multiplicity graph.
# Source: the multiplicity procedure applied to A1, B1, B2 and B3.

#' @inheritParams t_14_2_1_301
t_14_2_7_301 <- function(adam, analysis, outputs) {
  m <- analysis$multiplicity
  rows <- table_rows_301(
    sprintf("%s %s", m$hypothesis, m$label),
    list(
      analysis = m$analysis, p = p_301(m$p_one_sided), adjusted = p_301(m$adjusted_p),
      decision = ifelse(m$rejected, "Rejected", "Not rejected")
    )
  )
  alpha <- analysis$sap$multiplicity$alpha_one_sided
  tlf_table_301("T-14.2.7", outputs,
    c(analysis = "Analysis", p = "One-sided|p-value", adjusted = "Adjusted|p-value", decision = "Decision"), rows,
    footnotes = c(
      sprintf(
        "Graphical procedure of the SAP; a hypothesis is rejected when its adjusted p-value is at most %s (one-sided).",
        alpha
      ),
      "One-sided p-values in the direction the SAP calls favourable for each endpoint."
    )
  )
}
