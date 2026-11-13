# T-14.2.3 Clinical utility index by dose (X1, exploratory).
# Source: clinical_utility_201() (studies/201/analysis/cui.R).

#' @inheritParams t_14_2_1_201
t_14_2_3_201 <- function(adam, analysis, outputs) {
  u <- analysis$cui
  u <- u[order(-u$dose), ]
  w <- clinical_utility_weights_201(analysis$sap)
  cols <- c(
    effect = "HbA1c effect|vs placebo", disc = "AE leading to|discontinuation|n/N (%)",
    u_effect = "Effect|utility", u_safety = "Safety|utility", cui = "Clinical|utility|index"
  )
  rows <- table_rows_301(dose_label_201(u$dose), list(
    effect = fmt_num(u$effect, 2),
    disc = sprintf("%d/%d (%s%%)", u$n_disc, u$n, fmt_num(100 * u$rate, 1)),
    u_effect = fmt_num(u$u_effect, 2), u_safety = fmt_num(u$u_safety, 2), cui = fmt_num(u$cui, 2)
  ))
  source <- if (u$effect_source[1] == "MMRM") {
    "the MMRM difference from placebo"
  } else {
    sprintf("the fitted effect of the selected model (%s)", model_label_201(u$effect_source[1]))
  }
  tlf_table_301("T-14.2.3", outputs, cols, rows,
    footnotes = c(
      sprintf("HbA1c effect: %s, in percentage points.", source),
      "AE leading to discontinuation: treatment-emergent adverse event with action DRUG WITHDRAWN, by dose taken.",
      sprintf(paste(
        "Each utility is scaled linearly over the doses, 1 for the best and 0 for the worst;",
        "index = %s x effect + %s x safety (exploratory, ADR 0031)."
      ), format(w[["effect"]]), format(w[["safety"]]))
    )
  )
}
