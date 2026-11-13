# T-14.2.2 Dose-response of HbA1c at week 12, MCP-Mod (A1, with S1 and S2).
# Source: the results of analyse_201().

#' @inheritParams t_14_2_1_201
t_14_2_2_201 <- function(adam, analysis, outputs) {
  r <- analysis$mcpmod
  mcp <- analysis$sap$primary$mcp
  td <- analysis$sap$primary$mod$target_dose
  doses <- r$means$doses
  cols <- c(a1 = "A1|Primary", s1 = "S1|Model averaging", s2 = "S2|All observed")
  mct_cells <- function(res, model) {
    m <- res$mct[res$mct$model == model, ]
    sprintf("%s (%s)", fmt_num(m$t_stat, 2), p_301(m$adjusted_p))
  }
  models <- r$a1$mct$model
  mct_rows <- do.call(bind_rows_301, lapply(models, function(m) {
    table_rows_301(model_label_201(m), list(a1 = mct_cells(r$a1, m), s2 = mct_cells(r$s2, m)), 1L)
  }))
  selected <- function(res) {
    if (is.na(res$model)) {
      return("None")
    }
    sprintf("%s (gAIC %s)", model_label_201(res$model), fmt_num(res$gaic$gaic[res$gaic$model == res$model], 2))
  }
  effect <- function(res, d) if (is.null(res$effects)) "" else fmt_num(unname(res$effects[doses == d]), 2)
  effect_rows <- do.call(bind_rows_301, lapply(doses[doses > 0], function(d) {
    table_rows_301(dose_label_201(d), list(a1 = effect(r$a1, d), s1 = effect(r$s1, d), s2 = effect(r$s2, d)), 1L)
  }))
  target <- function(res) {
    if (is.null(res)) "" else if (is.na(res$target_dose)) "None" else dose_label_201(res$target_dose)
  }
  yes_no <- function(x) if (x) "Yes" else "No"
  rows <- bind_rows_301(
    heading_301(sprintf("MCP step: contrast test, t (adjusted one-sided p)")),
    mct_rows,
    table_rows_301("Dose-response signal", list(a1 = yes_no(r$a1$signal), s2 = yes_no(r$s2$signal)), 1L),
    heading_301("Mod step"),
    table_rows_301("Selected model", list(a1 = selected(r$a1), s1 = "All, weighted by gAIC", s2 = selected(r$s2)), 1L),
    heading_301("Fitted effect against placebo"),
    effect_rows,
    table_rows_301("Target dose", list(a1 = target(r$a1), s1 = target(r$s1), s2 = target(r$s2)))
  )
  tlf_table_301("T-14.2.2", outputs, cols, rows,
    footnotes = c(
      sprintf(
        "MCP step: optimal contrasts of the candidate models, one-sided alpha %s, decreasing HbA1c.",
        format(mcp$alpha_one_sided)
      ),
      "Mod step: the significant model with the smallest generalised AIC (gAIC), fitted to the MMRM means.",
      sprintf(paste(
        "Target dose: the smallest dose with at least %d%% of the maximum effect and at least %s points",
        "better than placebo."
      ), round(100 * td$p), format(td$clinical_relevance)),
      "S1: median of the bootstrap model-averaged effects. Effects in HbA1c percentage points."
    )
  )
}
