# Second programming of the tables of FJF-2026-201 (independent of studies/201/tlf/ and of
# studies/201/analysis/cui.R).
#
# Counts and summaries are recomputed with base R from the ADaM datasets, the MMRM differences
# and the clinical utility index from the MCP-Mod inputs and ADAE, and the model results are
# reformatted from analyse_201(). Each must appear, as printed, in the production table at the
# same section, row and column (compare_tlf_301()). Only the number formats (R/tlf.R) and the
# cell helpers of qc/301 are shared.

#' Expected cells of the tables of FJF-2026-201.
#'
#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results, as for [tables_201()].
#' @return A data frame of expected cells (id, section, label, column, value).
qc_tlf_201 <- function(adam, analysis) {
  found <- new.env()
  found$cells <- list()
  add <- function(...) found$cells[[length(found$cells) + 1]] <- qc_cell_301(...)
  adsl <- adam$ADSL
  dose_name <- function(d) if (d == 0) "Placebo" else paste(d, "mg")

  # T-14.1.1: disposition of the randomised patients.
  rand <- adsl[adsl$RANDFL == "Y", ]
  groups <- c(split(seq_len(nrow(rand)), rand$TRT01P), Total = list(seq_len(nrow(rand))))
  for (g in names(groups)) {
    d <- rand[groups[[g]], ]
    n <- nrow(d)
    add("T-14.1.1", "Randomised", "Randomised", g, qc_npct_301(n, n))
    add("T-14.1.1", "Treated (safety set)", "Treated (safety set)", g, qc_npct_301(sum(d$SAFFL %in% "Y"), n))
    add("T-14.1.1", "Completed treatment", "Completed treatment", g, qc_npct_301(sum(d$EOTSTT %in% "COMPLETED"), n))
    add("T-14.1.1", "Discontinued study", "Discontinued study", g, qc_npct_301(sum(d$EOSSTT %in% "DISCONTINUED"), n))
  }

  # T-14.1.2: age, sex and baseline HbA1c of the full analysis set.
  fas <- adsl[adsl$FASFL == "Y", ]
  groups <- c(split(seq_len(nrow(fas)), fas$TRT01P), Total = list(seq_len(nrow(fas))))
  for (g in names(groups)) {
    d <- fas[groups[[g]], ]
    add("T-14.1.2", "Age (years)", "n", g, nrow(d))
    add("T-14.1.2", "Age (years)", "Mean (SD)", g, qc_mean_sd_301(d$AGE, 0))
    add("T-14.1.2", "Sex, n (%)", "Female", g, qc_npct_301(sum(d$SEX %in% "F"), nrow(d)))
    add("T-14.1.2", "HbA1c at baseline (%)", "Mean (SD)", g, qc_mean_sd_301(d$HBA1CBL, 1))
  }

  # T-14.2.1: observed HbA1c (change on treatment and before rescue) and the MMRM estimates.
  visit <- sprintf("Week %d", analysis$protocol$primary_visit_week)
  change_section <- sprintf("Change from baseline to %s, on treatment and before rescue", tolower(visit))
  model_section <- sprintf("MMRM at %s", tolower(visit))
  e <- adam$ADEFF
  e <- e[e$FASFL == "Y" & e$PARAMCD == "HBA1C" & e$ANL01FL %in% "Y" & is.na(e$DTYPE) & !is.na(e$AVAL), ]
  m <- analysis$mcpmod$means
  placebo <- which(m$doses == 0)
  arm_dose <- tapply(fas$TRT01PN, fas$TRT01P, unique)
  for (a in names(arm_dose)) {
    base <- e$AVAL[e$AVISIT == "Baseline" & e$TRTP == a]
    chg <- e$CHG[e$AVISIT == visit & e$TRTP == a & e$ONTRTFL %in% "Y" & e$PRERESFL %in% "Y"]
    add("T-14.2.1", "Baseline", "n", a, length(base))
    add("T-14.2.1", "Baseline", "Mean (SD)", a, qc_mean_sd_301(base, 1))
    add("T-14.2.1", change_section, "n", a, length(chg))
    add("T-14.2.1", change_section, "Mean (SD)", a, qc_mean_sd_301(chg, 1))
    i <- which(m$doses == arm_dose[[a]])
    mean_se <- sprintf("%s (%s)", fmt_num(m$means[i], 2), fmt_num(sqrt(m$S[i, i]), 3))
    add("T-14.2.1", model_section, "Mean change (SE)", a, mean_se)
    if (i != placebo) {
      est <- m$means[i] - m$means[placebo]
      se <- sqrt(m$S[i, i] - 2 * m$S[i, placebo] + m$S[placebo, placebo])
      add(
        "T-14.2.1", model_section, "Difference from placebo (95% CI)", a,
        sprintf("%s (%s, %s)", fmt_num(est, 2), fmt_num(est - 1.959964 * se, 2), fmt_num(est + 1.959964 * se, 2))
      )
    }
  }

  # T-14.2.2: contrast tests of A1 and S2, and the target doses.
  names_of <- c(linear = "Linear", emax = "Emax", sigEmax = "Sigmoid Emax", quadratic = "Quadratic")
  section <- "MCP step: contrast test, t (adjusted one-sided p)"
  for (k in c(A1 = "a1", S2 = "s2")) {
    res <- analysis$mcpmod[[k]]
    column <- names(which(c(A1 = "a1", S2 = "s2") == k))
    for (j in seq_len(nrow(res$mct))) {
      p <- res$mct$adjusted_p[j]
      add(
        "T-14.2.2", section, names_of[[res$mct$model[j]]], column,
        sprintf("%s (%s)", fmt_num(res$mct$t_stat[j], 2), if (p < 0.0001) "<0.0001" else fmt_p(p, 4))
      )
    }
  }
  for (k in c(A1 = "a1", S1 = "s1", S2 = "s2")) {
    res <- analysis$mcpmod[[k]]
    column <- names(which(c(A1 = "a1", S1 = "s1", S2 = "s2") == k))
    target <- if (is.na(res$target_dose)) "None" else dose_name(res$target_dose)
    add("T-14.2.2", "Target dose", "Target dose", column, target)
  }

  # T-14.2.3: clinical utility index, recomputed (weights 0.7 and 0.3 from the SAP text of X1).
  a1 <- analysis$mcpmod$a1
  effect <- if (is.null(a1$effects)) m$means - m$means[placebo] else a1$effects
  saf <- adsl[adsl$SAFFL == "Y", ]
  ae <- adam$ADAE
  stopped <- unique(ae$USUBJID[ae$SAFFL == "Y" & ae$TRTEMFL == "Y" & ae$AEACN == "DRUG WITHDRAWN" & !is.na(ae$AEACN)])
  n <- sapply(m$doses, function(d) sum(saf$TRT01AN == d))
  n_disc <- sapply(m$doses, function(d) sum(saf$TRT01AN == d & saf$USUBJID %in% stopped))
  rate <- n_disc / n
  benefit <- -effect
  to_unit <- function(x) if (diff(range(x)) == 0) rep(1, length(x)) else (x - min(x)) / diff(range(x))
  u_effect <- to_unit(benefit)
  u_safety <- to_unit(-rate)
  cui <- 0.7 * u_effect + 0.3 * u_safety
  for (i in seq_along(m$doses)) {
    row <- dose_name(m$doses[i])
    add("T-14.2.3", row, row, "AE leading to", sprintf("%d/%d (%s%%)", n_disc[i], n[i], fmt_num(100 * rate[i], 1)))
    add("T-14.2.3", row, row, "Effect", fmt_num(u_effect[i], 2))
    add("T-14.2.3", row, row, "Safety", fmt_num(u_safety[i], 2))
    add("T-14.2.3", row, row, "Clinical", fmt_num(cui[i], 2))
  }

  # T-14.3.1 and T-14.3.6: adverse events and exposure of the safety set.
  n_arm <- table(saf$TRT01A)
  teae <- ae[ae$SAFFL == "Y" & ae$TRTEMFL %in% "Y", ]
  pts <- function(d, a) length(unique(d$USUBJID[d$TRTA == a]))
  ex <- adam$ADEX[adam$ADEX$SAFFL == "Y", ]
  for (a in names(n_arm)) {
    add("T-14.3.1", "Any TEAE", "Any TEAE", a, qc_npct_301(pts(teae, a), n_arm[[a]]))
    serious <- qc_npct_301(pts(teae[teae$AESER == "Y", ], a), n_arm[[a]])
    add("T-14.3.1", "Any serious TEAE", "Any serious TEAE", a, serious)
    add(
      "T-14.3.1", "TEAE leading to discontinuation of study drug", "TEAE leading to discontinuation of study drug", a,
      qc_npct_301(pts(teae[teae$AEACN %in% "DRUG WITHDRAWN", ], a), n_arm[[a]])
    )
    add("T-14.3.6", "Duration of treatment (days)", "Mean (SD)", a, qc_mean_sd_301(ex$TRTDURD[ex$TRTA == a], 0))
  }
  do.call(rbind, found$cells)
}
