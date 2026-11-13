# T-14.3.1 Overview of treatment-emergent adverse events. Source: ADAE (TRTEMFL), safety set.

#' The safety set and its arms.
#' @param adam Named list of the ADaM datasets.
safety_set_301 <- function(adam) {
  adsl <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  arms <- arms_301(adsl, "TRT01A")
  list(adsl = adsl, arms = arms, denom = stats::setNames(arms$n, arms$key))
}

#' Treatment-emergent adverse events of the safety set.
teae_301 <- function(adam) {
  ae <- adam$ADAE
  ae[ae$SAFFL == "Y" & ae$TRTEMFL %in% "Y", ]
}

#' Patients with at least one record matching a condition, n (%) by arm.
#' @param d Records with USUBJID and TRTA.
#' @param flag Condition on each record.
#' @param s Output of [safety_set_301()].
patients_301 <- function(d, flag, s) {
  hit <- d[flag & !is.na(flag), ]
  stats::setNames(lapply(seq_len(nrow(s$arms)), function(i) {
    npct_301(length(unique(hit$USUBJID[hit$TRTA == s$arms$label[i]])), s$arms$n[i])
  }), s$arms$key)
}

#' @inheritParams t_14_2_1_301
t_14_3_1_301 <- function(adam, analysis, outputs) {
  s <- safety_set_301(adam)
  ae <- teae_301(adam)
  row <- function(label, flag, indent = 0L) table_rows_301(label, patients_301(ae, flag, s), indent)
  rows <- bind_rows_301(
    row("Any TEAE", rep(TRUE, nrow(ae))),
    row("Related to study drug", ae$AEREL == "RELATED", 1L),
    row("Severe", ae$ASEV == "SEVERE", 1L),
    row("Any serious TEAE", ae$AESER == "Y"),
    row("TEAE leading to discontinuation of study drug", ae$AEACN == "DRUG WITHDRAWN"),
    row("TEAE leading to interruption of study drug", ae$AEACN == "DRUG INTERRUPTED"),
    row("Fatal TEAE", ae$AEOUT == "FATAL")
  )
  tlf_table_301("T-14.3.1", outputs, arm_columns_301(s$arms), rows,
    footnotes = c(
      "TEAE: treatment-emergent adverse event (started or worsened on or after the first dose; ADAE.TRTEMFL).",
      "Patients are counted once in each row.",
      "Severity is the analysis severity (ASEV; missing severity counted as severe, CR09)."
    )
  )
}
