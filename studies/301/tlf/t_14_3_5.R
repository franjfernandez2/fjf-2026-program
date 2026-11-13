# T-14.3.5 Vital signs and body weight: values and change from baseline by visit, and patients
# outside the predefined limits (SAP SF4, ADR 0028). Source: ADVS (ANL01FL, CRIT1FL, CRIT2FL),
# safety set. ECG is collected at screening and week 26 but not transferred to the study
# database, so it has no rows here (see the ADRG).

#' @inheritParams t_14_2_1_301
t_14_3_5_301 <- function(adam, analysis, outputs) {
  s <- safety_set_301(adam)
  vs <- adam$ADVS[adam$ADVS$SAFFL == "Y" & !is.na(adam$ADVS$AVAL), ]
  vs <- vs[vs$ABLFL %in% "Y" | (vs$ANL01FL %in% "Y" & !is.na(vs$AVISIT)), ]
  vs$AVISIT[vs$ABLFL %in% "Y"] <- "Baseline"
  vs$AVISITN[vs$ABLFL %in% "Y"] <- -1L
  params <- unique(vs[order(vs$PARAMCD), c("PARAMCD", "PARAM")])
  digits <- c(SYSBP = 0, DIABP = 0, PULSE = 0, WEIGHT = 1)
  mean_sd <- function(x, d) {
    if (length(x) == 0) {
      return("")
    }
    sprintf("%s (%s)", fmt_num(mean(x), d + 1), if (length(x) > 1) fmt_num(stats::sd(x), d + 2) else "-")
  }
  rows <- list()
  for (p in seq_len(nrow(params))) {
    code <- params$PARAMCD[p]
    d_p <- vs[vs$PARAMCD == code, ]
    rows[[length(rows) + 1]] <- heading_301(params$PARAM[p])
    visits <- unique(d_p[order(d_p$AVISITN), c("AVISITN", "AVISIT")])
    for (v in visits$AVISIT) {
      cells <- list()
      for (i in seq_len(nrow(s$arms))) {
        d <- d_p[d_p$AVISIT == v & d_p$TRTA == s$arms$label[i], ]
        k <- s$arms$key[i]
        cells[[paste0(k, "_n")]] <- as.character(nrow(d))
        cells[[paste0(k, "_val")]] <- mean_sd(d$AVAL, digits[[code]])
        cells[[paste0(k, "_chg")]] <- if (v == "Baseline") "" else mean_sd(d$CHG[!is.na(d$CHG)], digits[[code]])
      }
      rows[[length(rows) + 1]] <- table_rows_301(v, cells, 1L)
    }
  }
  # Patients with a post-baseline value outside the limits, among those with a post-baseline value.
  all_vs <- adam$ADVS[adam$ADVS$SAFFL == "Y" & !is.na(adam$ADVS$CHG) & !(adam$ADVS$ABLFL %in% "Y"), ]
  rows[[length(rows) + 1]] <- heading_301("Patients outside the predefined limits after baseline, n/N (%)")
  for (p in seq_len(nrow(params))) {
    code <- params$PARAMCD[p]
    d_p <- all_vs[all_vs$PARAMCD == code, ]
    for (crit in c("CRIT1", "CRIT2")) {
      label <- unique(stats::na.omit(d_p[[crit]]))
      if (length(label) == 0) next
      cells <- list()
      for (i in seq_len(nrow(s$arms))) {
        d <- d_p[d_p$TRTA == s$arms$label[i], ]
        n <- length(unique(d$USUBJID[d[[paste0(crit, "FL")]] %in% "Y"]))
        tot <- length(unique(d$USUBJID))
        pct <- fmt_num(if (tot > 0) 100 * n / tot else 0, 1)
        cells[[paste0(s$arms$key[i], "_val")]] <- sprintf("%d/%d (%s%%)", n, tot, pct)
      }
      shown <- if (startsWith(label[1], code)) label[1] else sprintf("%s %s", code, tolower(label[1]))
      rows[[length(rows) + 1]] <- table_rows_301(shown, cells, 1L)
    }
  }
  columns <- unlist(lapply(seq_len(nrow(s$arms)), function(i) {
    stats::setNames(
      c("|n", sprintf("%s (N=%d)|Value, mean (SD)", s$arms$label[i], s$arms$n[i]), "|Change, mean (SD)"),
      paste0(s$arms$key[i], c("_n", "_val", "_chg"))
    )
  }))
  tlf_table_301("T-14.3.5", outputs, columns, do.call(bind_rows_301, rows),
    footnotes = c(
      "Baseline: last value before the first dose (ADVS.ABLFL); then values by analysis visit (ADVS.ANL01FL).",
      "Limits: ADR 0028 item 1 (ADaM specification, vital_sign_limits).",
      "ECG is not in the study database, so it is not tabulated (see the ADRG). SD: standard deviation."
    )
  )
}
