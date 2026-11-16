# Second programming of the tables that qc/301/tlf_qc.R and qc/201/tlf_qc.R did not cover
# (independent of studies/301/tlf/ and studies/201/tlf/).
#
# Counts, summaries, shift tables and Kaplan-Meier proportions are recomputed with base R from
# the ADaM datasets; model results are reformatted from the analysis results. Each expected cell
# must appear, as printed, in the production table at the same section, row and column
# (compare_tlf_301()). Columns are named by the first header line (the arm) or, when several
# columns share it, by their column key; the keys follow the arm order of the tables (highest
# dose first). Only the number formats (R/tlf.R) and the cell helpers of qc/301/tlf_qc.R are shared.

#' Column keys of the arms of an analysis set, in the order of the tables (highest dose first).
#'
#' @param adsl ADSL rows of the analysis set.
#' @param var "TRT01P" or "TRT01A".
#' @return A named character vector: arm label -> key ("arm1", "arm2", ...).
qc_arm_keys <- function(adsl, var) {
  labels <- unique(adsl[[var]][order(-adsl[[paste0(var, "N")]])])
  stats::setNames(paste0("arm", seq_along(labels)), labels)
}

#' Accumulate expected cells.
qc_collector <- function() {
  env <- new.env()
  env$cells <- list()
  list(
    add = function(...) env$cells[[length(env$cells) + 1]] <- qc_cell_301(...),
    get = function() do.call(rbind, env$cells)
  )
}

#' A p-value as printed: four decimals, "<0.0001" below that.
qc_p <- function(p) if (p < 0.0001) "<0.0001" else fmt_p(p, 4)

#' Product-limit proportion with an event by a day.
#'
#' @param time Times.
#' @param event 1 for an event, 0 for a censored time.
#' @param day Day.
qc_km_event_by <- function(time, event, day) {
  surv <- 1
  for (t in sort(unique(time[event == 1 & time <= day]))) {
    surv <- surv * (1 - sum(time == t & event == 1) / sum(time >= t))
  }
  1 - surv
}

#' Expected cells of the adverse event and hypoglycaemia tables (T-14.3.2, T-14.3.3).
#'
#' Patients as n (%), events or episodes and their rate per 100 patient-years, by arm.
#'
#' @param adam Named list of the ADaM datasets.
#' @return A data frame of expected cells.
qc_ae_hypo_cells <- function(adam) {
  out <- qc_collector()
  saf <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  keys <- qc_arm_keys(saf, "TRT01A")
  n_arm <- table(saf$TRT01A)
  ex <- adam$ADEX[adam$ADEX$SAFFL == "Y", ]
  years <- vapply(names(keys), function(a) sum(ex$TRTDURY[ex$TRTA == a], na.rm = TRUE), 0)
  patients <- function(d, a) length(unique(d$USUBJID[d$TRTA == a]))
  rate <- function(d, a) fmt_num(100 * sum(d$TRTA == a) / years[[a]], 1)

  ae <- adam$ADAE[adam$ADAE$SAFFL == "Y" & adam$ADAE$TRTEMFL %in% "Y", ]
  for (a in names(keys)) {
    out$add("T-14.3.2", "Any TEAE", "Any TEAE", a, qc_npct_301(patients(ae, a), n_arm[[a]]))
    out$add("T-14.3.2", "Any TEAE", "Any TEAE", paste0(keys[[a]], "_rate"), rate(ae, a))
    for (soc in unique(ae$AEBODSYS)) {
      in_soc <- ae[ae$AEBODSYS == soc, ]
      section <- tools::toTitleCase(tolower(soc))
      out$add("T-14.3.2", section, section, a, qc_npct_301(patients(in_soc, a), n_arm[[a]]))
      out$add("T-14.3.2", section, section, paste0(keys[[a]], "_rate"), rate(in_soc, a))
      for (pt in unique(in_soc$AEDECOD)) {
        in_pt <- in_soc[in_soc$AEDECOD == pt, ]
        out$add("T-14.3.2", section, pt, a, qc_npct_301(patients(in_pt, a), n_arm[[a]]))
        out$add("T-14.3.2", section, pt, paste0(keys[[a]], "_rate"), rate(in_pt, a))
      }
    }
  }

  hypo <- adam$ADHYPO[adam$ADHYPO$SAFFL == "Y" & adam$ADHYPO$ONTRTFL %in% "Y", ]
  levels <- list(
    "Any level" = 1:3, "Level 1 (glucose <3.9 and >=3.0 mmol/L)" = 1, "Level 2 (glucose <3.0 mmol/L)" = 2,
    "Level 3 (severe, needing assistance)" = 3, "Level 2 or 3" = 2:3
  )
  for (a in names(keys)) {
    for (l in names(levels)) {
      d <- hypo[as.integer(hypo$HYPOLVL) %in% levels[[l]], ]
      out$add("T-14.3.3", l, l, a, qc_npct_301(patients(d, a), n_arm[[a]]))
      out$add("T-14.3.3", l, l, paste0(keys[[a]], "_e"), as.character(sum(d$TRTA == a)))
      out$add("T-14.3.3", l, l, paste0(keys[[a]], "_rate"), rate(d, a))
    }
  }
  out$get()
}

#' Expected cells of the laboratory shift table and the vital signs table (T-14.3.4, T-14.3.5).
#'
#' @param adam Named list of the ADaM datasets.
#' @return A data frame of expected cells.
qc_lab_vitals_cells <- function(adam) {
  out <- qc_collector()
  saf <- adam$ADSL[adam$ADSL$SAFFL == "Y", ]
  keys <- qc_arm_keys(saf, "TRT01A")

  # T-14.3.4: baseline range against the range of the worst value on treatment, HbA1c excluded.
  lb <- adam$ADLB[adam$ADLB$SAFFL == "Y" & adam$ADLB$WORSTFL %in% "Y" & adam$ADLB$PARAMCD != "HBA1C", ]
  for (code in sort(unique(lb$PARAMCD))) {
    param <- unique(lb$PARAM[lb$PARAMCD == code])[1]
    for (a in names(keys)) {
      d <- lb[lb$PARAMCD == code & lb$TRTA == a, ]
      section <- sprintf("%s / %s (n=%d)", param, a, nrow(d))
      base <- ifelse(is.na(d$BNRIND), "MISSING", d$BNRIND)
      for (b in c("LOW", "NORMAL", "HIGH", if (any(base == "MISSING")) "MISSING")) {
        label <- paste("Baseline", tolower(b))
        for (w in c("LOW", "NORMAL", "HIGH")) {
          out$add("T-14.3.4", section, label, tolower(w), as.character(sum(base == b & d$ANRIND %in% w)))
        }
        out$add("T-14.3.4", section, label, "total", as.character(sum(base == b)))
      }
    }
  }

  # T-14.3.5: values and change by visit, and patients outside the limits after baseline.
  vs_all <- adam$ADVS[adam$ADVS$SAFFL == "Y", ]
  vs <- vs_all[!is.na(vs_all$AVAL) & (vs_all$ABLFL %in% "Y" | (vs_all$ANL01FL %in% "Y" & !is.na(vs_all$AVISIT))), ]
  vs$AVISIT[vs$ABLFL %in% "Y"] <- "Baseline"
  digits <- c(SYSBP = 0, DIABP = 0, PULSE = 0, WEIGHT = 1)
  mean_sd <- function(x, d) {
    if (length(x) == 0) {
      return("")
    }
    sprintf("%s (%s)", fmt_num(mean(x), d + 1), if (length(x) > 1) fmt_num(stats::sd(x), d + 2) else "-")
  }
  post <- vs_all[!is.na(vs_all$CHG) & !(vs_all$ABLFL %in% "Y"), ]
  limits <- "Patients outside the predefined limits after baseline, n/N (%)"
  for (code in sort(unique(vs$PARAMCD))) {
    param <- unique(vs$PARAM[vs$PARAMCD == code])[1]
    for (v in unique(vs$AVISIT[vs$PARAMCD == code])) {
      for (a in names(keys)) {
        d <- vs[vs$PARAMCD == code & vs$AVISIT == v & vs$TRTA == a, ]
        out$add("T-14.3.5", param, v, paste0(keys[[a]], "_n"), as.character(nrow(d)))
        out$add("T-14.3.5", param, v, paste0(keys[[a]], "_val"), mean_sd(d$AVAL, digits[[code]]))
        chg <- if (v == "Baseline") "" else mean_sd(d$CHG[!is.na(d$CHG)], digits[[code]])
        out$add("T-14.3.5", param, v, paste0(keys[[a]], "_chg"), chg)
      }
    }
    for (crit in c("CRIT1", "CRIT2")) {
      p <- post[post$PARAMCD == code, ]
      text <- unique(stats::na.omit(p[[crit]]))
      if (length(text) == 0) next
      shown <- if (startsWith(text[1], code)) text[1] else sprintf("%s %s", code, tolower(text[1]))
      for (a in names(keys)) {
        d <- p[p$TRTA == a, ]
        n <- length(unique(d$USUBJID[d[[paste0(crit, "FL")]] %in% "Y"]))
        tot <- length(unique(d$USUBJID))
        pct <- fmt_num(if (tot > 0) 100 * n / tot else 0, 1)
        out$add("T-14.3.5", limits, shown, paste0(keys[[a]], "_val"), sprintf("%d/%d (%s%%)", n, tot, pct))
      }
    }
  }
  out$get()
}

#' Expected cells of the efficacy tables of FJF-2026-301 not covered before
#' (T-14.2.2, T-14.2.3, T-14.2.4, T-14.2.6).
#'
#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results, as for [tables_301()].
#' @return A data frame of expected cells.
qc_efficacy_cells <- function(adam, analysis) {
  out <- qc_collector()
  fas <- adam$ADSL[adam$ADSL$FASFL == "Y", ]
  arms <- unique(fas$TRT01P)
  active <- unique(fas$TRT01P[fas$TRT01PN == max(fas$TRT01PN)])
  placebo <- unique(fas$TRT01P[fas$TRT01PN == min(fas$TRT01PN)])
  visit <- sprintf("Week %d", analysis$protocol$primary_visit_week)
  results <- analysis$efficacy$results

  # T-14.2.2: observed values on treatment and before rescue, and analysis A2.
  e <- adam$ADEFF
  e <- e[e$FASFL == "Y" & e$PARAMCD == "HBA1C" & e$ANL01FL %in% "Y" & is.na(e$DTYPE) & !is.na(e$AVAL), ]
  e <- e[e$AVISIT == "Baseline" | (e$ONTRTFL %in% "Y" & e$PRERESFL %in% "Y"), ]
  arm_code <- stats::setNames(fas$ARMCD, fas$TRT01P)
  change <- sprintf("Change from baseline to %s", tolower(visit))
  for (a in arms) {
    base <- e[e$AVISIT == "Baseline" & e$TRTP == a, ]
    at <- e[e$AVISIT == visit & e$TRTP == a, ]
    out$add("T-14.2.2", "Baseline", "n", a, nrow(base))
    out$add("T-14.2.2", "Baseline", "Mean (SD)", a, qc_mean_sd_301(base$AVAL, 1))
    out$add("T-14.2.2", visit, "n", a, nrow(at))
    out$add("T-14.2.2", visit, "Mean (SD)", a, qc_mean_sd_301(at$AVAL, 1))
    out$add("T-14.2.2", change, "n", a, sum(!is.na(at$CHG)))
    out$add("T-14.2.2", change, "Mean (SD)", a, qc_mean_sd_301(at$CHG, 1))
    m <- analysis$lsmeans[analysis$lsmeans$analysis == "A2" & analysis$lsmeans$ARMCD == arm_code[[a]], ]
    out$add(
      "T-14.2.2", sprintf("Analysis at %s", tolower(visit)), "LS mean change (SE)", a,
      sprintf("%s (%s)", fmt_num(m$lsmean, 2), fmt_num(m$se, 3))
    )
  }
  r <- results[results$analysis == "A2", ]
  section <- sprintf("Analysis at %s", tolower(visit))
  out$add(
    "T-14.2.2", section, "Difference from placebo (95% CI)", active,
    sprintf("%s (%s, %s)", fmt_num(r$estimate, 2), fmt_num(r$lower, 2), fmt_num(r$upper, 2))
  )
  out$add("T-14.2.2", section, "p-value (two-sided)", active, qc_p(r$p_value))

  # T-14.2.3: sensitivity analyses.
  strategy <- c(JR = "jump to reference", CR = "copy reference", MAR = "missing at random", MMRM = "MMRM")
  rows <- c(A1 = "A1 Primary analysis", S1 = "S1 Missing at random", S2 = "S2 Copy reference")
  for (id in names(rows)) {
    r <- results[results$analysis == id, ]
    out$add("T-14.2.3", rows[[id]], rows[[id]], "strategy", strategy[[r$strategy]])
    out$add(
      "T-14.2.3", rows[[id]], rows[[id]], "diff",
      sprintf("%s (%s, %s)", fmt_num(r$estimate, 2), fmt_num(r$lower, 2), fmt_num(r$upper, 2))
    )
    out$add("T-14.2.3", rows[[id]], rows[[id]], "p", qc_p(r$p_value))
  }
  tip <- analysis$efficacy$tipping
  tp <- analysis$efficacy$tipping_point
  text <- if (is.na(tp)) {
    sprintf("Not reached up to %s", fmt_num(max(tip$delta), 2))
  } else if (tp == min(tip$delta)) {
    "Not applicable: the 95% CI includes 0 without shift"
  } else {
    fmt_num(tp, 2)
  }
  out$add("T-14.2.3", "S3 Tipping point (HbA1c %)", "S3 Tipping point (HbA1c %)", "diff", text)

  # T-14.2.4: patients with HbA1c below the threshold at the primary visit, and analysis B1.
  d <- adam$ADEFF
  d <- d[d$FASFL == "Y" & d$PARAMCD == "HBA1C" & d$AVISIT == visit & d$ANL01FL %in% "Y", ]
  d <- d[is.na(d$DTYPE) & !is.na(d$AVAL), ]
  label <- sprintf("Observed at %s, n/N (%%)", tolower(visit))
  for (a in arms) {
    yes <- d$CRIT1FL[d$TRTP == a] %in% "Y"
    out$add("T-14.2.4", label, label, a, sprintf("%d/%d (%s%%)", sum(yes), length(yes), fmt_num(100 * mean(yes), 1)))
  }
  r <- analysis$secondary$results[analysis$secondary$results$analysis == "B1", ]
  out$add("T-14.2.4", "Analysis B1", "Estimated proportion (%)", active, fmt_num(100 * r$proportion_active, 1))
  out$add("T-14.2.4", "Analysis B1", "Estimated proportion (%)", placebo, fmt_num(100 * r$proportion_reference, 1))
  out$add(
    "T-14.2.4", "Analysis B1", "Odds ratio (95% CI)", active,
    sprintf("%s (%s, %s)", fmt_num(r$estimate, 2), fmt_num(r$lower, 2), fmt_num(r$upper, 2))
  )
  out$add(
    "T-14.2.4", "Analysis B1", "Difference in proportions, % (95% CI)", active,
    sprintf(
      "%s (%s, %s)", fmt_num(100 * r$difference, 1), fmt_num(100 * r$difference_lower, 1),
      fmt_num(100 * r$difference_upper, 1)
    )
  )
  out$add("T-14.2.4", "Analysis B1", "p-value (two-sided)", active, qc_p(r$p_value))

  # T-14.2.6: time to first rescue medication, and analysis B3.
  tt <- adam$ADTTE[adam$ADTTE$PARAMCD == "TTRESC" & adam$ADTTE$FASFL == "Y", ]
  day <- 7 * analysis$protocol$primary_visit_week
  for (a in arms) {
    n <- sum(fas$TRT01P == a)
    t <- tt[tt$TRTP == a, ]
    rescue <- "Patients with rescue medication, n (%)"
    out$add("T-14.2.6", rescue, rescue, a, qc_npct_301(sum(t$CNSR == 0), n))
    out$add("T-14.2.6", "Censored, n (%)", "Censored, n (%)", a, qc_npct_301(sum(t$CNSR == 1), n))
    km_label <- sprintf("Kaplan-Meier proportion with rescue by day %d (%%)", day)
    out$add("T-14.2.6", km_label, km_label, a, fmt_num(100 * qc_km_event_by(t$AVAL, 1 - t$CNSR, day), 1))
  }
  r <- analysis$secondary$results[analysis$secondary$results$analysis == "B3", ]
  out$add(
    "T-14.2.6", "Analysis B3", "Hazard ratio (95% CI)", active,
    sprintf("%s (%s, %s)", fmt_num(r$estimate, 2), fmt_num(r$lower, 2), fmt_num(r$upper, 2))
  )
  out$add("T-14.2.6", "Analysis B3", "p-value (two-sided)", active, qc_p(r$p_value))
  out$get()
}

#' All additional expected cells of the tables of FJF-2026-301.
#'
#' @inheritParams qc_efficacy_cells
qc_tlf_301_more <- function(adam, analysis) {
  rbind(qc_efficacy_cells(adam, analysis), qc_ae_hypo_cells(adam), qc_lab_vitals_cells(adam))
}

#' All additional expected cells of the tables of FJF-2026-201.
#'
#' @inheritParams qc_efficacy_cells
qc_tlf_201_more <- function(adam) {
  rbind(qc_ae_hypo_cells(adam), qc_lab_vitals_cells(adam))
}

#' Expected cells of the demographic, adverse event overview and exposure tables, by arm.
#'
#' The cells of T-14.1.2, T-14.3.1 and T-14.3.6 of qc_tlf_301() that need no analysis result, without
#' the total column, for the pooled review (the studies are its columns).
#'
#' @param adam Named list of the ADaM datasets.
#' @return A data frame of expected cells, with the ids of FJF-2026-301.
qc_basic_cells <- function(adam) {
  out <- qc_collector()
  adsl <- adam$ADSL
  fas <- adsl[adsl$FASFL == "Y", ]
  for (g in unique(fas$TRT01P)) {
    d <- fas[fas$TRT01P == g, ]
    out$add("T-14.1.2", "Age (years)", "n", g, nrow(d))
    out$add("T-14.1.2", "Age (years)", "Mean (SD)", g, qc_mean_sd_301(d$AGE, 0))
    out$add("T-14.1.2", "Age (years)", "Min, max", g, sprintf("%d, %d", min(d$AGE), max(d$AGE)))
    out$add("T-14.1.2", "Sex, n (%)", "Female", g, qc_npct_301(sum(d$SEX %in% "F"), nrow(d)))
    out$add("T-14.1.2", "HbA1c at baseline (%)", "Mean (SD)", g, qc_mean_sd_301(d$HBA1CBL, 1))
  }
  saf <- adsl[adsl$SAFFL == "Y", ]
  n_arm <- table(saf$TRT01A)
  ae <- adam$ADAE[adam$ADAE$SAFFL == "Y" & adam$ADAE$TRTEMFL %in% "Y", ]
  ex <- adam$ADEX[adam$ADEX$SAFFL == "Y", ]
  pts <- function(d, a) length(unique(d$USUBJID[d$TRTA == a]))
  stopped <- "TEAE leading to discontinuation of study drug"
  for (a in names(n_arm)) {
    out$add("T-14.3.1", "Any TEAE", "Any TEAE", a, qc_npct_301(pts(ae, a), n_arm[[a]]))
    serious <- qc_npct_301(pts(ae[ae$AESER == "Y", ], a), n_arm[[a]])
    out$add("T-14.3.1", "Any serious TEAE", "Any serious TEAE", a, serious)
    out$add("T-14.3.1", stopped, stopped, a, qc_npct_301(pts(ae[ae$AEACN == "DRUG WITHDRAWN", ], a), n_arm[[a]]))
    out$add("T-14.3.1", "Any TEAE", "Severe", a, qc_npct_301(pts(ae[ae$ASEV == "SEVERE", ], a), n_arm[[a]]))
    d <- ex[ex$TRTA == a, ]
    out$add("T-14.3.6", "Duration of treatment (days)", "Mean (SD)", a, qc_mean_sd_301(d$TRTDURD, 0))
    years <- "Total exposure (patient-years)"
    out$add("T-14.3.6", years, years, a, fmt_num(sum(d$TRTDURY), 1))
  }
  out$get()
}

#' Expected cells of the blinded pooled review tables (T-ISS-1 to T-ISS-7).
#'
#' Each table is the FJF-2026-301 table of its source id on the pooled datasets, so its expected
#' cells are those of that table, recomputed from the pooled ADaM datasets and renumbered.
#'
#' @param adam Output of [pool_iss()].
#' @param outputs The ISS outputs specification (gives each table's source).
#' @return A data frame of expected cells.
qc_tlf_iss <- function(adam, outputs) {
  cells <- rbind(qc_basic_cells(adam), qc_ae_hypo_cells(adam), qc_lab_vitals_cells(adam))
  ids <- stats::setNames(
    vapply(outputs$outputs, function(o) o$id, ""), vapply(outputs$outputs, function(o) o$source, "")
  )
  # The pooled tables have no total column: the pooled column already holds every patient.
  cells <- cells[cells$id %in% names(ids) & cells$column != "total", ]
  cells$id <- unname(ids[cells$id])
  cells
}
