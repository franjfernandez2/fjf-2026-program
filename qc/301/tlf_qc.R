# Second programming of the tables of FJF-2026-301 (independent of studies/301/tlf/).
#
# The cells that come from counting and summarising ADaM records are recomputed here with base
# R from the ADaM datasets, and the cells that come from a model are reformatted from the
# analysis results; each must appear, as printed, in the production table at the same section,
# row and column. Only the number formats (R/tlf.R) are shared, as the shells fix them.

#' A production table in long form: one row per cell, with its section and column header.
#'
#' @param table A table of [tables_301()].
#' @return A data frame: section, label, column (first header line), value.
long_table_qc_301 <- function(table) {
  rows <- table$rows
  section <- character(nrow(rows))
  current <- ""
  parent <- ""
  for (i in seq_len(nrow(rows))) {
    if (rows$indent[i] == 0) current <- rows$label[i]
    if (rows$indent[i] == 1) parent <- rows$label[i]
    # Rows two levels down are placed under their heading and its parent heading.
    section[i] <- if (rows$indent[i] >= 2) paste(current, parent, sep = " / ") else current
  }
  headers <- vapply(strsplit(table$columns, "|", fixed = TRUE), function(h) h[nzchar(h)][1], "")
  # Each column is found by its first header line and by its key (for columns that share a header).
  out <- lapply(names(table$columns), function(k) {
    rbind(
      data.frame(section = section, label = rows$label, column = headers[[k]], key = k, value = rows[[k]]),
      data.frame(section = section, label = rows$label, column = k, key = k, value = rows[[k]])
    )
  })
  unique(do.call(rbind, out))
}

#' An expected cell.
qc_cell_301 <- function(id, section, label, column, value) {
  data.frame(id = id, section = section, label = label, column = column, value = as.character(value))
}

#' "n (x.x%)" as the shells print it.
qc_npct_301 <- function(n, d) if (n == 0) "0" else fmt_n_pct(as.integer(n), d, digits = 1)

#' "mean (sd)": empty without values, "-" for the SD of a single value.
qc_mean_sd_301 <- function(x, digits) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return("")
  }
  sprintf("%s (%s)", fmt_num(mean(x), digits + 1), if (length(x) == 1) "-" else fmt_num(stats::sd(x), digits + 2))
}

#' Expected cells of the tables.
#'
#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results, as for [tables_301()].
#' @return A data frame of expected cells (id, section, label, column, value).
qc_tlf_301 <- function(adam, analysis) {
  found <- new.env()
  found$cells <- list()
  add <- function(...) found$cells[[length(found$cells) + 1]] <- qc_cell_301(...)
  adsl <- adam$ADSL

  # T-14.1.1: counts of the randomised patients.
  rand <- adsl[adsl$RANDFL == "Y", ]
  groups <- c(split(seq_len(nrow(rand)), rand$TRT01P), Total = list(seq_len(nrow(rand))))
  for (g in names(groups)) {
    d <- rand[groups[[g]], ]
    n <- nrow(d)
    add("T-14.1.1", "Randomised", "Randomised", g, qc_npct_301(n, n))
    add("T-14.1.1", "Treated (safety set)", "Treated (safety set)", g, qc_npct_301(sum(d$SAFFL %in% "Y"), n))
    add("T-14.1.1", "Per-protocol set", "Per-protocol set", g, qc_npct_301(sum(d$PPROTFL %in% "Y"), n))
    add("T-14.1.1", "Completed treatment", "Completed treatment", g, qc_npct_301(sum(d$EOTSTT %in% "COMPLETED"), n))
    add("T-14.1.1", "Discontinued study", "Discontinued study", g, qc_npct_301(sum(d$EOSSTT %in% "DISCONTINUED"), n))
    reasons <- table(d$DCTREAS)
    for (r in names(reasons)) {
      add("T-14.1.1", "Discontinued treatment", tools::toTitleCase(tolower(r)), g, qc_npct_301(reasons[[r]], n))
    }
  }

  # T-14.1.2: age, sex and baseline HbA1c of the full analysis set.
  fas <- adsl[adsl$FASFL == "Y", ]
  groups <- c(split(seq_len(nrow(fas)), fas$TRT01P), Total = list(seq_len(nrow(fas))))
  for (g in names(groups)) {
    d <- fas[groups[[g]], ]
    add("T-14.1.2", "Age (years)", "n", g, nrow(d))
    add("T-14.1.2", "Age (years)", "Mean (SD)", g, qc_mean_sd_301(d$AGE, 0))
    add("T-14.1.2", "Age (years)", "Min, max", g, sprintf("%d, %d", min(d$AGE), max(d$AGE)))
    add("T-14.1.2", "Sex, n (%)", "Female", g, qc_npct_301(sum(d$SEX %in% "F"), nrow(d)))
    add("T-14.1.2", "HbA1c at baseline (%)", "Mean (SD)", g, qc_mean_sd_301(d$HBA1CBL, 1))
  }

  # T-14.2.1 and T-14.2.5: observed values and the analyses' estimates.
  visit <- sprintf("Week %d", analysis$protocol$primary_visit_week)
  arm_of <- stats::setNames(fas$TRT01P, fas$ARMCD)
  for (spec in list(
    list(id = "T-14.2.1", param = "HBA1C", analysis = "A1", result = analysis$efficacy$results),
    list(id = "T-14.2.5", param = "WEIGHT", analysis = "B2", result = analysis$secondary$results)
  )) {
    e <- adam$ADEFF
    e <- e[e$FASFL == "Y" & e$PARAMCD == spec$param & e$ANL01FL %in% "Y" & is.na(e$DTYPE) & !is.na(e$AVAL), ]
    for (a in unique(fas$TRT01P)) {
      base <- e$AVAL[e$AVISIT == "Baseline" & e$TRTP == a]
      chg <- e$CHG[e$AVISIT == visit & e$TRTP == a]
      add(spec$id, "Baseline", "n", a, length(base))
      add(spec$id, "Baseline", "Mean (SD)", a, qc_mean_sd_301(base, 1))
      add(spec$id, sprintf("Change from baseline to %s", tolower(visit)), "Mean (SD)", a, qc_mean_sd_301(chg, 1))
    }
    means <- analysis$lsmeans[analysis$lsmeans$analysis == spec$analysis, ]
    for (i in seq_len(nrow(means))) {
      add(
        spec$id, sprintf("Analysis at %s", tolower(visit)), "LS mean change (SE)", arm_of[[means$ARMCD[i]]],
        sprintf("%s (%s)", fmt_num(means$lsmean[i], 2), fmt_num(means$se[i], 3))
      )
    }
    r <- spec$result[spec$result$analysis == spec$analysis, ]
    active <- unique(fas$TRT01P[fas$ARMCD != sap_item(analysis$sap, "A1")$settings$reference])
    add(
      spec$id, sprintf("Analysis at %s", tolower(visit)), "Difference from placebo (95% CI)", active,
      sprintf("%s (%s, %s)", fmt_num(r$estimate, 2), fmt_num(r$lower, 2), fmt_num(r$upper, 2))
    )
  }

  # T-14.3.1, T-14.3.2: treatment-emergent adverse events of the safety set.
  saf <- adsl[adsl$SAFFL == "Y", ]
  n_arm <- table(saf$TRT01A)
  ae <- adam$ADAE[adam$ADAE$SAFFL == "Y" & adam$ADAE$TRTEMFL %in% "Y", ]
  pts <- function(d, a) length(unique(d$USUBJID[d$TRTA == a]))
  for (a in names(n_arm)) {
    add("T-14.3.1", "Any TEAE", "Any TEAE", a, qc_npct_301(pts(ae, a), n_arm[[a]]))
    add("T-14.3.1", "Any serious TEAE", "Any serious TEAE", a, qc_npct_301(pts(ae[ae$AESER == "Y", ], a), n_arm[[a]]))
    add(
      "T-14.3.1", "TEAE leading to discontinuation of study drug", "TEAE leading to discontinuation of study drug", a,
      qc_npct_301(pts(ae[ae$AEACN == "DRUG WITHDRAWN", ], a), n_arm[[a]])
    )
    add("T-14.3.1", "Any TEAE", "Severe", a, qc_npct_301(pts(ae[ae$ASEV == "SEVERE", ], a), n_arm[[a]]))
    for (soc in unique(ae$AEBODSYS)) {
      in_soc <- ae[ae$AEBODSYS == soc, ]
      section <- tools::toTitleCase(tolower(soc))
      add("T-14.3.2", section, section, a, qc_npct_301(pts(in_soc, a), n_arm[[a]]))
      for (pt in unique(in_soc$AEDECOD)) {
        add("T-14.3.2", section, pt, a, qc_npct_301(pts(in_soc[in_soc$AEDECOD == pt, ], a), n_arm[[a]]))
      }
    }
  }

  # T-14.3.3: hypoglycaemia on treatment.
  hypo <- adam$ADHYPO[adam$ADHYPO$SAFFL == "Y" & adam$ADHYPO$ONTRTFL %in% "Y", ]
  levels <- list(
    "Any level" = 1:3, "Level 1 (glucose <3.9 and >=3.0 mmol/L)" = 1, "Level 2 (glucose <3.0 mmol/L)" = 2,
    "Level 3 (severe, needing assistance)" = 3, "Level 2 or 3" = 2:3
  )
  for (a in names(n_arm)) {
    for (l in names(levels)) {
      d <- hypo[as.integer(hypo$HYPOLVL) %in% levels[[l]], ]
      add("T-14.3.3", l, l, a, qc_npct_301(pts(d, a), n_arm[[a]]))
    }
  }

  # T-14.3.6: exposure.
  ex <- adam$ADEX[adam$ADEX$SAFFL == "Y", ]
  for (a in names(n_arm)) {
    d <- ex[ex$TRTA == a, ]
    add("T-14.3.6", "Duration of treatment (days)", "Mean (SD)", a, qc_mean_sd_301(d$TRTDURD, 0))
    add("T-14.3.6", "Total exposure (patient-years)", "Total exposure (patient-years)", a, fmt_num(sum(d$TRTDURY), 1))
  }

  # T-14.2.7: the multiplicity decisions.
  m <- analysis$multiplicity
  for (i in seq_len(nrow(m))) {
    label <- sprintf("%s %s", m$hypothesis[i], m$label[i])
    add("T-14.2.7", label, label, "Decision", if (m$rejected[i]) "Rejected" else "Not rejected")
  }
  do.call(rbind, found$cells)
}

#' Compare the production tables with the second programming.
#'
#' @param tables Output of [tables_301()].
#' @param expected Output of [qc_tlf_301()].
#' @return A list: `cells` (each expected cell with the production value and whether it
#'   matches) and `clean` (TRUE when every expected cell is found as printed).
compare_tlf_301 <- function(tables, expected) {
  long <- do.call(rbind, lapply(names(tables), function(id) cbind(id = id, long_table_qc_301(tables[[id]]))))
  found <- merge(expected, long[, c("id", "section", "label", "column", "value")],
    by = c("id", "section", "label", "column"), all.x = TRUE, suffixes = c("", "_production")
  )
  found$match <- !is.na(found$value_production) & found$value == found$value_production
  list(cells = found, clean = nrow(found) > 0 && all(found$match))
}
