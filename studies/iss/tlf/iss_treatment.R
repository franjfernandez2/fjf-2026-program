# Integrated safety analysis of FJF-2026 by treatment: the placebo-controlled pool of
# FJF-2026-201 and FJF-2026-301 (docs/sap/fjf-2026-iss-sap.yml, version 1.0; ADR 0034). The
# outputs are in docs/sap/fjf-2026-iss-treatment-outputs.yml and the choices the plan leaves
# open in ADR 0035.
#
# The ADaM datasets of the two studies are stacked, not derived again. Each patient is in the
# column of the dose actually taken; a patient on FJF-2026 is also in the all-doses column.
# Tables that FJF-2026-301 already has (exposure, hypoglycaemia by level) are made by its
# programs on the pooled datasets; the others are here. Everything is descriptive.

#' Read the plan of the integrated safety analysis.
#' @param path Path to docs/sap/fjf-2026-iss-sap.yml.
read_iss_sap <- function(path) yaml::read_yaml(path)

#' The columns of the placebo-controlled pool, in the order of the plan.
#' @param sap Output of [read_iss_sap()].
#' @return A data frame: id (PBO, FJF5, ..., FJFALL) and label.
iss_columns <- function(sap) {
  cols <- sap$pools[[1]]$columns
  data.frame(id = purrr::map_chr(cols, "id"), label = purrr::map_chr(cols, "label"), stringsAsFactors = FALSE)
}

#' Dose in mg of a dose column (0 for placebo, NA for the all-doses column).
#' @param id Column ids.
iss_column_dose <- function(id) {
  ifelse(id == "PBO", 0, suppressWarnings(as.numeric(sub("^FJF", "", id))))
}

#' Whether a dose belongs to a column.
#' @param id One column id.
#' @param dose Doses in mg.
iss_in_column <- function(id, dose) {
  if (id == "FJFALL") {
    return(!is.na(dose) & dose > 0)
  }
  !is.na(dose) & dose == iss_column_dose(id)
}

#' Pool the safety datasets of the studies by treatment actually taken.
#'
#' @param adam_by_study Named list (by study, e.g. "201") of named lists of ADaM datasets.
#' @param sap Output of [read_iss_sap()].
#' @param datasets Names of the datasets to pool.
#' @return A named list of datasets. The treatment variables hold the label of the column and
#'   their numeric codes order the columns as in the plan; `ISSCOL` is the column id and
#'   `ISSDOSE` the dose taken. A patient on FJF-2026 has two sets of records: the dose and all doses.
pool_iss_treatment <- function(adam_by_study, sap, datasets) {
  cols <- iss_columns(sap)
  studies <- names(adam_by_study)
  subjects <- unlist(lapply(adam_by_study, function(a) a$ADSL$USUBJID))
  if (anyDuplicated(subjects)) stop("a USUBJID is in more than one study")
  stats::setNames(lapply(datasets, function(ds) {
    stacked <- dplyr::bind_rows(lapply(studies, function(study) {
      d <- adam_by_study[[study]][[ds]]
      if (is.null(d)) stop("no ", ds, " for study ", study)
      d$STUDYID <- rep(sprintf("FJF-2026-%s", study), nrow(d))
      d
    }))
    stacked$ISSDOSE <- stacked[[if ("TRT01AN" %in% names(stacked)) "TRT01AN" else "TRTAN"]]
    in_safety <- stacked$SAFFL %in% "Y"
    placed <- !is.na(stacked$ISSDOSE) & stacked$ISSDOSE %in% iss_column_dose(cols$id)
    if (any(in_safety & !placed)) stop(ds, ": a patient of the safety set takes a dose with no column in the plan")
    parts <- lapply(seq_len(nrow(cols)), function(i) {
      d <- stacked[iss_in_column(cols$id[i], stacked$ISSDOSE), ]
      d$ISSCOL <- rep(cols$id[i], nrow(d))
      for (v in intersect(iss_treatment_vars, names(d))) {
        d[[v]] <- rep(cols$label[i], nrow(d))
        d[[paste0(v, "N")]] <- rep(nrow(cols) + 1L - i, nrow(d))
      }
      d
    })
    as.data.frame(dplyr::bind_rows(parts))
  }), datasets)
}

#' The safety set of the pool and its columns, every column of the plan included.
#' @param pool Output of [pool_iss_treatment()].
#' @param sap Output of [read_iss_sap()].
#' @return A list: adsl, arms (key, id, label, n) and denom.
iss_safety_set <- function(pool, sap) {
  adsl <- pool$ADSL[pool$ADSL$SAFFL %in% "Y", ]
  cols <- iss_columns(sap)
  arms <- data.frame(
    key = paste0("arm", seq_len(nrow(cols))), id = cols$id, label = cols$label,
    n = vapply(cols$id, function(id) sum(adsl$ISSCOL == id), 0L), row.names = NULL
  )
  list(adsl = adsl, arms = arms, denom = stats::setNames(arms$n, arms$key))
}

#' Patient-years of exposure of each column.
#' @param pool Output of [pool_iss_treatment()].
#' @param s Output of [iss_safety_set()].
iss_years <- function(pool, s) {
  ex <- pool$ADEX[pool$ADEX$SAFFL %in% "Y", ]
  stats::setNames(vapply(s$arms$id, function(id) sum(ex$TRTDURY[ex$ISSCOL == id], na.rm = TRUE), 0), s$arms$key)
}

#' A rate per 100 patient-years as printed; empty without exposure.
#' @param count Patients or events.
#' @param years Patient-years.
iss_rate <- function(count, years) if (years > 0) fmt_num(100 * count / years, 1) else ""

#' Estimate with its confidence interval, "e (l, u)"; a value that rounds to zero has no sign.
#' @param est,lower,upper Estimate and limits.
#' @param digits Decimals.
iss_est_ci <- function(est, lower, upper, digits) {
  shown <- sub("^-(0([.]0+)?)$", "\\1", fmt_num(c(est, lower, upper), digits))
  sprintf("%s (%s, %s)", shown[1], shown[2], shown[3])
}

#' Cells of a patients row: n (%) and the exposure-adjusted incidence rate of each column.
#' @param d Records of the events (with USUBJID and ISSCOL).
#' @param s Output of [iss_safety_set()].
#' @param years Output of [iss_years()].
iss_patient_cells <- function(d, s, years) {
  cells <- list()
  for (i in seq_len(nrow(s$arms))) {
    k <- s$arms$key[i]
    n <- length(unique(d$USUBJID[d$ISSCOL == s$arms$id[i]]))
    cells[[paste0(k, "_n")]] <- npct_301(n, s$arms$n[i])
    cells[[paste0(k, "_r")]] <- iss_rate(n, years[[k]])
  }
  cells
}

#' Column headers of a patients table: n (%) and the exposure-adjusted incidence rate.
#' @inheritParams iss_patient_cells
iss_patient_columns <- function(s, years) {
  unlist(lapply(seq_len(nrow(s$arms)), function(i) {
    stats::setNames(
      c(sprintf("%s|(N=%d)|n (%%)", s$arms$label[i], s$arms$n[i]), sprintf("|PY=%s|EAIR", fmt_num(years[[i]], 1))),
      paste0(s$arms$key[i], c("_n", "_r"))
    )
  }))
}

#' The rows of the overview of adverse events: label, records and indent.
#' @param ae Treatment-emergent adverse events.
iss_overview_rows <- function(ae) {
  row <- function(label, flag, indent = 0L) list(label = label, records = ae[flag & !is.na(flag), ], indent = indent)
  list(
    row("Any TEAE", rep(TRUE, nrow(ae))),
    row("Related to study drug", ae$AEREL == "RELATED", 1L),
    row("Severe", ae$ASEV == "SEVERE", 1L),
    row("Any serious TEAE", ae$AESER == "Y"),
    row("TEAE leading to discontinuation of study drug", ae$AEACN == "DRUG WITHDRAWN"),
    row("TEAE leading to interruption of study drug", ae$AEACN == "DRUG INTERRUPTED"),
    row("Fatal TEAE", ae$AEOUT == "FATAL")
  )
}

#' Footnotes every table of the pool carries.
iss_pool_notes <- function() {
  c(
    paste(
      "Placebo-controlled pool of FJF-2026-201 (12 weeks) and FJF-2026-301 (26 weeks), by treatment actually taken.",
      "Placebo and FJF-2026 20 mg come from both studies; the other doses from FJF-2026-201."
    ),
    "Descriptive: no adjustment for multiplicity and no test of a hypothesis (plan of the integrated safety analysis)."
  )
}

#' The footnote of the exposure-adjusted incidence rate.
iss_eair_note <- function() {
  paste(
    "EAIR: patients with at least one event per 100 patient-years of exposure (PY, ADEX.TRTDURY);",
    "the measure to compare columns, as the studies differ in length."
  )
}

# --- FJF-2026 20 mg against placebo, stratified by study -------------------------------------

#' Mantel-Haenszel risk difference over strata, with the variance of Sato (1989).
#'
#' The weights are n1 n0 / (n1 + n0). Sato's variance holds with few large strata and with
#' many small ones; with one stratum it is the usual variance of a difference of proportions.
#' A stratum without patients in one of the arms carries no information and is left out.
#'
#' @param x1,n1 Patients with the event and patients, active arm, by stratum.
#' @param x0,n0 The same for the control arm.
#' @param level Confidence level.
#' @return A list: estimate, se, lower, upper (proportions, not percentages).
mh_risk_difference <- function(x1, n1, x0, n0, level = 0.95) {
  keep <- n1 > 0 & n0 > 0
  x1 <- x1[keep]
  n1 <- n1[keep]
  x0 <- x0[keep]
  n0 <- n0[keep]
  if (length(n1) == 0) {
    return(list(estimate = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  n <- n1 + n0
  w <- n1 * n0 / n
  estimate <- sum(w * (x1 / n1 - x0 / n0)) / sum(w)
  p <- (n1^2 * x0 - n0^2 * x1 + n1 * n0 * (n0 - n1) / 2) / n^2
  q <- (x1 * (n0 - x0) + x0 * (n1 - x1)) / (2 * n)
  se <- sqrt(max(estimate * sum(p) + sum(q), 0)) / sum(w)
  z <- stats::qnorm(1 - (1 - level) / 2)
  list(estimate = estimate, se = se, lower = estimate - z * se, upper = estimate + z * se)
}

#' The patients of FJF-2026 20 mg and of placebo, with their study.
#' @param s Output of [iss_safety_set()].
iss_two_arms <- function(s) s$adsl[s$adsl$ISSCOL %in% c("FJF20", "PBO"), c("USUBJID", "STUDYID", "ISSCOL")]

#' Cells of a comparison row: n/N (%) of each arm in each study and pooled, and the risk difference.
#' @param ids USUBJID of the patients with the event.
#' @param two Output of [iss_two_arms()].
iss_comparison_cells <- function(ids, two) {
  studies <- sort(unique(two$STUDYID))
  count <- function(study, col) {
    in_cell <- two$ISSCOL == col & (is.null(study) | two$STUDYID %in% study)
    c(x = sum(in_cell & two$USUBJID %in% ids), n = sum(in_cell))
  }
  shown <- function(v) {
    if (v[["n"]] == 0) {
      return("0/0")
    }
    sprintf("%d/%d (%s%%)", v[["x"]], v[["n"]], fmt_num(100 * v[["x"]] / v[["n"]], 1))
  }
  cells <- list()
  by_study <- lapply(studies, function(st) list(a = count(st, "FJF20"), p = count(st, "PBO")))
  for (i in seq_along(studies)) {
    cells[[sprintf("s%d_a", i)]] <- shown(by_study[[i]]$a)
    cells[[sprintf("s%d_p", i)]] <- shown(by_study[[i]]$p)
  }
  cells$all_a <- shown(count(NULL, "FJF20"))
  cells$all_p <- shown(count(NULL, "PBO"))
  x1 <- vapply(by_study, function(b) b$a[["x"]], 0)
  x0 <- vapply(by_study, function(b) b$p[["x"]], 0)
  rd <- mh_risk_difference(
    x1, vapply(by_study, function(b) b$a[["n"]], 0), x0, vapply(by_study, function(b) b$p[["n"]], 0)
  )
  cells$rd <- if (is.na(rd$estimate) || sum(x1) + sum(x0) == 0) {
    "-"
  } else {
    iss_est_ci(100 * rd$estimate, 100 * rd$lower, 100 * rd$upper, 1)
  }
  cells
}

#' Column headers of a comparison table.
#' @param two Output of [iss_two_arms()].
#' @param s Output of [iss_safety_set()].
iss_comparison_columns <- function(two, s) {
  studies <- sort(unique(two$STUDYID))
  label <- function(id) s$arms$label[s$arms$id == id]
  pair <- function(head) c(sprintf("%s|%s|n/N (%%)", head, label("FJF20")), sprintf("|%s|n/N (%%)", label("PBO")))
  heads <- unlist(lapply(studies, pair))
  names(heads) <- unlist(lapply(seq_along(studies), function(i) sprintf("s%d_%s", i, c("a", "p"))))
  c(heads, stats::setNames(pair("Both studies"), c("all_a", "all_p")), rd = "Risk|difference|(95% CI)")
}

#' The footnotes of a comparison table.
iss_comparison_notes <- function() {
  c(
    "n/N (%): patients with at least one event over the patients of the arm.",
    paste(
      "Risk difference: FJF-2026 20 mg minus placebo, in percentage points, Mantel-Haenszel estimate stratified",
      "by study with the variance of Sato (1989); '-' when no patient has the event."
    )
  )
}

# --- Tables -----------------------------------------------------------------------------------

#' A table of FJF-2026-301 made by its own program on the pooled datasets.
#' @param id Output id in the ISS specification (its `source` names the 301 program).
#' @param pool Output of [pool_iss_treatment()].
#' @param outputs The ISS outputs specification by treatment.
iss_from_301 <- function(id, pool, outputs) {
  spec <- output_spec_301(outputs, id)
  f <- iss_source_functions()[[spec$source]]
  if (is.null(f)) stop("no FJF-2026-301 program for ", spec$source)
  as_301 <- list(study = outputs$study, outputs = list(list(
    id = spec$source, title = spec$title, population = spec$population, program = spec$program, type = spec$type
  )))
  table <- f(pool, NULL, as_301)
  table$id <- id
  table$footnotes <- c(table$footnotes, iss_pool_notes())
  table
}

#' T-ISS-8 Exposure to study drug by treatment (I6): the program of T-14.3.6.
#' @param pool Output of [pool_iss_treatment()].
#' @param sap Output of [read_iss_sap()].
#' @param outputs The ISS outputs specification by treatment.
t_iss_8 <- function(pool, sap, outputs) iss_from_301("T-ISS-8", pool, outputs)

#' T-ISS-9 Overview of treatment-emergent adverse events by treatment (I1).
#' @inheritParams t_iss_8
t_iss_9 <- function(pool, sap, outputs) {
  s <- iss_safety_set(pool, sap)
  years <- iss_years(pool, s)
  rows <- lapply(iss_overview_rows(teae_301(pool)), function(r) {
    table_rows_301(r$label, iss_patient_cells(r$records, s, years), r$indent)
  })
  tlf_table_301("T-ISS-9", outputs, iss_patient_columns(s, years), do.call(bind_rows_301, rows),
    footnotes = c(
      "TEAE: treatment-emergent adverse event (ADAE.TRTEMFL). Patients are counted once in each row.",
      iss_eair_note(), iss_pool_notes()
    )
  )
}

#' T-ISS-10 Overview of adverse events, FJF-2026 20 mg against placebo, stratified by study (I1).
#' @inheritParams t_iss_8
t_iss_10 <- function(pool, sap, outputs) {
  s <- iss_safety_set(pool, sap)
  two <- iss_two_arms(s)
  rows <- lapply(iss_overview_rows(teae_301(pool)), function(r) {
    table_rows_301(r$label, iss_comparison_cells(unique(r$records$USUBJID), two), r$indent)
  })
  tlf_table_301("T-ISS-10", outputs, iss_comparison_columns(two, s), do.call(bind_rows_301, rows),
    footnotes = c(iss_comparison_notes(), iss_pool_notes())
  )
}

#' System organ classes and their preferred terms, sorted by the number of patients on all
#' doses of FJF-2026, then on every column, then alphabetically.
#' @param ae Treatment-emergent adverse events of the pool.
#' @return A list of lists: soc and its ordered terms.
iss_soc_pt_order <- function(ae) {
  once <- ae[ae$ISSCOL != "FJFALL", ]
  order_of <- function(d, d_once, by) {
    terms <- unique(d_once[[by]])
    n_active <- vapply(terms, function(t) length(unique(d$USUBJID[d[[by]] == t & d$ISSCOL == "FJFALL"])), 0L)
    n_all <- vapply(terms, function(t) length(unique(d_once$USUBJID[d_once[[by]] == t])), 0L)
    terms[order(-n_active, -n_all, terms)]
  }
  lapply(order_of(ae, once, "AEBODSYS"), function(soc) {
    in_soc <- ae$AEBODSYS == soc
    list(soc = soc, terms = order_of(ae[in_soc, ], once[once$AEBODSYS == soc, ], "AEDECOD"))
  })
}

#' Rows of a table by system organ class and preferred term.
#' @param ae Treatment-emergent adverse events of the pool.
#' @param cells Function giving the cells of a set of records.
iss_soc_pt_rows <- function(ae, cells) {
  rows <- list(table_rows_301("Any TEAE", cells(ae)))
  for (block in iss_soc_pt_order(ae)) {
    d <- ae[ae$AEBODSYS == block$soc, ]
    rows[[length(rows) + 1]] <- table_rows_301(tools::toTitleCase(tolower(block$soc)), cells(d))
    for (pt in block$terms) rows[[length(rows) + 1]] <- table_rows_301(pt, cells(d[d$AEDECOD == pt, ]), 1L)
  }
  do.call(bind_rows_301, rows)
}

#' T-ISS-11 Adverse events by system organ class and preferred term: patients (I2).
#' @inheritParams t_iss_8
t_iss_11 <- function(pool, sap, outputs) {
  s <- iss_safety_set(pool, sap)
  years <- iss_years(pool, s)
  rows <- iss_soc_pt_rows(teae_301(pool), function(d) iss_patient_cells(d, s, years))
  tlf_table_301("T-ISS-11", outputs, iss_patient_columns(s, years), rows,
    footnotes = c(
      "n (%): patients with at least one event.", iss_eair_note(),
      "Sorted by the number of patients on all doses of FJF-2026. MedDRA system organ class and preferred term.",
      iss_pool_notes()
    )
  )
}

#' T-ISS-12 Adverse events by system organ class and preferred term: events (I2).
#' @inheritParams t_iss_8
t_iss_12 <- function(pool, sap, outputs) {
  s <- iss_safety_set(pool, sap)
  years <- iss_years(pool, s)
  cells <- function(d) {
    out <- list()
    for (i in seq_len(nrow(s$arms))) {
      k <- s$arms$key[i]
      events <- sum(d$ISSCOL == s$arms$id[i])
      out[[paste0(k, "_e")]] <- as.character(events)
      out[[paste0(k, "_r")]] <- iss_rate(events, years[[k]])
    }
    out
  }
  columns <- unlist(lapply(seq_len(nrow(s$arms)), function(i) {
    stats::setNames(
      c(sprintf("%s|(N=%d)|E", s$arms$label[i], s$arms$n[i]), sprintf("|PY=%s|E/100 PY", fmt_num(years[[i]], 1))),
      paste0(s$arms$key[i], c("_e", "_r"))
    )
  }))
  tlf_table_301("T-ISS-12", outputs, columns, iss_soc_pt_rows(teae_301(pool), cells),
    footnotes = c(
      "E: events. E/100 PY: events per 100 patient-years of exposure (PY, ADEX.TRTDURY).",
      "Same order as T-ISS-11. MedDRA system organ class and preferred term.", iss_pool_notes()
    )
  )
}

#' Preferred terms with an incidence of at least 2% in any FJF-2026 column, sorted by the
#' incidence on FJF-2026 20 mg, then alphabetically.
#' @param ae Treatment-emergent adverse events of the pool.
#' @param s Output of [iss_safety_set()].
#' @param threshold Incidence in percent.
iss_common_terms <- function(ae, s, threshold = 2) {
  active <- s$arms[s$arms$id != "PBO" & s$arms$n > 0, ]
  terms <- sort(unique(ae$AEDECOD))
  incidence <- function(term, id, n) 100 * length(unique(ae$USUBJID[ae$AEDECOD == term & ae$ISSCOL == id])) / n
  common <- terms[vapply(terms, function(t) {
    any(mapply(function(id, n) incidence(t, id, n), active$id, active$n) >= threshold)
  }, TRUE)]
  n20 <- s$arms$n[s$arms$id == "FJF20"]
  on20 <- vapply(common, function(t) if (n20 > 0) incidence(t, "FJF20", n20) else 0, 0)
  common[order(-on20, common)]
}

#' T-ISS-13 Common adverse events, FJF-2026 20 mg against placebo, stratified by study (I2).
#' @inheritParams t_iss_8
t_iss_13 <- function(pool, sap, outputs) {
  s <- iss_safety_set(pool, sap)
  two <- iss_two_arms(s)
  ae <- teae_301(pool)
  rows <- lapply(iss_common_terms(ae, s), function(pt) {
    table_rows_301(pt, iss_comparison_cells(unique(ae$USUBJID[ae$AEDECOD == pt]), two))
  })
  tlf_table_301("T-ISS-13", outputs, iss_comparison_columns(two, s), do.call(bind_rows_301, rows),
    footnotes = c(
      "Preferred terms with an incidence of 2% or more in any FJF-2026 column of the pool (T-ISS-11).",
      "Sorted by the incidence on FJF-2026 20 mg.", iss_comparison_notes(), iss_pool_notes()
    )
  )
}

#' T-ISS-14 Hypoglycaemia by level and treatment (I3): the program of T-14.3.3.
#' @inheritParams t_iss_8
t_iss_14 <- function(pool, sap, outputs) iss_from_301("T-ISS-14", pool, outputs)

#' Rate ratio of a count between two arms, from a negative binomial model.
#'
#' Log link, treatment and study as factors (study only when there is more than one), log of
#' the exposure as offset. Not estimable when one arm has no event.
#'
#' @param count Events of each patient.
#' @param active Whether the patient is in the active arm.
#' @param study Study of each patient.
#' @param years Exposure of each patient, in years.
#' @return A list: estimate, lower, upper (95% Wald, on the log scale), theta and notes.
nb_rate_ratio <- function(count, active, study, years) {
  d <- data.frame(count = count, active = as.integer(active), study = factor(study), years = years)
  out <- list(estimate = NA_real_, lower = NA_real_, upper = NA_real_, theta = NA_real_, notes = character())
  if (sum(d$count[d$active == 1]) == 0 || sum(d$count[d$active == 0]) == 0) {
    out$notes <- "not estimable: one arm has no episode"
    return(out)
  }
  model <- if (nlevels(d$study) > 1) {
    count ~ active + study + offset(log(years))
  } else {
    count ~ active + offset(log(years))
  }
  notes <- character()
  fit <- tryCatch(
    withCallingHandlers(MASS::glm.nb(model, data = d), warning = function(w) {
      notes <<- c(notes, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      notes <<- c(notes, conditionMessage(e))
      NULL
    }
  )
  out$notes <- unique(notes)
  if (is.null(fit)) {
    return(out)
  }
  b <- stats::coef(fit)[["active"]]
  se <- sqrt(diag(stats::vcov(fit)))[["active"]]
  z <- stats::qnorm(0.975)
  out$estimate <- exp(b)
  out$lower <- exp(b - z * se)
  out$upper <- exp(b + z * se)
  out$theta <- fit$theta
  out
}

#' Level 2 or 3 hypoglycaemic episodes on treatment of the patients of FJF-2026 20 mg and placebo.
#' @param pool Output of [pool_iss_treatment()].
#' @param s Output of [iss_safety_set()].
#' @return A data frame: USUBJID, STUDYID, ISSCOL, years, episodes.
iss_hypo_counts <- function(pool, s) {
  two <- iss_two_arms(s)
  ex <- pool$ADEX[pool$ADEX$ISSCOL %in% c("FJF20", "PBO"), ]
  hypo <- pool$ADHYPO
  hypo <- hypo[hypo$SAFFL %in% "Y" & hypo$ONTRTFL %in% "Y" & hypo$ISSCOL %in% c("FJF20", "PBO"), ]
  hypo <- hypo[as.integer(hypo$HYPOLVL) >= 2, ]
  two$years <- ex$TRTDURY[match(two$USUBJID, ex$USUBJID)]
  two$episodes <- vapply(two$USUBJID, function(id) sum(hypo$USUBJID == id), 0L)
  two
}

#' T-ISS-15 Level 2 or 3 hypoglycaemia, FJF-2026 20 mg against placebo, negative binomial model (I3).
#' @inheritParams t_iss_8
t_iss_15 <- function(pool, sap, outputs) {
  s <- iss_safety_set(pool, sap)
  d <- iss_hypo_counts(pool, s)
  label <- function(id) s$arms$label[s$arms$id == id]
  block <- function(head, rows_in) {
    cell <- function(f) list(a = f(d[rows_in & d$ISSCOL == "FJF20", ]), p = f(d[rows_in & d$ISSCOL == "PBO", ]))
    bind_rows_301(
      heading_301(head),
      table_rows_301("Patients", cell(function(x) as.character(nrow(x))), 1L),
      table_rows_301("Patients with an episode, n (%)", cell(function(x) npct_301(sum(x$episodes > 0), nrow(x))), 1L),
      table_rows_301("Episodes", cell(function(x) as.character(sum(x$episodes))), 1L),
      table_rows_301("Patient-years", cell(function(x) fmt_num(sum(x$years), 1)), 1L),
      table_rows_301("Episodes per 100 patient-years", cell(function(x) iss_rate(sum(x$episodes), sum(x$years))), 1L)
    )
  }
  studies <- sort(unique(d$STUDYID))
  blocks <- lapply(studies, function(st) block(st, d$STUDYID == st))
  fit <- nb_rate_ratio(d$episodes, d$ISSCOL == "FJF20", d$STUDYID, d$years)
  ratio <- if (is.na(fit$estimate)) "-" else iss_est_ci(fit$estimate, fit$lower, fit$upper, 2)
  model <- bind_rows_301(
    heading_301("Negative binomial model"),
    table_rows_301("Rate ratio, FJF-2026 20 mg / placebo (95% CI)", list(est = ratio), 1L),
    table_rows_301("Dispersion (theta)", list(est = if (is.na(fit$theta)) "-" else fmt_num(fit$theta, 2)), 1L)
  )
  rows <- do.call(bind_rows_301, c(blocks, list(block("Both studies", rep(TRUE, nrow(d))), model)))
  tlf_table_301("T-ISS-15", outputs, c(a = label("FJF20"), p = label("PBO"), est = "Estimate"), rows,
    footnotes = c(
      "Episodes of level 2 or 3 on treatment (ADHYPO.HYPOLVL, ADHYPO.ONTRTFL); exposure from ADEX.TRTDURY.",
      paste(
        "Negative binomial model of the episodes of each patient: treatment and study as factors, log of the",
        "exposure as offset; 95% Wald confidence interval; '-' when not estimable."
      ),
      "Theta: a large value means little overdispersion; the model is then close to a Poisson model.",
      if (length(fit$notes) > 0) sprintf("Model note: %s.", paste(fit$notes, collapse = "; ")),
      iss_pool_notes()
    )
  )
}

#' The dose-response of safety within FJF-2026-201: patients with each measure, by dose.
#' @param pool Output of [pool_iss_treatment()].
#' @param sap Output of [read_iss_sap()].
#' @return A data frame: id, label, dose, n (patients of the dose), measure, x, pct.
iss_dose_response <- function(pool, sap) {
  in_201 <- function(d) d[d$STUDYID == "FJF-2026-201" & d$ISSCOL != "FJFALL", ]
  adsl <- in_201(pool$ADSL[pool$ADSL$SAFFL %in% "Y", ])
  ae <- in_201(teae_301(pool))
  hypo <- in_201(pool$ADHYPO[pool$ADHYPO$SAFFL %in% "Y" & pool$ADHYPO$ONTRTFL %in% "Y", ])
  cols <- iss_columns(sap)
  cols <- cols[cols$id != "FJFALL", ]
  measures <- list(
    "Any TEAE" = ae$USUBJID,
    "TEAE leading to discontinuation of study drug" = ae$USUBJID[ae$AEACN %in% "DRUG WITHDRAWN"],
    "Level 2 or 3 hypoglycaemia" = hypo$USUBJID[as.integer(hypo$HYPOLVL) >= 2]
  )
  do.call(rbind, lapply(names(measures), function(m) {
    n <- vapply(cols$id, function(id) sum(adsl$ISSCOL == id), 0L)
    x <- vapply(cols$id, function(id) sum(adsl$ISSCOL == id & adsl$USUBJID %in% measures[[m]]), 0L)
    data.frame(
      id = cols$id, label = cols$label, dose = iss_column_dose(cols$id), n = n, measure = m, x = x,
      pct = ifelse(n > 0, 100 * x / n, NA_real_), row.names = NULL
    )
  }))
}

#' T-ISS-16 Dose-response of safety in FJF-2026-201 (I4).
#' @inheritParams t_iss_8
t_iss_16 <- function(pool, sap, outputs) {
  d <- iss_dose_response(pool, sap)
  first <- d[d$measure == d$measure[1], ]
  keys <- paste0("arm", seq_len(nrow(first)))
  rows <- lapply(unique(d$measure), function(m) {
    table_rows_301(m, stats::setNames(as.list(npct_301(d$x[d$measure == m], d$n[d$measure == m])), keys))
  })
  tlf_table_301("T-ISS-16", outputs, stats::setNames(sprintf("%s|(N=%d)", first$label, first$n), keys),
    do.call(bind_rows_301, rows),
    footnotes = c(
      "FJF-2026-201 only, the one study with every dose. n (%): patients with at least one event or episode.",
      "Hypoglycaemia: episodes on treatment (ADHYPO.ONTRTFL). Shown against dose without a test (F-ISS-1).",
      iss_pool_notes()[2]
    )
  )
}

#' F-ISS-1 Dose-response of safety in FJF-2026-201 (I4).
#' @inheritParams t_iss_8
#' @param path File to write.
#' @param locked Whether both studies are unblinded.
f_iss_1 <- function(pool, sap, outputs, path, locked) {
  d <- iss_dose_response(pool, sap)
  plot <- ggplot2::ggplot(d, ggplot2::aes(.data$dose, .data$pct, colour = .data$measure, shape = .data$measure)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = unique(d$dose)) +
    ggplot2::labs(x = "Dose of FJF-2026 (mg; 0: placebo)", y = "Patients (%)", colour = NULL, shape = NULL)
  save_figure_301(plot, "F-ISS-1", outputs, path, locked,
    footnotes = c("FJF-2026-201 only. Percentages of T-ISS-16, joined by lines; no model and no test.")
  )
}

#' Peak ALT, AST and total bilirubin on treatment of each patient, as multiples of the upper limit of normal.
#' @param pool Output of [pool_iss_treatment()].
#' @return A data frame: USUBJID, STUDYID, ISSCOL, TRTA, ALT, AST, BILI, AT (the larger of ALT
#'   and AST) and the flags at3 (AT > 3), bili2 (BILI > 2) and both.
iss_liver_peaks <- function(pool) {
  lb <- pool$ADLB
  lb <- lb[lb$SAFFL %in% "Y" & lb$ONTRTFL %in% "Y" & lb$PARAMCD %in% c("ALT", "AST", "BILI"), ]
  lb <- lb[!is.na(lb$AVAL) & !is.na(lb$ANRHI) & lb$ANRHI > 0, ]
  lb$xuln <- lb$AVAL / lb$ANRHI
  grouped <- dplyr::group_by(lb, .data$USUBJID, .data$STUDYID, .data$ISSCOL, .data$TRTA, .data$PARAMCD)
  peak <- dplyr::summarise(grouped, xuln = max(.data$xuln), .groups = "drop")
  wide <- as.data.frame(tidyr::pivot_wider(peak, names_from = "PARAMCD", values_from = "xuln"))
  for (p in setdiff(c("ALT", "AST", "BILI"), names(wide))) wide[[p]] <- rep(NA_real_, nrow(wide))
  wide$AT <- pmax(wide$ALT, wide$AST, na.rm = TRUE)
  wide$at3 <- !is.na(wide$AT) & wide$AT > 3
  wide$bili2 <- !is.na(wide$BILI) & wide$BILI > 2
  wide$both <- wide$at3 & wide$bili2
  wide
}

#' T-ISS-17 Liver safety by treatment (I5).
#' @inheritParams t_iss_8
t_iss_17 <- function(pool, sap, outputs) {
  s <- iss_safety_set(pool, sap)
  peaks <- iss_liver_peaks(pool)
  cells <- function(flag, percent = TRUE) {
    stats::setNames(lapply(seq_len(nrow(s$arms)), function(i) {
      n <- sum(flag & peaks$ISSCOL == s$arms$id[i])
      if (percent) npct_301(n, s$arms$n[i]) else as.character(n)
    }), s$arms$key)
  }
  rows <- bind_rows_301(
    table_rows_301(
      "Patients with ALT or AST and bilirubin on treatment", cells(!is.na(peaks$AT) & !is.na(peaks$BILI), FALSE)
    ),
    table_rows_301("Peak ALT or AST >3x ULN", cells(peaks$at3)),
    table_rows_301("Peak total bilirubin >2x ULN", cells(peaks$bili2)),
    table_rows_301("Peak ALT or AST >3x ULN and peak total bilirubin >2x ULN", cells(peaks$both))
  )
  tlf_table_301("T-ISS-17", outputs, arm_columns_301(s$arms), rows,
    footnotes = c(
      "Peak: the highest value on treatment (ADLB.ONTRTFL), as a multiple of the upper limit of normal (ULN).",
      "The two peaks need not be on the same day. Last row: potential Hy's law cases, listed in L-ISS-1.",
      "n (%): of the patients of the column.", iss_pool_notes()
    )
  )
}

#' L-ISS-1 Patients with peak ALT or AST above 3 times and peak bilirubin above 2 times the ULN (I5).
#' @inheritParams t_iss_8
l_iss_1 <- function(pool, sap, outputs) {
  peaks <- iss_liver_peaks(pool)
  cases <- peaks[peaks$both & peaks$ISSCOL != "FJFALL", ]
  cases <- cases[order(cases$STUDYID, cases$USUBJID), ]
  # Without a case the listing says "No records".
  rows <- if (nrow(cases) == 0) {
    bind_rows_301()
  } else {
    table_rows_301(cases$USUBJID, list(
      study = cases$STUDYID, trt = cases$TRTA, alt = fmt_num(cases$ALT, 2), ast = fmt_num(cases$AST, 2),
      bili = fmt_num(cases$BILI, 2)
    ))
  }
  tlf_table_301("L-ISS-1", outputs,
    c(
      study = "Study", trt = "Treatment", alt = "Peak ALT|(x ULN)", ast = "Peak AST|(x ULN)",
      bili = "Peak bilirubin|(x ULN)"
    ),
    rows,
    footnotes = c("Peaks on treatment (ADLB.ONTRTFL), as multiples of the upper limit of normal (ULN).")
  )
}

#' F-ISS-2 eDISH plot of peak ALT or AST against peak total bilirubin, by treatment (I5).
#' @inheritParams f_iss_1
f_iss_2 <- function(pool, sap, outputs, path, locked) {
  peaks <- iss_liver_peaks(pool)
  peaks <- peaks[peaks$ISSCOL != "FJFALL" & !is.na(peaks$AT) & !is.na(peaks$BILI), ]
  cols <- iss_columns(sap)
  peaks$TRTA <- factor(peaks$TRTA, levels = cols$label[cols$id != "FJFALL"])
  plot <- ggplot2::ggplot(peaks, ggplot2::aes(.data$AT, .data$BILI, colour = .data$TRTA, shape = .data$TRTA)) +
    ggplot2::geom_vline(xintercept = 3, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(
      x = "Peak ALT or AST on treatment (x ULN)", y = "Peak total bilirubin on treatment (x ULN)",
      colour = NULL, shape = NULL
    )
  save_figure_301(plot, "F-ISS-2", outputs, path, locked,
    footnotes = c(
      "One point per patient: the larger of the peaks of ALT and AST. Reference lines: 3x ULN and 2x ULN.",
      "ULN: upper limit of normal. Both studies, by treatment actually taken."
    )
  )
}

# --- Every output -----------------------------------------------------------------------------

#' The functions that make the outputs, by output id, listed by name so that targets sees
#' them as dependencies.
iss_treatment_functions <- function() {
  list(
    `T-ISS-8` = t_iss_8, `T-ISS-9` = t_iss_9, `T-ISS-10` = t_iss_10, `T-ISS-11` = t_iss_11,
    `T-ISS-12` = t_iss_12, `T-ISS-13` = t_iss_13, `T-ISS-14` = t_iss_14, `T-ISS-15` = t_iss_15,
    `T-ISS-16` = t_iss_16, `F-ISS-1` = f_iss_1, `T-ISS-17` = t_iss_17, `L-ISS-1` = l_iss_1, `F-ISS-2` = f_iss_2
  )
}

#' Check the outputs by treatment against the plan.
#'
#' Every output has a unique id, the safety set, at least one analysis of the plan, ADaM
#' references of the form DATASET.VARIABLE with a declared dataset, a program under
#' studies/iss/tlf/ and a function. Every analysis of the plan has at least one output.
#'
#' @param outputs The ISS outputs specification by treatment.
#' @param sap Output of [read_iss_sap()].
#' @return A character vector of problems; empty when consistent.
check_iss_outputs <- function(outputs, sap) {
  problems <- character()
  ids <- purrr::map_chr(outputs$outputs, "id")
  if (anyDuplicated(ids)) problems <- c(problems, sprintf("duplicated output %s", unique(ids[duplicated(ids)])))
  analyses <- purrr::map_chr(sap$analyses, "id")
  for (o in outputs$outputs) {
    if (!identical(o$population, "SAF")) problems <- c(problems, sprintf("%s: population is not SAF", o$id))
    unknown <- setdiff(unlist(o$sap), analyses)
    if (length(o$sap) == 0 || length(unknown) > 0) {
      problems <- c(problems, sprintf("%s: unknown analysis %s", o$id, paste(unknown, collapse = ", ")))
    }
    refs <- unlist(o$adam)
    bad <- refs[!grepl("^[A-Z]+\\.[A-Z0-9]+$", refs) | !sub("\\..*", "", refs) %in% outputs$adam_datasets]
    if (length(bad) > 0) {
      problems <- c(problems, sprintf("%s: bad ADaM reference %s", o$id, paste(bad, collapse = ", ")))
    }
    if (!startsWith(o$program, "studies/iss/tlf/")) {
      problems <- c(problems, sprintf("%s: program outside studies/iss/tlf/", o$id))
    }
    if (is.null(iss_treatment_functions()[[o$id]])) problems <- c(problems, sprintf("%s: no function", o$id))
  }
  uncovered <- setdiff(analyses, unique(unlist(purrr::map(outputs$outputs, "sap"))))
  if (length(uncovered) > 0) problems <- c(problems, sprintf("no output for %s", paste(uncovered, collapse = ", ")))
  problems
}

#' Every table and listing of the specification.
#'
#' @param pool Output of [pool_iss_treatment()].
#' @param sap Output of [read_iss_sap()].
#' @param outputs The ISS outputs specification by treatment.
#' @return A named list of tables ([tlf_table_301()]), by id.
tables_iss_treatment <- function(pool, sap, outputs) {
  ids <- purrr::map_chr(purrr::keep(outputs$outputs, function(o) o$type != "figure"), "id")
  stats::setNames(lapply(ids, function(id) iss_treatment_functions()[[id]](pool, sap, outputs)), ids)
}

#' Write every output: tables and listing as text, figures as PNG.
#'
#' @param tables Output of [tables_iss_treatment()].
#' @inheritParams tables_iss_treatment
#' @param locked Whether both studies are unblinded (otherwise the outputs carry the blinding note).
#' @param dir Output directory.
#' @return The paths written.
write_tlf_iss_treatment <- function(tables, pool, sap, outputs, locked, dir) {
  unlink(dir, recursive = TRUE)
  vapply(outputs$outputs, function(o) {
    path <- output_file_301(o$id, dir)
    if (o$type == "figure") {
      return(iss_treatment_functions()[[o$id]](pool, sap, outputs, path, locked))
    }
    write_table_301(tables[[o$id]], path, locked)
    if (o$type == "listing") {
      text <- readLines(path)
      text[2] <- sub("^Table L-", "Listing ", text[2])
      writeLines(text, path)
    }
    path
  }, "")
}
