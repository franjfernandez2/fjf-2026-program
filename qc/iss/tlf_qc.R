# Second programming of the integrated safety analysis by treatment (independent of
# studies/iss/tlf/iss_treatment.R).
#
# It does not use the pooled datasets of the production program. It starts from the ADaM
# datasets of each study, finds who is in each column from ADSL (the dose actually taken) and
# recomputes the cells with base R. The risk difference is written from the 2x2 cell counts,
# and the negative binomial model is fitted by maximising its likelihood directly, not with
# MASS::glm.nb. Each expected cell must be in the production table, as printed, at the same
# section, row and column key. Only the number formats (R/tlf.R) are shared.

#' An expected cell of an ISS table, by column key.
qc_cell_iss <- function(id, section, label, key, value) {
  data.frame(id = id, section = section, label = label, key = key, value = as.character(value))
}

#' A rate per 100 patient-years as the shells print it.
qc_rate_iss <- function(count, years) if (years > 0) fmt_num(100 * count / years, 1) else ""

#' A number as printed; one that rounds to zero is printed without a sign.
qc_num_iss <- function(x, digits) {
  shown <- fmt_num(x, digits)
  if (as.numeric(shown) == 0) fmt_num(0, digits) else shown
}

#' "x/n (p%)" as the comparison tables print it.
qc_frac_iss <- function(x, n) if (n == 0) "0/0" else sprintf("%d/%d (%s%%)", x, n, fmt_num(100 * x / n, 1))

#' Mantel-Haenszel risk difference and the variance of Sato (1989), from the cells of each
#' stratum's 2x2 table: a, b (active with and without the event) and c, d (control).
#' @return "estimate (lower, upper)" in percentage points, or "-" without events.
qc_mh_rd_iss <- function(a, b, c, d) {
  if (sum(a) + sum(c) == 0) {
    return("-")
  }
  num <- 0
  den <- 0
  sum_p <- 0
  sum_q <- 0
  for (k in seq_along(a)) {
    n1 <- a[k] + b[k]
    n0 <- c[k] + d[k]
    total <- n1 + n0
    if (n1 == 0 || n0 == 0) next
    num <- num + (a[k] * n0 - c[k] * n1) / total
    den <- den + n1 * n0 / total
    sum_p <- sum_p + (n1^2 * c[k] - n0^2 * a[k] + n1 * n0 * (n0 - n1) / 2) / total^2
    sum_q <- sum_q + (a[k] * d[k] + c[k] * b[k]) / (2 * total)
  }
  rd <- num / den
  se <- sqrt(max(rd * sum_p + sum_q, 0)) / den
  half <- stats::qnorm(0.975) * se
  sprintf("%s (%s, %s)", qc_num_iss(100 * rd, 1), qc_num_iss(100 * (rd - half), 1), qc_num_iss(100 * (rd + half), 1))
}

#' Rate ratio of a negative binomial model, by direct maximum likelihood.
#'
#' Mean = exposure x exp(intercept + treatment + study); variance = mean + mean^2 / size. The
#' standard error comes from the expected information of the coefficients at the estimates.
#' @return "ratio (lower, upper)", or "-" when one arm has no event.
qc_nb_ratio_iss <- function(y, active, study, years) {
  if (sum(y[active]) == 0 || sum(y[!active]) == 0) {
    return("-")
  }
  x <- cbind(1, as.numeric(active))
  studies <- sort(unique(study))
  for (s in studies[-1]) x <- cbind(x, as.numeric(study == s))
  nb <- ncol(x)
  minus_loglik <- function(par) {
    mu <- years * exp(as.vector(x %*% par[seq_len(nb)]))
    -sum(stats::dnbinom(y, size = exp(par[nb + 1]), mu = mu, log = TRUE))
  }
  start <- c(log(sum(y) / sum(years)), rep(0, nb - 1), 0)
  fit <- stats::optim(start, minus_loglik, method = "BFGS", control = list(reltol = 1e-14, maxit = 5000))
  fit <- stats::optim(fit$par, minus_loglik, method = "BFGS", control = list(reltol = 1e-14, maxit = 5000))
  beta <- fit$par[seq_len(nb)]
  mu <- years * exp(as.vector(x %*% beta))
  weight <- mu / (1 + mu / exp(fit$par[nb + 1]))
  se <- sqrt(diag(solve(t(x) %*% (weight * x))))[2]
  half <- stats::qnorm(0.975) * se
  sprintf("%s (%s, %s)", fmt_num(exp(beta[2]), 2), fmt_num(exp(beta[2] - half), 2), fmt_num(exp(beta[2] + half), 2))
}

#' Expected cells of the tables of the integrated safety analysis by treatment.
#'
#' @param adam_201,adam_301 Named lists of the ADaM datasets of each study.
#' @return A data frame of expected cells (id, section, label, key, value).
qc_tlf_iss_treatment <- function(adam_201, adam_301) {
  found <- new.env()
  found$cells <- list()
  add <- function(...) found$cells[[length(found$cells) + 1]] <- qc_cell_iss(...)
  both <- function(ds, vars) {
    one <- function(a, study) cbind(as.data.frame(a[[ds]])[, vars, drop = FALSE], STUDY = rep(study, nrow(a[[ds]])))
    rbind(one(adam_201, "FJF-2026-201"), one(adam_301, "FJF-2026-301"))
  }

  # Who is in each column: the safety set by dose actually taken; all doses last.
  adsl <- both("ADSL", c("USUBJID", "SAFFL", "TRT01AN"))
  saf <- adsl[adsl$SAFFL %in% "Y", ]
  doses <- list(arm1 = 0, arm2 = 5, arm3 = 10, arm4 = 20, arm5 = 40, arm6 = c(5, 10, 20, 40))
  members <- lapply(doses, function(d) saf$USUBJID[saf$TRT01AN %in% d])
  size <- lengths(members)
  ex <- both("ADEX", c("USUBJID", "TRTDURD", "TRTDURY"))
  ex <- ex[ex$USUBJID %in% saf$USUBJID, ]
  years <- vapply(members, function(m) sum(ex$TRTDURY[ex$USUBJID %in% m]), 0)
  ae <- both("ADAE", c("USUBJID", "TRTEMFL", "AEREL", "ASEV", "AESER", "AEACN", "AEOUT", "AEBODSYS", "AEDECOD"))
  ae <- ae[ae$USUBJID %in% saf$USUBJID & ae$TRTEMFL %in% "Y", ]
  patients <- function(d, arm) sum(members[[arm]] %in% d$USUBJID)
  events <- function(d, arm) sum(d$USUBJID %in% members[[arm]])

  # FJF-2026 20 mg against placebo, in each study and in both.
  two <- saf[saf$TRT01AN %in% c(0, 20), ]
  studies <- sort(unique(two$STUDY))
  comparison <- function(id, section, label, with_event) {
    cell <- function(dose, study = studies) {
      who <- two$USUBJID[two$TRT01AN == dose & two$STUDY %in% study]
      c(x = sum(who %in% with_event), n = length(who))
    }
    a <- b <- c0 <- d0 <- numeric()
    for (i in seq_along(studies)) {
      on <- cell(20, studies[i])
      off <- cell(0, studies[i])
      add(id, section, label, sprintf("s%d_a", i), qc_frac_iss(on[["x"]], on[["n"]]))
      add(id, section, label, sprintf("s%d_p", i), qc_frac_iss(off[["x"]], off[["n"]]))
      a[i] <- on[["x"]]
      b[i] <- on[["n"]] - on[["x"]]
      c0[i] <- off[["x"]]
      d0[i] <- off[["n"]] - off[["x"]]
    }
    add(id, section, label, "all_a", qc_frac_iss(cell(20)[["x"]], cell(20)[["n"]]))
    add(id, section, label, "all_p", qc_frac_iss(cell(0)[["x"]], cell(0)[["n"]]))
    add(id, section, label, "rd", qc_mh_rd_iss(a, b, c0, d0))
  }

  # T-ISS-8: exposure.
  for (arm in names(doses)) {
    d <- ex[ex$USUBJID %in% members[[arm]], ]
    add("T-ISS-8", "Duration of treatment (days)", "Mean (SD)", arm, qc_mean_sd_301(d$TRTDURD, 0))
    add("T-ISS-8", "Total exposure (patient-years)", "Total exposure (patient-years)", arm, fmt_num(years[[arm]], 1))
  }

  # T-ISS-9 and T-ISS-10: overview of adverse events.
  overview <- list(
    list("Any TEAE", "Any TEAE", ae),
    list("Any TEAE", "Related to study drug", ae[ae$AEREL %in% "RELATED", ]),
    list("Any TEAE", "Severe", ae[ae$ASEV %in% "SEVERE", ]),
    list("Any serious TEAE", "Any serious TEAE", ae[ae$AESER %in% "Y", ]),
    list(
      "TEAE leading to discontinuation of study drug", "TEAE leading to discontinuation of study drug",
      ae[ae$AEACN %in% "DRUG WITHDRAWN", ]
    ),
    list(
      "TEAE leading to interruption of study drug", "TEAE leading to interruption of study drug",
      ae[ae$AEACN %in% "DRUG INTERRUPTED", ]
    ),
    list("Fatal TEAE", "Fatal TEAE", ae[ae$AEOUT %in% "FATAL", ])
  )
  for (row in overview) {
    for (arm in names(doses)) {
      n <- patients(row[[3]], arm)
      add("T-ISS-9", row[[1]], row[[2]], paste0(arm, "_n"), qc_npct_301(n, size[[arm]]))
      add("T-ISS-9", row[[1]], row[[2]], paste0(arm, "_r"), qc_rate_iss(n, years[[arm]]))
    }
    comparison("T-ISS-10", row[[1]], row[[2]], unique(row[[3]]$USUBJID))
  }

  # T-ISS-11 and T-ISS-12: by system organ class and preferred term.
  by_term <- function(section, label, d) {
    for (arm in names(doses)) {
      n <- patients(d, arm)
      add("T-ISS-11", section, label, paste0(arm, "_n"), qc_npct_301(n, size[[arm]]))
      add("T-ISS-11", section, label, paste0(arm, "_r"), qc_rate_iss(n, years[[arm]]))
      add("T-ISS-12", section, label, paste0(arm, "_e"), events(d, arm))
      add("T-ISS-12", section, label, paste0(arm, "_r"), qc_rate_iss(events(d, arm), years[[arm]]))
    }
  }
  by_term("Any TEAE", "Any TEAE", ae)
  for (soc in unique(ae$AEBODSYS)) {
    in_soc <- ae[ae$AEBODSYS == soc, ]
    section <- tools::toTitleCase(tolower(soc))
    by_term(section, section, in_soc)
    for (pt in unique(in_soc$AEDECOD)) by_term(section, pt, in_soc[in_soc$AEDECOD == pt, ])
  }

  # T-ISS-13: preferred terms with 2% or more in any FJF-2026 column.
  for (pt in unique(ae$AEDECOD)) {
    d <- ae[ae$AEDECOD == pt, ]
    share <- vapply(names(doses)[-1], function(arm) if (size[[arm]] > 0) 100 * patients(d, arm) / size[[arm]] else 0, 0)
    if (any(share >= 2)) comparison("T-ISS-13", pt, pt, unique(d$USUBJID))
  }

  # T-ISS-14 and T-ISS-15: hypoglycaemia on treatment.
  hypo <- both("ADHYPO", c("USUBJID", "HYPOLVL", "ONTRTFL"))
  hypo <- hypo[hypo$USUBJID %in% saf$USUBJID & hypo$ONTRTFL %in% "Y", ]
  levels <- list(
    "Any level" = 1:3, "Level 1 (glucose <3.9 and >=3.0 mmol/L)" = 1, "Level 2 (glucose <3.0 mmol/L)" = 2,
    "Level 3 (severe, needing assistance)" = 3, "Level 2 or 3" = 2:3
  )
  for (l in names(levels)) {
    d <- hypo[as.integer(hypo$HYPOLVL) %in% levels[[l]], ]
    for (arm in names(doses)) {
      add("T-ISS-14", l, l, paste0(arm, "_n"), qc_npct_301(patients(d, arm), size[[arm]]))
      add("T-ISS-14", l, l, paste0(arm, "_e"), events(d, arm))
      add("T-ISS-14", l, l, paste0(arm, "_rate"), qc_rate_iss(events(d, arm), years[[arm]]))
    }
  }
  severe <- hypo[as.integer(hypo$HYPOLVL) %in% 2:3, ]
  two$episodes <- vapply(two$USUBJID, function(id) sum(severe$USUBJID == id), 0L)
  two$years <- ex$TRTDURY[match(two$USUBJID, ex$USUBJID)]
  for (section in c(studies, "Both studies")) {
    in_section <- if (section == "Both studies") rep(TRUE, nrow(two)) else two$STUDY == section
    for (key in c("a", "p")) {
      d <- two[in_section & two$TRT01AN == if (key == "a") 20 else 0, ]
      add("T-ISS-15", section, "Patients", key, nrow(d))
      add("T-ISS-15", section, "Patients with an episode, n (%)", key, qc_npct_301(sum(d$episodes > 0), nrow(d)))
      add("T-ISS-15", section, "Episodes", key, sum(d$episodes))
      add("T-ISS-15", section, "Patient-years", key, fmt_num(sum(d$years), 1))
      add("T-ISS-15", section, "Episodes per 100 patient-years", key, qc_rate_iss(sum(d$episodes), sum(d$years)))
    }
  }
  add(
    "T-ISS-15", "Negative binomial model", "Rate ratio, FJF-2026 20 mg / placebo (95% CI)", "est",
    qc_nb_ratio_iss(two$episodes, two$TRT01AN == 20, two$STUDY, two$years)
  )

  # T-ISS-16: dose-response within FJF-2026-201 (placebo and the four doses).
  saf_201 <- saf[saf$STUDY == "FJF-2026-201", ]
  measures <- list(
    "Any TEAE" = ae$USUBJID,
    "TEAE leading to discontinuation of study drug" = ae$USUBJID[ae$AEACN %in% "DRUG WITHDRAWN"],
    "Level 2 or 3 hypoglycaemia" = severe$USUBJID
  )
  for (m in names(measures)) {
    for (i in 1:5) {
      who <- saf_201$USUBJID[saf_201$TRT01AN == doses[[i]]]
      add("T-ISS-16", m, m, paste0("arm", i), qc_npct_301(sum(who %in% measures[[m]]), length(who)))
    }
  }

  # T-ISS-17 and L-ISS-1: peaks of the liver tests on treatment, as multiples of the upper limit.
  lb <- both("ADLB", c("USUBJID", "PARAMCD", "AVAL", "ANRHI", "ONTRTFL"))
  lb <- lb[lb$USUBJID %in% saf$USUBJID & lb$ONTRTFL %in% "Y" & !is.na(lb$AVAL) & !is.na(lb$ANRHI) & lb$ANRHI > 0, ]
  peak <- function(test) {
    d <- lb[lb$PARAMCD == test, ]
    vapply(split(d$AVAL / d$ANRHI, d$USUBJID), max, 0)
  }
  alt <- peak("ALT")
  ast <- peak("AST")
  bili <- peak("BILI")
  high_at <- union(names(alt)[alt > 3], names(ast)[ast > 3])
  high_bili <- names(bili)[bili > 2]
  cases <- intersect(high_at, high_bili)
  for (arm in names(doses)) {
    count <- function(ids) qc_npct_301(sum(members[[arm]] %in% ids), size[[arm]])
    add("T-ISS-17", "Peak ALT or AST >3x ULN", "Peak ALT or AST >3x ULN", arm, count(high_at))
    add("T-ISS-17", "Peak total bilirubin >2x ULN", "Peak total bilirubin >2x ULN", arm, count(high_bili))
    both_label <- "Peak ALT or AST >3x ULN and peak total bilirubin >2x ULN"
    add("T-ISS-17", both_label, both_label, arm, count(cases))
  }
  if (length(cases) == 0) add("L-ISS-1", "No records", "No records", "study", "")
  for (id in cases) {
    add("L-ISS-1", id, id, "study", saf$STUDY[saf$USUBJID == id])
    add("L-ISS-1", id, id, "bili", fmt_num(bili[[id]], 2))
  }
  do.call(rbind, found$cells)
}

#' Compare the production tables of the integrated safety analysis with the second programming.
#'
#' @param tables Output of [tables_iss_treatment()].
#' @param expected Output of [qc_tlf_iss_treatment()].
#' @return A list: `cells` (each expected cell with the production value and whether it
#'   matches) and `clean` (TRUE when every expected cell is found as printed).
compare_tlf_iss_treatment <- function(tables, expected) {
  long <- do.call(rbind, lapply(names(tables), function(id) cbind(id = id, long_table_qc_301(tables[[id]]))))
  found <- merge(expected, long[, c("id", "section", "label", "key", "value")],
    by = c("id", "section", "label", "key"), all.x = TRUE, suffixes = c("", "_production")
  )
  found$match <- !is.na(found$value_production) & found$value == found$value_production
  list(cells = found, clean = nrow(found) > 0 && all(found$match))
}
