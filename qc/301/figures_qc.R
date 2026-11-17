# Second programming of the figures (independent of studies/301/tlf/ and studies/201/tlf/).
#
# A figure is a picture, so what is compared is the data it plots: the production program writes
# them next to the PNG (write_figure_data_301()) and each figure's values are recomputed here with
# base R from the ADaM datasets or the analysis results and compared point by point, with a
# relative tolerance of 1e-8 (the data file keeps 15 digits). The number of points must agree.

#' Compare the data a figure plots with the expected values.
#'
#' @param id Output id.
#' @param expected Data frame of expected values.
#' @param found The data written by the production program.
#' @param keys Columns that identify a point.
#' @param tol Relative tolerance of the numeric columns.
#' @return A data frame in the form of the cells of [compare_tlf_301()].
qc_figure_compare <- function(id, expected, found, keys, tol = 1e-8) {
  values <- setdiff(names(expected), keys)
  merged <- merge(expected, found[, c(keys, values), drop = FALSE], by = keys, all.x = TRUE, suffixes = c("", ".prod"))
  where <- do.call(paste, c(lapply(merged[keys], as.character), sep = " | "))
  cells <- lapply(values, function(v) {
    e <- merged[[v]]
    p <- merged[[paste0(v, ".prod")]]
    ok <- if (is.numeric(e)) {
      (is.na(e) & is.na(p)) | (!is.na(e) & !is.na(p) & abs(e - p) <= tol * pmax(1, abs(e)))
    } else {
      as.character(e) == as.character(p)
    }
    ok[is.na(ok)] <- FALSE
    data.frame(
      id = id, section = where, label = v, column = "", value = as.character(e),
      value_production = as.character(p), match = ok
    )
  })
  count <- data.frame(
    id = id, section = "Points plotted", label = "Number of points", column = "", value = as.character(nrow(expected)),
    value_production = as.character(nrow(found)), match = nrow(expected) == nrow(found)
  )
  rbind(do.call(rbind, cells), count)
}

#' The data file next to a figure of a list of written files.
#'
#' @param files Paths written by the study's output program.
#' @param id Output id, e.g. "F-14.2.2".
#' @return The data as read from the .csv file.
qc_figure_found <- function(files, id) {
  name <- paste0(tolower(gsub("[-.]", "_", id)), ".csv")
  path <- sub("[.]png$", ".csv", files[basename(files) == sub("[.]csv$", ".png", name)])
  if (length(path) != 1 || !file.exists(path)) stop("no data file written for ", id)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Product-limit curve: the proportion without an event at each time.
#'
#' @param time Times.
#' @param event 1 for an event, 0 for censored.
#' @return A data frame: time (0 and every observed time), surv.
qc_km_curve <- function(time, event) {
  times <- sort(unique(time))
  surv <- numeric(length(times))
  current <- 1
  for (i in seq_along(times)) {
    at_risk <- sum(time >= times[i])
    events <- sum(time == times[i] & event == 1)
    current <- current * (1 - events / at_risk)
    surv[i] <- current
  }
  data.frame(time = c(0, times), surv = c(1, surv))
}

#' Expected points of a Kaplan-Meier figure.
#'
#' @param d ADTTE records.
#' @param arm Arm label of each record.
#' @param incidence Whether the curve is the cumulative incidence (otherwise the survival).
qc_km_points <- function(d, arm, incidence) {
  do.call(rbind, lapply(unique(arm), function(a) {
    k <- qc_km_curve(d$AVAL[arm == a], 1 - d$CNSR[arm == a])
    data.frame(arm = a, time = k$time, value = if (incidence) 1 - k$surv else k$surv)
  }))
}

#' Peak ALT and total bilirubin on treatment of each patient, as multiples of the ULN (base R).
#'
#' @param adlb ADLB.
#' @return A data frame: USUBJID, ALT, BILI (patients with both).
qc_edish_points <- function(adlb) {
  lb <- adlb[adlb$SAFFL == "Y" & adlb$ONTRTFL %in% "Y" & adlb$PARAMCD %in% c("ALT", "BILI"), ]
  lb <- lb[!is.na(lb$AVAL) & !is.na(lb$ANRHI), ]
  lb$x <- lb$AVAL / lb$ANRHI
  peak <- function(code) {
    d <- lb[lb$PARAMCD == code, ]
    data.frame(USUBJID = names(split(d$x, d$USUBJID)), v = vapply(split(d$x, d$USUBJID), max, 0), row.names = NULL)
  }
  both <- merge(peak("ALT"), peak("BILI"), by = "USUBJID", suffixes = c("_alt", "_bili"))
  data.frame(USUBJID = both$USUBJID, ALT = both$v_alt, BILI = both$v_bili)
}

#' Comparison of the figures of FJF-2026-301.
#'
#' @param files Paths written by [write_tlf_301()].
#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results, as for [tables_301()].
#' @return A list: `cells` and `clean`, as [compare_tlf_301()].
qc_figures_301 <- function(files, adam, analysis) {
  adsl <- adam$ADSL
  fas <- adsl[adsl$FASFL == "Y", ]
  cells <- list()

  # F-14.2.1: the tipping-point analysis.
  tip <- analysis$efficacy$tipping
  cells[[1]] <- qc_figure_compare(
    "F-14.2.1", data.frame(delta = tip$delta, estimate = tip$estimate, lower = tip$lower, upper = tip$upper),
    qc_figure_found(files, "F-14.2.1"), "delta"
  )

  # F-14.2.2: mean change in HbA1c by arm and visit, observed values.
  e <- adam$ADEFF
  e <- e[e$FASFL == "Y" & e$PARAMCD == "HBA1C" & e$ANL01FL %in% "Y" & is.na(e$DTYPE), ]
  e <- e[!is.na(e$CHG) & e$AVISIT != "Baseline", ]
  groups <- unique(e[c("TRTP", "AVISITN")])
  means <- do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
    x <- e$CHG[e$TRTP == groups$TRTP[i] & e$AVISITN == groups$AVISITN[i]]
    data.frame(
      TRTP = groups$TRTP[i], AVISITN = groups$AVISITN[i], mean = mean(x), se = stats::sd(x) / sqrt(length(x)),
      n = length(x)
    )
  }))
  cells[[2]] <- qc_figure_compare("F-14.2.2", means, qc_figure_found(files, "F-14.2.2"), c("TRTP", "AVISITN"))

  # F-14.2.3 and F-14.3.1: Kaplan-Meier curves.
  rescue <- adam$ADTTE[adam$ADTTE$PARAMCD == "TTRESC" & adam$ADTTE$FASFL == "Y", ]
  cells[[3]] <- qc_figure_compare(
    "F-14.2.3", qc_km_points(rescue, rescue$TRTP, FALSE), qc_figure_found(files, "F-14.2.3"), c("arm", "time")
  )
  cells[[4]] <- qc_km_hypo(files, adam, "F-14.3.1")

  # F-14.2.4: treatment effect by subgroup.
  sg <- analysis$secondary$subgroups
  cells[[5]] <- qc_figure_compare(
    "F-14.2.4",
    data.frame(
      subgroup = sg$subgroup, level = as.character(sg$level), n = sg$n, estimate = sg$estimate, lower = sg$lower,
      upper = sg$upper
    ),
    qc_figure_found(files, "F-14.2.4")[c("subgroup", "level", "n", "estimate", "lower", "upper")],
    c("subgroup", "level")
  )

  # F-14.3.2: peak ALT against peak bilirubin.
  cells[[6]] <- qc_figure_compare(
    "F-14.3.2", qc_edish_points(adam$ADLB), qc_figure_found(files, "F-14.3.2"), "USUBJID"
  )
  cells <- do.call(rbind, cells)
  list(cells = cells, clean = nrow(cells) > 0 && all(cells$match))
}

#' Comparison of the Kaplan-Meier figure of the time to a level 2 or 3 hypoglycaemia.
#' @param files Paths written by the study's output program.
#' @param adam Named list of the ADaM datasets.
#' @param id Output id.
qc_km_hypo <- function(files, adam, id) {
  d <- adam$ADTTE[adam$ADTTE$PARAMCD == "TTHYPO23" & adam$ADTTE$SAFFL %in% "Y", ]
  qc_figure_compare(id, qc_km_points(d, d$TRTA, TRUE), qc_figure_found(files, id), c("arm", "time"))
}

#' Comparison of the figures of FJF-2026-201.
#'
#' @param files Paths written by the study's output program.
#' @param adam Named list of the ADaM datasets.
#' @param analysis Analysis results, as for [tables_201()].
#' @return A list: `cells` and `clean`, as [compare_tlf_301()].
qc_figures_201 <- function(files, adam, analysis) {
  m <- analysis$mcpmod$means
  placebo <- which(m$doses == 0)
  diff_se <- sqrt(diag(m$S) + m$S[placebo, placebo] - 2 * m$S[, placebo])
  diff <- unname(m$means - m$means[placebo])
  z <- 1.959964
  expected <- data.frame(
    dose = m$doses, mean = unname(m$means), se = sqrt(diag(m$S)), diff = ifelse(m$doses == 0, 0, diff),
    diff_se = ifelse(m$doses == 0, NA, diff_se), lower = ifelse(m$doses == 0, NA, diff - z * diff_se),
    upper = ifelse(m$doses == 0, NA, diff + z * diff_se)
  )
  cells <- rbind(
    qc_figure_compare("F-14.2.1", expected, qc_figure_found(files, "F-14.2.1"), "dose", tol = 1e-5),
    qc_km_hypo(files, adam, "F-14.3.1"),
    qc_figure_compare("F-14.3.2", qc_edish_points(adam$ADLB), qc_figure_found(files, "F-14.3.2"), "USUBJID")
  )
  list(cells = cells, clean = nrow(cells) > 0 && all(cells$match))
}

#' Put comparisons together, as one list of cells (the output id and whether each cell matches).
#'
#' @param ... Comparisons (lists with `cells` and `clean`).
combine_comparisons <- function(...) {
  parts <- list(...)
  cells <- do.call(rbind, lapply(parts, function(p) p$cells[, c("id", "match")]))
  list(cells = cells, clean = all(vapply(parts, function(p) p$clean, TRUE)))
}
