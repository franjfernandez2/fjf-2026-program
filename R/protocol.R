# Design data of the FJF-2026 studies (docs/protocols/fjf-2026-NNN.yml), read by the
# protocol synopsis, the SAP and the simulator.

#' Read a study's design data.
#'
#' @param path Path to the study's YAML file.
#' @return The parsed list, with `visits` as a tibble.
read_protocol <- function(path) {
  raw <- yaml::read_yaml(path)
  raw$visits <- tibble::tibble(
    code = purrr::map_chr(raw$visits, "code"),
    label = purrr::map_chr(raw$visits, "label"),
    week = purrr::map_dbl(raw$visits, "week"),
    window_days = purrr::map_dbl(raw$visits, "window_days"),
    assessments = purrr::map(raw$visits, \(v) as.character(unlist(v$assessments)))
  )
  raw
}

#' Rescue threshold for fasting plasma glucose at a given week.
#'
#' @param protocol The result of [read_protocol()].
#' @param week Week since randomisation (numeric vector).
#' @return The threshold in mg/dL, or `NA` where no rescue rule applies.
rescue_threshold <- function(protocol, week) {
  th <- protocol$rescue$thresholds
  last <- length(th)
  vapply(week, function(w) {
    for (i in seq_len(last)) {
      upper_ok <- if (i == last) w <= th[[i]]$to_week else w < th[[i]]$to_week
      if (w >= th[[i]]$from_week && upper_ok) {
        return(as.numeric(th[[i]]$fpg_mg_dl))
      }
    }
    NA_real_
  }, numeric(1))
}

#' Check a study's design data for internal consistency.
#'
#' @param protocol The result of [read_protocol()].
#' @param assumptions The result of [read_assumptions()].
#' @return A character vector of problems; empty when consistent.
check_protocol <- function(protocol, assumptions) {
  v <- protocol$visits
  problems <- character()
  add <- function(cond, msg) if (isTRUE(cond)) problems <<- c(problems, msg)

  add(anyDuplicated(v$code) > 0, "visit codes are not unique")
  add(is.unsorted(v$week, strictly = TRUE), "visit weeks are not strictly increasing")
  start <- 7 * v$week - v$window_days
  end <- 7 * v$week + v$window_days
  add(any(utils::head(end, -1) >= utils::tail(start, -1)), "visit windows overlap")

  used <- unique(unlist(v$assessments))
  undefined <- setdiff(used, names(protocol$assessments))
  add(length(undefined) > 0, paste("undefined assessments:", paste(undefined, collapse = ", ")))

  has <- function(week, code) any(v$week == week & purrr::map_lgl(v$assessments, \(a) code %in% a))
  add(protocol$primary_visit_week != protocol$treatment_weeks, "primary visit is not the end of treatment")
  add(!has(0, "hba1c"), "no baseline HbA1c at week 0")
  add(!has(protocol$primary_visit_week, "hba1c"), "no HbA1c at the primary visit")

  th <- protocol$rescue$thresholds
  from <- purrr::map_dbl(th, "from_week")
  to <- purrr::map_dbl(th, "to_week")
  fpg <- purrr::map_dbl(th, "fpg_mg_dl")
  add(any(utils::tail(from, -1) != utils::head(to, -1)), "rescue periods are not contiguous")
  add(max(to) != protocol$treatment_weeks, "rescue periods do not reach the end of treatment")
  add(is.unsorted(rev(fpg)), "rescue thresholds increase over time")
  on_rescue <- v$week >= min(from) & v$week <= max(to)
  no_fpg <- v$code[on_rescue & !purrr::map_lgl(v$assessments, \(a) "fpg" %in% a)]
  add(length(no_fpg) > 0, paste("no FPG at rescue visits:", paste(no_fpg, collapse = ", ")))

  doses <- purrr::map_dbl(protocol$arms, "dose_mg")
  add(sum(doses == 0) != 1, "there must be exactly one placebo arm")
  add(length(protocol$randomisation$ratio) != length(protocol$arms), "randomisation ratio does not match the arms")

  p <- assumptions$parameters
  baseline <- p$planning[p$id == "hba1c_baseline_mean"]
  range <- unlist(protocol$eligibility$hba1c_screening_pct)
  add(baseline < range[1] || baseline > range[2], "planning baseline HbA1c outside the eligibility range")
  problems
}
