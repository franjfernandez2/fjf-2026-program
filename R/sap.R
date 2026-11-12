# Statistical analysis plans of the FJF-2026 studies (docs/sap/fjf-2026-NNN-sap.yml).

#' rbmi strategies for missing data after a patient leaves the study.
sap_withdrawal_strategies <- c("MAR", "JR", "CR", "CIR", "LMCF")

#' Read a study's SAP file.
#'
#' @param path Path to the YAML file.
#' @return The parsed list.
read_sap <- function(path) yaml::read_yaml(path)

#' One analysis or sensitivity analysis of the SAP, by id.
#'
#' @param sap The result of [read_sap()].
#' @param id Its id, for example "A1" or "S3".
#' @return The list that defines it.
sap_item <- function(sap, id) {
  items <- c(sap$analyses, sap$sensitivity)
  found <- purrr::keep(items, \(x) identical(x$id, id))
  if (length(found) != 1) stop("SAP item ", id, " not found exactly once")
  found[[1]]
}

#' Check a SAP against the study's estimands and protocol.
#'
#' Every analysis names a known estimand and analysis set; the primary estimand has exactly
#' one primary analysis; every supplementary estimand has an analysis; every sensitivity
#' analysis belongs to an existing analysis and uses a known strategy; the reference arm is
#' the protocol's placebo arm.
#'
#' @param sap The result of [read_sap()].
#' @param estimands The result of [read_estimands()].
#' @param protocol The result of [read_protocol()].
#' @return A character vector of problems; empty when consistent.
check_sap <- function(sap, estimands, protocol) {
  found <- new.env()
  found$problems <- character()
  add <- function(...) found$problems <- c(found$problems, sprintf(...))
  est_ids <- purrr::map_chr(estimands$estimands, "id")
  est_roles <- stats::setNames(purrr::map_chr(estimands$estimands, "role"), est_ids)
  sets <- purrr::map_chr(sap$analysis_sets, "id")
  analyses <- sap$analyses
  ids <- c(purrr::map_chr(analyses, "id"), purrr::map_chr(sap$sensitivity, "id"))
  if (anyDuplicated(ids)) add("duplicated analysis ids: %s", paste(unique(ids[duplicated(ids)]), collapse = ", "))

  for (a in analyses) {
    if (!a$estimand %in% est_ids) add("%s: unknown estimand %s", a$id, a$estimand)
    if (!a$analysis_set %in% sets) add("%s: unknown analysis set %s", a$id, a$analysis_set)
    strategy <- a$settings$strategy_after_withdrawal
    if (!is.null(strategy) && !strategy %in% sap_withdrawal_strategies) add("%s: unknown strategy %s", a$id, strategy)
  }
  primary_est <- names(est_roles)[est_roles == "primary"]
  primary_analyses <- purrr::keep(analyses, \(a) a$role == "primary")
  if (length(primary_analyses) != 1 || !identical(primary_analyses[[1]]$estimand, primary_est)) {
    add("the primary estimand %s needs exactly one primary analysis", primary_est)
  }
  analysed <- purrr::map_chr(analyses, "estimand")
  for (e in names(est_roles)[est_roles == "supplementary"]) {
    if (!e %in% analysed) add("supplementary estimand %s has no analysis", e)
  }

  for (s in sap$sensitivity) {
    if (!s$of %in% purrr::map_chr(analyses, "id")) add("%s: belongs to unknown analysis %s", s$id, s$of)
    strategy <- s$settings$strategy_after_withdrawal
    if (is.null(strategy) || !strategy %in% sap_withdrawal_strategies) add("%s: unknown strategy", s$id)
    if (!is.null(s$settings$deltas)) {
      d <- s$settings$deltas
      if (!(d$from == 0 && d$to > d$from && d$by > 0)) add("%s: the delta grid must start at 0 and increase", s$id)
    }
  }

  placebo <- purrr::keep(protocol$arms, \(a) a$dose_mg == 0)
  for (a in purrr::keep(analyses, \(a) !is.null(a$settings$reference))) {
    if (length(placebo) != 1 || a$settings$reference != placebo[[1]]$code) {
      add("%s: the reference arm must be the protocol's placebo arm", a$id)
    }
  }
  found$problems
}

#' Analysis visit windows for HbA1c, derived from the protocol (SAP, visit windows).
#'
#' Boundaries are the midpoints between target days. The last window ends as far after its
#' target as half the gap to the visit before it. Study day 1 is randomisation.
#'
#' @param protocol The result of [read_protocol()].
#' @return A tibble: visit, week, target_day, from_day, to_day.
analysis_windows_301 <- function(protocol) {
  weeks <- hba1c_visit_weeks(protocol)
  target <- weeks * 7 + 1
  mid <- (utils::head(target, -1) + utils::tail(target, -1)) / 2
  last_end <- utils::tail(target, 1) + (utils::tail(target, 1) - utils::tail(target, 2)[1]) / 2
  tibble::tibble(
    visit = sprintf("Week %d", weeks),
    week = weeks,
    target_day = target,
    from_day = c(2, floor(mid) + 1),
    to_day = c(floor(mid), floor(last_end))
  )
}

#' Assign post-baseline assessments to analysis visits, and keep one value per visit.
#'
#' @param assessments A data frame with USUBJID, ADY (study day) and AVAL.
#' @param windows The result of [analysis_windows_301()].
#' @return The assessments inside a window, one per patient and visit: the one closest to
#'   the target day, the later one on a tie. Adds AVISIT, AVISITN and ANL01FL = "Y".
window_assessments <- function(assessments, windows) {
  idx <- findInterval(assessments$ADY, windows$from_day)
  inside <- idx > 0 & assessments$ADY <= windows$to_day[pmax(idx, 1)]
  assessments[inside, ] |>
    dplyr::mutate(
      AVISIT = windows$visit[idx[inside]],
      AVISITN = windows$week[idx[inside]],
      distance = abs(.data$ADY - windows$target_day[idx[inside]])
    ) |>
    dplyr::arrange(.data$USUBJID, .data$AVISITN, .data$distance, dplyr::desc(.data$ADY)) |>
    dplyr::distinct(.data$USUBJID, .data$AVISITN, .keep_all = TRUE) |>
    dplyr::select(-"distance") |>
    dplyr::mutate(ANL01FL = "Y")
}
