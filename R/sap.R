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
#' @param id Its id, for example "A1", "S3", "B2" or "SF1".
#' @return The list that defines it.
sap_item <- function(sap, id) {
  items <- c(sap$analyses, sap$sensitivity, sap$secondary, sap$safety)
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
  ids <- unlist(lapply(c(analyses, sap$sensitivity, sap$secondary, sap$safety), `[[`, "id"))
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

  # Key secondary analyses: one per key secondary estimand.
  directions <- c("lower", "higher")
  for (b in sap$secondary) {
    if (!b$estimand %in% est_ids) add("%s: unknown estimand %s", b$id, b$estimand)
    if (!b$analysis_set %in% sets) add("%s: unknown analysis set %s", b$id, b$analysis_set)
    if (!isTRUE(b$favourable %in% directions)) add("%s: favourable must be lower or higher", b$id)
  }
  for (e in names(est_roles)[est_roles == "key secondary"]) {
    if (!e %in% purrr::map_chr(sap$secondary, "estimand")) add("key secondary estimand %s has no analysis", e)
  }
  problems_graph <- check_multiplicity(sap)
  if (length(problems_graph) > 0) found$problems <- c(found$problems, problems_graph)
  for (g in sap$subgroups$variables) {
    if (length(g$levels) != 2) add("subgroup %s must have two levels", g$id)
  }

  placebo <- purrr::keep(protocol$arms, \(a) a$dose_mg == 0)
  for (a in purrr::keep(analyses, \(a) !is.null(a$settings$reference))) {
    if (length(placebo) != 1 || a$settings$reference != placebo[[1]]$code) {
      add("%s: the reference arm must be the protocol's placebo arm", a$id)
    }
  }
  found$problems
}

#' Check the multiplicity graph of a SAP.
#'
#' The graph must test the primary analysis and every key secondary analysis; all the
#' initial weight sits on the primary, so nothing else is tested unless it is rejected;
#' transition weights lie in [0, 1], with no self-loops and no row above 1.
#'
#' @param sap The result of [read_sap()].
#' @return A character vector of problems.
check_multiplicity <- function(sap) {
  m <- sap$multiplicity
  if (is.null(m)) {
    return("the SAP has no multiplicity section")
  }
  problems <- character()
  ids <- purrr::map_chr(m$hypotheses, "id")
  analyses <- purrr::map_chr(m$hypotheses, "analysis")
  weights <- purrr::map_dbl(m$hypotheses, "weight")
  primary <- purrr::keep(sap$analyses, \(a) a$role == "primary")
  primary_id <- if (length(primary) == 1) primary[[1]]$id else NA
  needed <- c(primary_id, purrr::map_chr(sap$secondary, "id"))
  missing <- setdiff(needed, analyses)
  if (length(missing) > 0) problems <- c(problems, sprintf("graph does not test %s", paste(missing, collapse = ", ")))
  if (!isTRUE(all.equal(sum(weights), 1)) || !isTRUE(all.equal(weights[analyses == primary_id], 1))) {
    problems <- c(problems, "all the initial weight must be on the primary hypothesis")
  }
  if (!setequal(names(m$transitions), ids)) {
    problems <- c(problems, "transitions must have one row per hypothesis")
  } else {
    tm <- do.call(rbind, lapply(ids, \(h) as.numeric(unlist(m$transitions[[h]]))))
    if (ncol(tm) != length(ids) || any(tm < 0 | tm > 1) || any(diag(tm) != 0) || any(rowSums(tm) > 1 + 1e-9)) {
      problems <- c(problems, "transition weights must be in [0, 1], without self-loops, rows summing to at most 1")
    }
  }
  if (!isTRUE(m$alpha_one_sided > 0 && m$alpha_one_sided <= 0.025)) {
    problems <- c(problems, "one-sided alpha must be in (0, 0.025]")
  }
  problems
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

#' Candidate dose-response models the 201 code can build.
sap_201_models <- c("linear", "emax", "sigEmax", "quadratic", "exponential")

#' Check the FJF-2026-201 SAP against its estimands and protocol.
#'
#' The primary analysis answers the primary estimand; the candidate models are known, with
#' guesstimates inside the dose range; the MCP level, direction and target-dose rule are
#' valid; the study has placebo and at least three doses; every sensitivity analysis belongs
#' to the primary analysis, and model averaging has a bootstrap size and a seed.
#'
#' @param sap The result of [read_sap()].
#' @param estimands The result of [read_estimands()].
#' @param protocol The result of [read_protocol()].
#' @return A character vector of problems; empty when consistent.
check_sap_201 <- function(sap, estimands, protocol) {
  found <- new.env()
  found$problems <- character()
  add <- function(cond, msg) if (isTRUE(cond)) found$problems <- c(found$problems, msg)
  primary <- purrr::keep(estimands$estimands, \(e) e$role == "primary")
  add(length(primary) != 1 || !identical(sap$primary$estimand, primary[[1]]$id), "A1 must answer the primary estimand")
  doses <- purrr::map_dbl(protocol$arms, "dose_mg")
  add(sum(doses == 0) != 1 || sum(doses > 0) < 3, "MCP-Mod needs placebo and at least three doses")
  mcp <- sap$primary$mcp
  add(!isTRUE(mcp$alpha_one_sided > 0 && mcp$alpha_one_sided <= 0.025), "one-sided alpha must be in (0, 0.025]")
  add(!isTRUE(mcp$direction %in% c("increasing", "decreasing")), "direction must be increasing or decreasing")
  models <- purrr::map_chr(mcp$candidate_models, "model")
  add(length(models) < 2 || anyDuplicated(models) > 0, "at least two distinct candidate models are needed")
  unknown <- setdiff(models, sap_201_models)
  add(length(unknown) > 0, paste("unknown candidate model:", paste(unknown, collapse = ", ")))
  for (m in mcp$candidate_models) {
    add(!is.null(m$ed50) && !(m$ed50 > 0 && m$ed50 <= max(doses)), sprintf("%s: ED50 outside the dose range", m$model))
  }
  td <- sap$primary$mod$target_dose
  add(!isTRUE(td$p > 0 && td$p < 1), "target dose share p must be in (0, 1)")
  add(!isTRUE(td$clinical_relevance > 0), "clinical relevance must be positive")
  for (s in sap$sensitivity) add(!identical(s$of, sap$primary$id), sprintf("%s: belongs to unknown analysis", s$id))
  s1 <- purrr::keep(sap$sensitivity, \(x) x$id == "S1")
  add(
    length(s1) != 1 || is.null(s1[[1]]$settings$bootstrap) || is.null(s1[[1]]$settings$seed),
    "S1 (model averaging) needs a bootstrap size and a seed"
  )
  found$problems
}
