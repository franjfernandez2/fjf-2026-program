# Estimands of the FJF-2026 studies (docs/protocols/fjf-2026-NNN-estimands.yml).

#' Strategies for intercurrent events named in ICH E9(R1).
estimand_strategies <- c(
  "treatment policy", "hypothetical", "composite", "while on treatment", "principal stratum"
)

#' Read a study's estimands file.
#'
#' @param path Path to the YAML file.
#' @return The parsed list.
read_estimands <- function(path) yaml::read_yaml(path)

#' Check a study's estimands.
#'
#' Each estimand needs the five ICH E9(R1) attributes, written out, and one strategy from
#' the guideline for every intercurrent event of the study. There must be exactly one
#' primary estimand, and the variable of the primary must name the protocol's primary week.
#'
#' @param estimands The result of [read_estimands()].
#' @param protocol The result of [read_protocol()].
#' @return A character vector of problems; empty when complete.
check_estimands <- function(estimands, protocol) {
  problems <- character()
  events <- purrr::map_chr(estimands$intercurrent_events, "id")
  roles <- purrr::map_chr(estimands$estimands, "role")
  if (sum(roles == "primary") != 1) problems <- c(problems, "there must be exactly one primary estimand")

  for (e in estimands$estimands) {
    for (attr in c("population", "treatment", "variable", "summary")) {
      if (is.null(e[[attr]]) || nchar(trimws(e[[attr]])) < 5) {
        problems <- c(problems, sprintf("%s: attribute '%s' missing", e$id, attr))
      }
    }
    handled <- purrr::map_chr(e$intercurrent_events, "event")
    missing_events <- setdiff(events, handled)
    if (length(missing_events) > 0) {
      problems <- c(problems, sprintf("%s: no strategy for %s", e$id, paste(missing_events, collapse = ", ")))
    }
    unknown <- setdiff(handled, events)
    if (length(unknown) > 0) {
      problems <- c(problems, sprintf("%s: unknown event %s", e$id, paste(unknown, collapse = ", ")))
    }
    strategies <- purrr::map_chr(e$intercurrent_events, "strategy")
    bad <- setdiff(strategies, estimand_strategies)
    if (length(bad) > 0) problems <- c(problems, sprintf("%s: unknown strategy %s", e$id, paste(bad, collapse = ", ")))
    if (e$role == "primary" && !grepl(sprintf("week %g", protocol$primary_visit_week), e$variable)) {
      problems <- c(problems, sprintf("%s: variable does not name the primary week", e$id))
    }
  }
  problems
}

#' One estimand as a two-column table of its attributes.
#'
#' @param estimand One element of `estimands$estimands`.
#' @param events The study's `intercurrent_events`.
#' @return A data frame with columns Attribute and Definition.
estimand_table <- function(estimand, events) {
  labels <- stats::setNames(purrr::map_chr(events, "label"), purrr::map_chr(events, "id"))
  ice <- purrr::map_chr(estimand$intercurrent_events, function(x) {
    sprintf("%s: **%s**. %s", labels[[x$event]], x$strategy, x$rationale)
  })
  data.frame(
    Attribute = c("Population", "Treatment", "Variable", "Intercurrent events", "Population-level summary"),
    Definition = c(
      estimand$population, estimand$treatment, estimand$variable,
      paste(ice, collapse = " "), estimand$summary
    )
  )
}
