# Design assumptions of the FJF-2026 programme (docs/design/assumptions.yml).
# Read by the protocols, the SAPs, the sample size and, through its truth ranges, the
# simulator (ADR 0018).

#' Read the design assumptions file.
#'
#' @param path Path to `assumptions.yml`.
#' @return A list with `sources`, `parameters` (a tibble), `design_rules` and
#'   `calibration_targets`.
read_assumptions <- function(path = "docs/design/assumptions.yml") {
  raw <- yaml::read_yaml(path)
  parameters <- tibble::tibble(
    id = purrr::map_chr(raw$parameters, "id"),
    description = purrr::map_chr(raw$parameters, "description"),
    unit = purrr::map_chr(raw$parameters, "unit"),
    planning = purrr::map_dbl(raw$parameters, "planning"),
    truth_low = purrr::map_dbl(raw$parameters, \(p) min(unlist(p$truth_range))),
    truth_high = purrr::map_dbl(raw$parameters, \(p) max(unlist(p$truth_range))),
    sources = purrr::map(raw$parameters, \(p) as.character(unlist(p$sources))),
    evidence = purrr::map_chr(raw$parameters, \(p) p$evidence %||% NA_character_)
  )
  list(
    sources = raw$sources,
    parameters = parameters,
    design_rules = raw$design_rules,
    calibration_targets = raw$calibration_targets
  )
}

#' Check the design assumptions.
#'
#' Every parameter needs a planning value inside its truth range and either at least one
#' known source or `none` with a written justification. Every design rule and calibration
#' target needs a known source, and every source needs a citation and a URL.
#'
#' @param assumptions The result of [read_assumptions()].
#' @return A character vector of problems; empty when the file is complete.
check_assumptions <- function(assumptions) {
  known <- names(assumptions$sources)
  p <- assumptions$parameters
  problems <- character()

  dup <- unique(p$id[duplicated(p$id)])
  if (length(dup) > 0) problems <- c(problems, paste("duplicated parameter:", dup))

  outside <- p$id[p$planning < p$truth_low | p$planning > p$truth_high]
  problems <- c(problems, sprintf("%s: planning value outside its truth range", outside))

  for (i in seq_len(nrow(p))) {
    s <- p$sources[[i]]
    if (identical(s, "none")) {
      if (is.na(p$evidence[i]) || nchar(p$evidence[i]) < 40) {
        problems <- c(problems, sprintf("%s: no source and no justification", p$id[i]))
      }
    } else if (length(s) == 0 || any(!s %in% known)) {
      problems <- c(problems, sprintf("%s: unknown or missing source", p$id[i]))
    }
  }

  for (item in c(assumptions$design_rules, assumptions$calibration_targets)) {
    s <- as.character(unlist(item$sources))
    if (length(s) == 0 || any(!s %in% known)) {
      problems <- c(problems, sprintf("%s: unknown or missing source", item$id))
    }
  }

  for (key in known) {
    src <- assumptions$sources[[key]]
    if (is.null(src$citation) || is.null(src$url)) {
      problems <- c(problems, sprintf("source %s: citation or URL missing", key))
    }
  }
  problems
}
