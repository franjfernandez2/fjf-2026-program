# The public design files the simulator reads: assumptions, protocols and the raw-layer
# specification. The simulator uses no code from the analysis side (R/).

#' Read the design files of the programme.
#'
#' @param root Root of the repository.
#' @return A list: `assumptions` (with `parameters` as a data frame), `protocols` (named
#'   "201" and "301") and `raw_spec`.
read_design <- function(root) {
  a <- yaml::read_yaml(file.path(root, "docs", "design", "assumptions.yml"))
  a$parameters <- data.frame(
    id = purrr::map_chr(a$parameters, "id"),
    planning = purrr::map_dbl(a$parameters, "planning"),
    truth_low = purrr::map_dbl(a$parameters, \(x) min(unlist(x$truth_range))),
    truth_high = purrr::map_dbl(a$parameters, \(x) max(unlist(x$truth_range)))
  )
  protocol <- function(study) yaml::read_yaml(file.path(root, "docs", "protocols", sprintf("fjf-2026-%s.yml", study)))
  list(
    assumptions = a,
    protocols = list(`201` = protocol("201"), `301` = protocol("301")),
    raw_spec = yaml::read_yaml(file.path(root, "docs", "specs", "raw-layer.yml"))
  )
}

#' Visits of a protocol as a data frame.
#'
#' @param protocol A parsed protocol file.
#' @return A data frame: code, week, window_days, and one logical column per assessment.
protocol_visits <- function(protocol) {
  v <- protocol$visits
  out <- data.frame(
    code = purrr::map_chr(v, "code"),
    week = purrr::map_dbl(v, "week"),
    window_days = purrr::map_dbl(v, "window_days")
  )
  out$assessments <- purrr::map(v, \(x) as.character(unlist(x$assessments)))
  out
}

#' Rescue threshold of fasting plasma glucose at a week (NA where no rule applies).
#'
#' @param protocol A parsed protocol file.
#' @param week Week since randomisation.
#' @return Threshold in mg/dL.
fpg_threshold <- function(protocol, week) {
  th <- protocol$rescue$thresholds
  vapply(week, function(w) {
    for (i in seq_along(th)) {
      last <- i == length(th)
      if (w >= th[[i]]$from_week && (w < th[[i]]$to_week || (last && w <= th[[i]]$to_week))) {
        return(th[[i]]$fpg_mg_dl)
      }
    }
    NA_real_
  }, numeric(1))
}
