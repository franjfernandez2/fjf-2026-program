# Helpers shared by the ADaM programs of FJF-2026-301 (docs/specs/adam-301.yml).

#' Variables of one dataset of the ADaM specification.
#'
#' @param spec The ADaM specification.
#' @param dataset Dataset name, for example "ADSL".
#' @return The list of variable definitions.
adam_variables <- function(spec, dataset) {
  d <- purrr::keep(spec$datasets, \(x) x$name == dataset)
  if (length(d) != 1) stop("Dataset ", dataset, " is not in the specification exactly once")
  d[[1]]$variables
}

#' Keep a dataset's variables in specification order, with their types.
#'
#' @param data A data frame with at least the dataset's variables.
#' @param spec The ADaM specification.
#' @param dataset Dataset name.
#' @return A tibble with exactly the dataset's variables.
adam_conform <- function(data, spec, dataset) {
  vars <- adam_variables(spec, dataset)
  names <- purrr::map_chr(vars, "name")
  missing <- setdiff(names, names(data))
  if (length(missing) > 0) stop(dataset, " lacks ", toString(missing))
  cast <- list(text = as.character, integer = as.integer, float = as.numeric, date = as.Date)
  out <- as.data.frame(data)[names]
  for (v in vars) out[[v$name]] <- cast[[v$type]](out[[v$name]])
  tibble::as_tibble(out)
}

#' Analysis study day: day 1 is the randomisation date, and there is no day 0 (SAP).
#'
#' @param date,randdt Date vectors.
#' @return Integer vector.
adam_study_day <- function(date, randdt) {
  d <- as.integer(date - randdt)
  ifelse(is.na(d), NA_integer_, d + as.integer(d >= 0))
}

#' Complete SDTM dates (--DTC) as Date; partial or missing dates give NA.
#'
#' @param dtc Character vector of ISO 8601 dates.
#' @return Date vector.
adam_date <- function(dtc) {
  full <- !is.na(dtc) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", dtc)
  out <- rep(as.Date(NA), length(dtc))
  out[full] <- as.Date(substr(dtc[full], 1, 10))
  out
}
