# Helpers shared by the SDTM programs of FJF-2026-301 (docs/specs/sdtm-301.yml).

#' CRF dates ("09-MAR-2028", "UNK-MAR-2028", "2023") to ISO 8601, partial dates kept partial.
#'
#' @param x Character vector of CRF dates.
#' @return Character vector of ISO 8601 dates (CR02: no imputation in SDTM).
crf_to_iso <- function(x) {
  x <- as.character(x)
  out <- as.character(sdtm.oak::create_iso8601(x, .format = list(c("dd-mmm-y", "y")), .na = c("UN", "UNK")))
  out[is.na(x)] <- NA_character_
  out
}

#' Unique subject identifier.
#'
#' @param studyid Study identifier.
#' @param subject_id Subject identifier of the CRF.
#' @return Character vector.
sdtm_usubjid <- function(studyid, subject_id) paste(studyid, subject_id, sep = "-")

#' Complete ISO 8601 dates as Date; partial or missing dates give NA.
#'
#' @param dtc Character vector of ISO 8601 dates.
#' @return Date vector.
iso_date <- function(dtc) {
  out <- rep(as.Date(NA), length(dtc))
  full <- !is.na(dtc) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", dtc)
  out[full] <- as.Date(substr(dtc[full], 1, 10))
  out
}

#' SDTM study day: day 1 is the reference date, and there is no day 0.
#'
#' @param dtc,refdtc ISO 8601 dates of the observation and of the reference (RFSTDTC).
#' @return Integer vector; NA when either date is not complete.
sdtm_study_day <- function(dtc, refdtc) {
  d <- as.integer(iso_date(dtc) - iso_date(refdtc))
  ifelse(is.na(d), NA_integer_, d + as.integer(d >= 0))
}

#' Partial birth or diagnosis date imputed to the middle of the period (CR02), for age and
#' duration only.
#'
#' @param dtc ISO 8601 date, complete or partial.
#' @return Date vector.
impute_mid_period <- function(dtc) {
  out <- iso_date(dtc)
  ym <- is.na(out) & !is.na(dtc) & grepl("^[0-9]{4}-[0-9]{2}$", dtc)
  y <- is.na(out) & !is.na(dtc) & grepl("^[0-9]{4}$", dtc)
  # paste0() on an empty vector returns the suffix alone, so each step is guarded.
  if (any(ym)) out[ym] <- as.Date(paste0(dtc[ym], "-15"))
  if (any(y)) out[y] <- as.Date(paste0(dtc[y], "-07-01"))
  out
}

#' Whole years between two dates.
#'
#' @param from,to Date vectors.
#' @return Integer vector.
whole_years <- function(from, to) {
  years <- as.integer(format(to, "%Y")) - as.integer(format(from, "%Y"))
  before_birthday <- format(to, "%m%d") < format(from, "%m%d")
  as.integer(years - before_birthday)
}

#' Variables of one domain of the specification.
#'
#' @param spec The SDTM specification.
#' @param domain Domain name, for example "DM".
#' @return The list of variable definitions.
sdtm_variables <- function(spec, domain) {
  d <- purrr::keep(spec$domains, \(x) x$name == domain)
  if (length(d) != 1) stop("Domain ", domain, " is not in the specification exactly once")
  d[[1]]$variables
}

#' Keep a domain's variables in specification order, with their types.
#'
#' @param data A data frame with at least the domain's variables.
#' @param spec The SDTM specification.
#' @param domain Domain name.
#' @return A tibble with exactly the domain's variables.
sdtm_conform <- function(data, spec, domain) {
  vars <- sdtm_variables(spec, domain)
  names <- purrr::map_chr(vars, "name")
  missing <- setdiff(names, names(data))
  if (length(missing) > 0) stop(domain, " lacks ", toString(missing))
  cast <- list(text = as.character, integer = as.integer, float = as.numeric)
  out <- data[names]
  for (v in vars) out[[v$name]] <- cast[[v$type]](out[[v$name]])
  tibble::as_tibble(out)
}

#' Code verbatim terms with the project's reduced dictionary.
#'
#' @param verbatim Character vector of terms as collected.
#' @param entries Dictionary section (`adverse_events` or `medications`).
#' @param what Name of the dictionary section, for the error message.
#' @return A data frame with one row per term and the entry's fields; stops if a term is not
#'   in the dictionary, as an uncoded term would raise a query.
code_terms <- function(verbatim, entries, what) {
  key <- function(x) tolower(trimws(x))
  lookup <- purrr::list_rbind(purrr::map(entries, \(e) {
    fields <- e[setdiff(names(e), "verbatim")]
    data.frame(key = key(unlist(e$verbatim)), fields)
  }))
  if (anyDuplicated(lookup$key) > 0) stop("Duplicated verbatim terms in the ", what, " dictionary")
  i <- match(key(verbatim), lookup$key)
  uncoded <- unique(verbatim[is.na(i) & !is.na(verbatim)])
  if (length(uncoded) > 0) stop("Uncoded ", what, ": ", toString(uncoded))
  lookup[i, setdiff(names(lookup), "key"), drop = FALSE]
}

#' Visit number and name of planned visits.
#'
#' @param code Visit codes of the CRF.
#' @param protocol The protocol design.
#' @return A list with `VISITNUM` and `VISIT`.
planned_visit <- function(code, protocol) {
  i <- match(code, protocol$visits$code)
  list(VISITNUM = as.numeric(i), VISIT = toupper(protocol$visits$label[i]))
}

#' Sequence number within subject, in the order given.
#'
#' @param usubjid Subject identifiers, already in the wanted order.
#' @return Integer vector.
sdtm_seq <- function(usubjid) as.integer(stats::ave(seq_along(usubjid), usubjid, FUN = seq_along))

#' Keep the first entry of rows that share a natural key (CR01).
#'
#' @param d A raw table with `record_id`.
#' @param key Columns of the natural key.
#' @param table Table name, for the issues listing.
#' @return A list with `data` (one row per key, lowest record_id) and `issues` (one row per
#'   dropped duplicate, saying whether its values differ from the kept one).
keep_first_entry <- function(d, key, table) {
  d <- d[order(d$record_id), ]
  drop <- duplicated(d[key])
  kept <- d[!drop, ]
  dropped <- d[drop, ]
  values <- setdiff(names(d), c("record_id", key))
  k <- match(do.call(paste, dropped[key]), do.call(paste, kept[key]))
  differs <- vapply(seq_len(nrow(dropped)), \(i) {
    !identical(unlist(dropped[i, values]), unlist(kept[k[i], values]))
  }, logical(1))
  issues <- data.frame(
    rule = rep("CR01", nrow(dropped)), table = rep(table, nrow(dropped)),
    subject_id = dropped$subject_id, record_id = dropped$record_id,
    detail = sprintf(
      "duplicate of record %s, %s; first entry kept", kept$record_id[k],
      ifelse(differs, "with different values", "identical")
    )
  )
  list(data = kept, issues = issues)
}

#' Last non-missing observation on or before the first dose, per subject and test.
#'
#' @param usubjid,testcd,dtc Subject, test and ISO 8601 date of each record.
#' @param result Numeric result; missing results are not flagged.
#' @param dm The DM dataset, for RFXSTDTC.
#' @return "Y" or NA for each record.
last_before_exposure <- function(usubjid, testcd, dtc, result, dm) {
  first_dose <- iso_date(dm$RFXSTDTC[match(usubjid, dm$USUBJID)])
  date <- iso_date(dtc)
  ok <- !is.na(result) & !is.na(date) & !is.na(first_dose) & date <= first_dose
  flag <- rep(NA_character_, length(usubjid))
  if (!any(ok)) {
    return(flag)
  }
  idx <- which(ok)
  idx <- idx[order(usubjid[idx], testcd[idx], date[idx])]
  last <- idx[!duplicated(paste(usubjid[idx], testcd[idx]), fromLast = TRUE)]
  flag[last] <- "Y"
  flag
}

#' Natural key of a raw table.
#'
#' @param raw_spec The raw layer specification ([read_raw_spec()]).
#' @param table Table name.
#' @return Character vector of column names.
natural_key <- function(raw_spec, table) {
  t <- purrr::keep(raw_spec$tables, \(x) x$name == table)
  unlist(t[[1]]$natural_key)
}
