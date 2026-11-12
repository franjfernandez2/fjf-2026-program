# Data-quality checks of the raw layer (gate-2 criterion 7). They read the raw tables as
# collected and only count and list findings: nothing is corrected here. Cleaning rules are
# written in F3, when the data are mapped to SDTM. The checks never use the treatment arm,
# so they can be read before the database lock.

crf_months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

#' Parse complete CRF dates ("09-MAR-2028"), whatever the system locale.
#'
#' @param x Character vector.
#' @return A Date vector; NA where the date is missing or not complete.
parse_raw_date <- function(x) {
  ok <- !is.na(x) & grepl("^[0-9]{2}-[A-Z]{3}-[0-9]{4}$", x)
  out <- rep(as.Date(NA), length(x))
  if (any(ok)) {
    p <- strsplit(x[ok], "-", fixed = TRUE)
    month <- match(vapply(p, `[`, "", 2), crf_months)
    iso <- sprintf("%s-%02d-%s", vapply(p, `[`, "", 3), month, vapply(p, `[`, "", 1))
    out[ok] <- as.Date(ifelse(is.na(month), NA_character_, iso), optional = TRUE)
  }
  out
}

#' Is a CRF date present but incomplete (for example "UN-MAR-2028")?
#'
#' @param x Character vector.
#' @return Logical vector.
is_partial_date <- function(x) !is.na(x) & is.na(parse_raw_date(x))

finding <- function(study, check, table, n, rows, detail) {
  data.frame(study = study, check = check, table = table, n = n, rows = rows, detail = detail)
}

#' Run the data-quality checks on one study's raw tables.
#'
#' @param tables Named list of data frames, one study of [read_raw_files()].
#' @param spec The result of [read_raw_spec()].
#' @param protocol The study's protocol design ([read_protocol()]), for the visit windows.
#' @param study Study code, for example "301".
#' @return A data frame with one row per check and table: `n` findings among `rows` rows.
check_raw_quality <- function(tables, spec, protocol, study) {
  out <- list()
  for (t in spec$tables) {
    d <- tables[[t$name]]
    key <- unlist(t$natural_key)
    dup <- duplicated(d[key]) | duplicated(d[key], fromLast = TRUE)
    excess <- nrow(d) - nrow(unique(d[key]))
    out[[length(out) + 1]] <- finding(
      study, "Duplicated natural key", t$name, excess, nrow(d),
      sprintf("%d extra rows in %d keys (%s)", excess, nrow(unique(d[dup, key, drop = FALSE])), toString(key))
    )
    for (col in grep("_date$", names(d), value = TRUE)) {
      n <- sum(is_partial_date(d[[col]]))
      out[[length(out) + 1]] <- finding(study, "Partial date", t$name, n, nrow(d), col)
    }
  }

  # Visits outside their protocol window, counted from the randomisation date (day 1 = week 0).
  visits <- tables$visits
  rand <- tables$randomisation
  v <- merge(visits[visits$visit_status == "DONE", ], rand[c("subject_id", "randomisation_date")], by = "subject_id")
  schedule <- data.frame(
    visit_code = protocol$visits$code, week = protocol$visits$week, window = protocol$visits$window_days
  )
  v <- merge(v, schedule, by = "visit_code")
  shift <- as.numeric(parse_raw_date(v$visit_date) - (parse_raw_date(v$randomisation_date) + 7 * v$week))
  outside <- !is.na(shift) & abs(shift) > v$window
  out[[length(out) + 1]] <- finding(
    study, "Visit outside its window", "visits", sum(outside), nrow(v), "scheduled visits of randomised patients"
  )

  # Units: weight in kg, height in cm, and one unit per laboratory test.
  vs <- tables$vital_signs
  out[[length(out) + 1]] <- finding(
    study, "Unexpected unit", "vital_signs", sum(!is.na(vs$weight_unit) & vs$weight_unit != "kg"), nrow(vs),
    "weight not in kg"
  )
  dm <- tables$demography
  out[[length(out) + 1]] <- finding(
    study, "Unexpected unit", "demography", sum(!is.na(dm$height_cm) & dm$height_cm < 3), nrow(dm),
    "height below 3, probably in metres"
  )
  lb <- tables$lab_results
  usual <- tapply(lb$unit, lb$test_code, \(u) names(which.max(table(u))))
  odd <- !is.na(lb$unit) & lb$unit != usual[lb$test_code]
  out[[length(out) + 1]] <- finding(
    study, "Unexpected unit", "lab_results", sum(odd), nrow(lb),
    if (any(odd)) toString(unique(paste(lb$test_code[odd], lb$unit[odd]))) else "every test in its usual unit"
  )
  qualified <- !is.na(lb$result) & is.na(suppressWarnings(as.numeric(lb$result)))
  out[[length(out) + 1]] <- finding(
    study, "Qualified or non-numeric result", "lab_results", sum(qualified), nrow(lb),
    if (any(qualified)) toString(utils::head(unique(lb$result[qualified]), 5)) else "none"
  )

  # Adverse events.
  ae <- tables$adverse_events
  ends_first <- parse_raw_date(ae$end_date) < parse_raw_date(ae$start_date)
  out[[length(out) + 1]] <- finding(
    study, "Ends before it starts", "adverse_events", sum(ends_first, na.rm = TRUE), nrow(ae), "complete dates only"
  )
  out[[length(out) + 1]] <- finding(
    study, "Missing required value", "adverse_events", sum(is.na(ae$severity)), nrow(ae), "severity"
  )

  do.call(rbind, out)
}
