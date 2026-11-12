# Data errors of the kinds an EDC system lets through. The raw layer refuses values outside
# a codelist (ADR 0024), so every error here passes the database's checks and has to be
# found by the data review and the SDTM programs of F3.

error_settings <- list(
  partial_ae_start = 0.04, # day unknown: UNK-MAR-2028
  partial_cm_start = 0.10,
  year_only_diagnosis = 0.25, # "2015"
  partial_birth_date = 0.05, # UNK-UNK-1961
  out_of_window = 0.02, # scheduled visits moved outside their window
  duplicate_lab = 0.005, # records sent twice by the laboratory
  duplicate_vitals = 0.003,
  weight_in_lb = 0.01,
  height_in_m = 0.005,
  missing_severity = 0.01,
  ae_end_before_start = 0.005,
  lab_below_limit = c(BILI = 0.2, ALT = 8) # reported as "<0.2", "<8" below these values
)

#' Add the data errors to a study's raw tables.
#'
#' @param tables The result of [simulate_study()].
#' @param seed The seed of the study's data.
#' @param study "201" or "301".
#' @param spec The raw-layer specification.
#' @return A list: `tables` with the errors, and `log`, one row per error introduced (table,
#'   subject, kind). The log is how the simulator's tests check the errors; it is never
#'   written to disk, because finding the errors is the work of F3.
add_data_errors <- function(tables, seed, study, spec) {
  e <- error_settings
  found <- new.env()
  found$log <- list()
  note <- function(table, subject_id, kind) {
    if (length(subject_id) == 0) {
      return(invisible())
    }
    found$log[[length(found$log) + 1]] <- data.frame(table = table, subject_id = subject_id, kind = kind)
  }
  stream <- c(`201` = 12L, `301` = 13L)[[as.character(study)]]
  withr::with_seed(seed_for(seed, stream), {
    pick_rows <- function(n, share) which(stats::runif(n) < share)

    ae <- tables$adverse_events
    r <- pick_rows(nrow(ae), e$partial_ae_start)
    ae$start_date[r] <- sub("^[0-9]{2}", "UNK", ae$start_date[r])
    note("adverse_events", ae$subject_id[r], "partial start date")
    r <- pick_rows(nrow(ae), e$missing_severity)
    ae$severity[r] <- NA
    note("adverse_events", ae$subject_id[r], "missing severity")
    can <- which(!is.na(ae$end_date) & !grepl("UNK", ae$start_date))
    r <- can[stats::runif(length(can)) < e$ae_end_before_start]
    start <- parse_crf_date(ae$start_date[r])
    ae$end_date[r] <- crf_date(start - sample(1:10, length(r), replace = TRUE))
    note("adverse_events", ae$subject_id[r], "end before start")
    tables$adverse_events <- ae

    cm <- tables$concomitant_meds
    r <- pick_rows(nrow(cm), e$partial_cm_start)
    cm$start_date[r] <- sub("^[0-9]{2}", "UNK", cm$start_date[r])
    note("concomitant_meds", cm$subject_id[r], "partial start date")
    tables$concomitant_meds <- cm

    dm <- tables$demography
    r <- pick_rows(nrow(dm), e$year_only_diagnosis)
    dm$diabetes_diagnosis_date[r] <- substr(dm$diabetes_diagnosis_date[r], 8, 11)
    note("demography", dm$subject_id[r], "year-only diagnosis date")
    r <- pick_rows(nrow(dm), e$partial_birth_date)
    dm$birth_date[r] <- paste0("UNK-UNK-", substr(dm$birth_date[r], 8, 11))
    note("demography", dm$subject_id[r], "partial birth date")
    r <- pick_rows(nrow(dm), e$height_in_m)
    dm$height_cm[r] <- round(dm$height_cm[r] / 100, 2)
    note("demography", dm$subject_id[r], "height in metres")
    tables$demography <- dm

    # Visits outside their window: the visit and everything recorded at it move together.
    v <- tables$visits
    sched <- which(v$visit_status == "DONE" & !v$visit_code %in% c("V1", "V2", "UNS"))
    r <- sched[stats::runif(length(sched)) < e$out_of_window]
    shift <- sample(c(-1, 1), length(r), replace = TRUE) * sample(9:20, length(r), replace = TRUE)
    old <- v$visit_date[r]
    new <- crf_date(parse_crf_date(old) + shift)
    v$visit_date[r] <- new
    key <- paste(v$subject_id[r], v$visit_code[r])
    move <- function(tab, date_col) {
      d <- tables[[tab]]
      k <- match(paste(d$subject_id, d$visit_code), key)
      hit <- !is.na(k)
      hit[hit] <- d[[date_col]][hit] == old[k[hit]]
      d[[date_col]][hit] <- new[k[hit]]
      d
    }
    tables$visits <- v
    tables$lab_results <- move("lab_results", "collection_date")
    tables$vital_signs <- move("vital_signs", "measure_date")
    note("visits", v$subject_id[r], "visit out of window")

    vs <- tables$vital_signs
    can <- which(!is.na(vs$weight))
    r <- can[stats::runif(length(can)) < e$weight_in_lb]
    vs$weight[r] <- round(vs$weight[r] * 2.20462, 1)
    vs$weight_unit[r] <- "lb"
    note("vital_signs", vs$subject_id[r], "weight in lb")
    d <- pick_rows(nrow(vs), e$duplicate_vitals)
    note("vital_signs", vs$subject_id[d], "duplicate record")
    vs <- vs[sort(c(seq_len(nrow(vs)), d)), ]
    tables$vital_signs <- vs

    lab <- tables$lab_results
    for (test in names(e$lab_below_limit)) {
      lim <- e$lab_below_limit[[test]]
      r <- which(lab$test_code == test & suppressWarnings(as.numeric(lab$result)) < lim)
      lab$result[r] <- paste0("<", lim)
    }
    d <- pick_rows(nrow(lab), e$duplicate_lab)
    note("lab_results", lab$subject_id[d], "duplicate record")
    lab <- lab[sort(c(seq_len(nrow(lab)), d)), ]
    tables$lab_results <- lab
  })
  # Record ids are the EDC's own: renumbered after the duplicates were added.
  for (t in names(tables)) tables[[t]] <- as_raw_table(tables[[t]][names(tables[[t]]) != "record_id"], spec, t)
  list(tables = tables, log = do.call(rbind, found$log))
}
