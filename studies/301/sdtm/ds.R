# SDTM DS of FJF-2026-301: protocol milestones (consent, randomisation) and disposition
# events (screen failure, end of treatment, end of study).

#' Map the raw tables of FJF-2026-301 to DS.
#'
#' @param raw Named list of the study's raw tables.
#' @param assignment Result of [assignment_in_use()].
#' @param dm The DM dataset ([map_dm_301()]), for the reference start date.
#' @param spec The SDTM specification.
#' @return The DS dataset.
map_ds_301 <- function(raw, assignment, dm, spec) {
  enr <- raw$enrolment
  rand <- assignment$randomisation
  disp <- raw$disposition
  rec <- function(subject_id, term, decod, cat, scat, date) {
    data.frame(subject_id = subject_id, DSTERM = term, DSDECOD = decod, DSCAT = cat, DSSCAT = scat, date = date)
  }
  sf <- enr[enr$eligible == "N", ]
  scat <- c(TREATMENT = "STUDY TREATMENT", STUDY = "STUDY PARTICIPATION")
  term <- ifelse(disp$status == "COMPLETED", "COMPLETED", disp$reason)
  d <- rbind(
    rec(
      enr$subject_id, "INFORMED CONSENT OBTAINED", "INFORMED CONSENT OBTAINED", "PROTOCOL MILESTONE", NA,
      enr$consent_date
    ),
    rec(rand$subject_id, "RANDOMIZED", "RANDOMIZED", "PROTOCOL MILESTONE", NA, rand$randomisation_date),
    rec(
      sf$subject_id, ifelse(is.na(sf$failed_criterion), "SCREEN FAILURE", sf$failed_criterion), "SCREEN FAILURE",
      "DISPOSITION EVENT", "SCREENING", sf$screening_date
    ),
    rec(disp$subject_id, term, term, "DISPOSITION EVENT", unname(scat[disp$event]), disp$event_date)
  )
  d$USUBJID <- sdtm_usubjid(spec$study, d$subject_id)
  d$DSSTDTC <- crf_to_iso(d$date)
  d <- d[order(d$USUBJID, d$DSSTDTC, d$DSCAT != "PROTOCOL MILESTONE"), ]
  d$DSSEQ <- stats::ave(seq_len(nrow(d)), d$USUBJID, FUN = seq_along)
  d$DSSTDY <- sdtm_study_day(d$DSSTDTC, dm$RFSTDTC[match(d$USUBJID, dm$USUBJID)])
  d$STUDYID <- spec$study
  d$DOMAIN <- "DS"
  sdtm_conform(d, spec, "DS")
}
