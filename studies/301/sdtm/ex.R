# SDTM EX and DA of FJF-2026-301: one exposure record per dispensing interval, and the tablet
# counts as product accountability. The treatment comes from the assignment in use, so it is
# the dummy one until the database lock (ADR 0027).

#' Map the raw exposure of FJF-2026-301 to EX.
#'
#' @param raw Named list of the study's raw tables.
#' @param assignment Result of [assignment_in_use()].
#' @param dm The DM dataset.
#' @param protocol The protocol design.
#' @param spec The SDTM specification.
#' @return The EX dataset.
map_ex_301 <- function(raw, assignment, dm, protocol, spec) {
  ex <- raw$exposure
  rand <- assignment$randomisation
  dose <- stats::setNames(purrr::map_dbl(protocol$arms, "dose_mg"), purrr::map_chr(protocol$arms, "code"))
  ex$USUBJID <- sdtm_usubjid(spec$study, ex$subject_id)
  ex$EXSTDTC <- crf_to_iso(ex$dispense_date)
  ex <- ex[order(ex$USUBJID, ex$EXSTDTC), ]
  arm <- rand$arm_code[match(ex$subject_id, rand$subject_id)]
  if (anyNA(arm)) stop("Exposure recorded for subjects without an assigned arm")
  ex$EXDOSE <- unname(dose[arm])
  ex$EXTRT <- ifelse(ex$EXDOSE == 0, "PLACEBO", "FJF-2026")
  # The tablets of a bottle are taken until the day before it is returned. A bottle never
  # returned ends at the last dose, if it is the subject's last.
  returned <- iso_date(crf_to_iso(ex$return_date))
  last <- !duplicated(ex$USUBJID, fromLast = TRUE)
  last_dose <- dm$RFXENDTC[match(ex$USUBJID, dm$USUBJID)]
  ex$EXENDTC <- ifelse(!is.na(returned), format(returned - 1), ifelse(last, last_dose, NA))
  v <- planned_visit(ex$visit_code, protocol)
  ref <- dm$RFSTDTC[match(ex$USUBJID, dm$USUBJID)]
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "EX", USUBJID = ex$USUBJID, EXSEQ = sdtm_seq(ex$USUBJID),
    EXREFID = as.character(ex$record_id), EXTRT = ex$EXTRT, EXDOSE = ex$EXDOSE, EXDOSU = "mg",
    EXDOSFRM = "TABLET", EXDOSFRQ = "QD", EXROUTE = "ORAL", VISITNUM = v$VISITNUM, VISIT = v$VISIT,
    EXSTDTC = ex$EXSTDTC, EXENDTC = ex$EXENDTC, EXSTDY = sdtm_study_day(ex$EXSTDTC, ref),
    EXENDY = sdtm_study_day(ex$EXENDTC, ref)
  )
  sdtm_conform(out, spec, "EX")
}

#' Map the tablet counts of FJF-2026-301 to DA.
#'
#' @inheritParams map_ex_301
#' @return The DA dataset.
map_da_301 <- function(raw, dm, protocol, spec) {
  ex <- raw$exposure
  count <- function(test, name, n, date) {
    data.frame(
      subject_id = ex$subject_id, record_id = ex$record_id, visit_code = ex$visit_code,
      DATESTCD = test, DATEST = name, n = n, DADTC = crf_to_iso(date)
    )
  }
  d <- rbind(
    count("DISPAMT", "Dispensed Amount", ex$tablets_dispensed, ex$dispense_date),
    count("RETAMT", "Returned Amount", ex$tablets_returned, ex$return_date)
  )
  d$USUBJID <- sdtm_usubjid(spec$study, d$subject_id)
  d <- d[order(d$USUBJID, d$record_id, d$DATESTCD), ]
  v <- planned_visit(d$visit_code, protocol)
  done <- !is.na(d$n)
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "DA", USUBJID = d$USUBJID, DASEQ = sdtm_seq(d$USUBJID),
    DAREFID = as.character(d$record_id), DATESTCD = d$DATESTCD, DATEST = d$DATEST,
    DAORRES = ifelse(done, as.character(d$n), NA), DAORRESU = ifelse(done, "TABLET", NA),
    DASTRESC = ifelse(done, as.character(d$n), NA), DASTRESN = as.numeric(d$n),
    DASTRESU = ifelse(done, "TABLET", NA), DASTAT = ifelse(done, NA, "NOT DONE"),
    VISITNUM = v$VISITNUM, VISIT = v$VISIT, DADTC = d$DADTC,
    DADY = sdtm_study_day(d$DADTC, dm$RFSTDTC[match(d$USUBJID, dm$USUBJID)])
  )
  sdtm_conform(out, spec, "DA")
}
