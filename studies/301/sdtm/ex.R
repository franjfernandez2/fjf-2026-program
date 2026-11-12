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
#' @param se The SE dataset, for EPOCH.
#' @return The EX dataset.
map_ex_301 <- function(raw, assignment, dm, protocol, spec, se) {
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
  # The tablets of a bottle are taken until the day before it is returned, and never after
  # the last dose. A bottle never returned ends at the last dose, if it is the subject's last.
  # CR10: if the last-dose date of the disposition page is after the end of the last bottle,
  # the disposition date is kept and the last bottle is taken until then.
  returned <- iso_date(crf_to_iso(ex$return_date))
  last <- !duplicated(ex$USUBJID, fromLast = TRUE)
  last_dose <- iso_date(dm$RFXENDTC[match(ex$USUBJID, dm$USUBJID)])
  end <- returned - 1
  capped <- !is.na(end) & !is.na(last_dose) & end > last_dose
  end[capped] <- last_dose[capped]
  end[is.na(end) & last] <- last_dose[is.na(end) & last]
  extend <- last & !is.na(end) & !is.na(last_dose) & end < last_dose
  end[extend] <- last_dose[extend]
  ex$EXENDTC <- ifelse(is.na(end), NA, format(end))
  v <- planned_visit(ex$visit_code, protocol)
  ref <- dm$RFSTDTC[match(ex$USUBJID, dm$USUBJID)]
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "EX", USUBJID = ex$USUBJID, EXSEQ = sdtm_seq(ex$USUBJID),
    EXREFID = as.character(ex$record_id), EXTRT = ex$EXTRT, EXDOSE = ex$EXDOSE, EXDOSU = "mg",
    EXDOSFRM = "TABLET", EXDOSFRQ = "QD", EXROUTE = "ORAL", VISITNUM = v$VISITNUM, VISIT = v$VISIT,
    EPOCH = epoch_of(ex$USUBJID, ex$EXSTDTC, se), EXSTDTC = ex$EXSTDTC, EXENDTC = ex$EXENDTC,
    EXSTDY = sdtm_study_day(ex$EXSTDTC, ref),
    EXENDY = sdtm_study_day(ex$EXENDTC, ref)
  )
  sdtm_conform(out, spec, "EX")
}

#' Data issues of the exposure: last-dose dates after the end of the last bottle (CR10).
#'
#' @param raw Named list of the study's raw tables.
#' @param dm The DM dataset, for the last-dose date (RFXENDTC, from the disposition page).
#' @param spec The SDTM specification.
#' @return A data frame of data issues, one row per subject concerned.
ex_issues_301 <- function(raw, dm, spec) {
  ex <- raw$exposure
  ex$USUBJID <- sdtm_usubjid(spec$study, ex$subject_id)
  ex <- ex[order(ex$USUBJID, crf_to_iso(ex$dispense_date)), ]
  ex <- ex[!duplicated(ex$USUBJID, fromLast = TRUE), ]
  end <- iso_date(crf_to_iso(ex$return_date)) - 1
  last_dose <- iso_date(dm$RFXENDTC[match(ex$USUBJID, dm$USUBJID)])
  late <- !is.na(end) & !is.na(last_dose) & end < last_dose
  data.frame(
    rule = rep("CR10", sum(late)), table = rep("exposure", sum(late)), subject_id = ex$subject_id[late],
    record_id = ex$record_id[late],
    detail = sprintf(
      "last dose %s (disposition) is %d day(s) after the last bottle ends (returned %s); exposure extended to it",
      format(last_dose[late]), as.integer(last_dose[late] - end[late]), format(end[late] + 1)
    )
  )
}

#' Map the tablet counts of FJF-2026-301 to DA.
#'
#' @inheritParams map_ex_301
#' @return The DA dataset.
map_da_301 <- function(raw, dm, protocol, spec, se) {
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
    VISITNUM = v$VISITNUM, VISIT = v$VISIT, EPOCH = epoch_of(d$USUBJID, d$DADTC, se), DADTC = d$DADTC,
    DADY = sdtm_study_day(d$DADTC, dm$RFSTDTC[match(d$USUBJID, dm$USUBJID)])
  )
  sdtm_conform(out, spec, "DA")
}
