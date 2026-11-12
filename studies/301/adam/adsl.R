# ADSL of FJF-2026-301: one record per randomised subject, with the analysis sets, the
# treatment dates, the dates of the intercurrent events of the estimands (treatment
# discontinuation, rescue, end of study) and the baseline covariates of the SAP.

#' Derive ADSL of FJF-2026-301 from its SDTM.
#'
#' @param sdtm Named list of the SDTM datasets.
#' @param protocol The protocol design, for the doses of the arms.
#' @param spec The ADaM specification.
#' @return The ADSL dataset.
derive_adsl_301 <- function(sdtm, protocol, spec) {
  dm <- sdtm$DM[!is.na(sdtm$DM$ARMCD), ]
  dose <- stats::setNames(purrr::map_int(protocol$arms, "dose_mg"), purrr::map_chr(protocol$arms, "code"))
  supp <- sdtm$SUPPDM
  ds <- sdtm$DS
  by_subject <- function(d, value) value[match(dm$USUBJID, d$USUBJID)]

  rand <- ds[ds$DSDECOD == "RANDOMIZED", ]
  trt <- ds[!is.na(ds$DSSCAT) & ds$DSSCAT == "STUDY TREATMENT", ]
  study <- ds[!is.na(ds$DSSCAT) & ds$DSSCAT == "STUDY PARTICIPATION", ]
  randdt <- by_subject(rand, adam_date(rand$DSSTDTC))

  # Compliance: over the bottles returned with a count, tablets taken over days of use.
  ex <- sdtm$EX
  da <- sdtm$DA
  disp <- da$DASTRESN[da$DATESTCD == "DISPAMT"][match(ex$EXREFID, da$DAREFID[da$DATESTCD == "DISPAMT"])]
  ret <- da$DASTRESN[da$DATESTCD == "RETAMT"][match(ex$EXREFID, da$DAREFID[da$DATESTCD == "RETAMT"])]
  days <- as.numeric(adam_date(ex$EXENDTC) - adam_date(ex$EXSTDTC)) + 1
  use <- !is.na(ret) & !is.na(days)
  taken <- tapply((disp - ret)[use], ex$USUBJID[use], sum)
  used <- tapply(days[use], ex$USUBJID[use], sum)
  compl <- round(100 * taken / used, 1)

  cm <- sdtm$CM[sdtm$CM$CMCAT == "RESCUE MEDICATION", ]
  rescue <- tapply(adam_date(cm$CMSTDTC), cm$USUBJID, \(x) if (all(is.na(x))) NA else min(x, na.rm = TRUE))
  rescdt <- as.Date(as.numeric(rescue[dm$USUBJID]))

  # Baselines: the last value on or before randomisation (SAP); height from screening.
  last_before <- function(d, testcd, test, res, dtc) {
    x <- d[d[[testcd]] == test & !is.na(d[[res]]), ]
    x$date <- adam_date(x[[dtc]])
    x$randdt <- randdt[match(x$USUBJID, dm$USUBJID)]
    x <- x[!is.na(x$date) & !is.na(x$randdt) & x$date <= x$randdt, ]
    x <- x[order(x$USUBJID, x$date), ]
    x <- x[!duplicated(x$USUBJID, fromLast = TRUE), ]
    x[[res]][match(dm$USUBJID, x$USUBJID)]
  }
  hba1c <- last_before(sdtm$LB, "LBTESTCD", "HBA1C", "LBSTRESN", "LBDTC")
  weight <- last_before(sdtm$VS, "VSTESTCD", "WEIGHT", "VSSTRESN", "VSDTC")
  ht <- sdtm$VS[sdtm$VS$VSTESTCD == "HEIGHT", ]
  height <- ht$VSSTRESN[match(dm$USUBJID, ht$USUBJID)]
  bmi <- round(weight / (height / 100)^2, 1)

  trt_status <- by_subject(trt, trt$DSDECOD)
  study_status <- by_subject(study, study$DSDECOD)
  out <- data.frame(
    STUDYID = dm$STUDYID, USUBJID = dm$USUBJID, SUBJID = dm$SUBJID, SITEID = dm$SITEID, AGE = dm$AGE,
    AGEU = dm$AGEU, AGEGR1 = ifelse(dm$AGE < 65, "<65", ">=65"), AGEGR1N = ifelse(dm$AGE < 65, 1L, 2L),
    SEX = dm$SEX, RACE = dm$RACE, ETHNIC = dm$ETHNIC, ARMCD = dm$ARMCD, ARM = dm$ARM,
    ACTARMCD = dm$ACTARMCD, ACTARM = dm$ACTARM, TRT01P = dm$ARM, TRT01PN = unname(dose[dm$ARMCD]),
    TRT01A = dm$ACTARM, TRT01AN = unname(dose[dm$ACTARMCD]),
    STRAT1R = by_subject(supp[supp$QNAM == "STRATUM", ], supp$QVAL[supp$QNAM == "STRATUM"]),
    RANDDT = randdt, FASFL = "Y", SAFFL = ifelse(!is.na(dm$RFXSTDTC), "Y", "N"),
    TRTSDT = adam_date(dm$RFXSTDTC), TRTEDT = adam_date(dm$RFXENDTC),
    COMPLPCT = unname(compl[dm$USUBJID]),
    EOTSTT = ifelse(is.na(trt_status), NA, ifelse(trt_status == "COMPLETED", "COMPLETED", "DISCONTINUED")),
    DCTREAS = ifelse(!is.na(trt_status) & trt_status != "COMPLETED", trt_status, NA),
    EOSSTT = ifelse(is.na(study_status), NA, ifelse(study_status == "COMPLETED", "COMPLETED", "DISCONTINUED")),
    DCSREAS = ifelse(!is.na(study_status) & study_status != "COMPLETED", study_status, NA),
    EOSDT = by_subject(study, adam_date(study$DSSTDTC)),
    RESCFL = ifelse(is.na(rescdt), "N", "Y"), RESCDT = rescdt, LSTCNDT = adam_date(dm$RFPENDTC),
    DTHFL = dm$DTHFL, DTHDT = adam_date(dm$DTHDTC), HBA1CBL = hba1c, HEIGHTBL = height, WEIGHTBL = weight,
    BMIBL = bmi, BMIGR1 = ifelse(is.na(bmi), NA, ifelse(bmi < 30, "<30", ">=30"))
  )
  out$TRTDURD <- as.integer(out$TRTEDT - out$TRTSDT) + 1L
  out$DCTDT <- by_subject(trt, adam_date(trt$DSSTDTC))
  out$DCTDT[out$EOTSTT != "DISCONTINUED" | is.na(out$EOTSTT)] <- NA
  # Set by add_pps_301(), which needs the analysis visits of ADEFF.
  out$PPROTFL <- NA_character_
  out <- out[order(out$USUBJID), ]
  adam_conform(out, spec, "ADSL")
}
