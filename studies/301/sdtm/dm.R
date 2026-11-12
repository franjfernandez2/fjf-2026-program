# SDTM DM of FJF-2026-301: one record per screened subject (screen failures included, with
# what the CRF collected for them).

#' Map the raw tables of FJF-2026-301 to DM.
#'
#' @param raw Named list of the study's raw tables ([read_raw_files()]).
#' @param assignment Result of [assignment_in_use()]: the dummy arms before the lock.
#' @param protocol The protocol design ([read_protocol()]).
#' @param spec The SDTM specification (docs/specs/sdtm-301.yml).
#' @return The DM dataset.
map_dm_301 <- function(raw, assignment, protocol, spec) {
  enr <- raw$enrolment
  rand <- assignment$randomisation[c("subject_id", "randomisation_date", "arm_code")]
  dem <- raw$demography
  arms <- data.frame(
    arm_code = purrr::map_chr(protocol$arms, "code"), arm_label = purrr::map_chr(protocol$arms, "label")
  )
  ex <- raw$exposure
  first_dose <- stats::aggregate(
    list(first = iso_date(crf_to_iso(ex$dispense_date))), list(subject_id = ex$subject_id), min,
    na.rm = TRUE
  )
  ds <- raw$disposition
  last_dose <- ds[ds$event == "TREATMENT", c("subject_id", "event_date")]
  names(last_dose)[2] <- "last_dose_date"
  end_study <- ds[ds$event == "STUDY", c("subject_id", "event_date", "reason")]
  names(end_study)[2:3] <- c("end_date", "end_reason")

  d <- merge(enr, rand, by = "subject_id", all.x = TRUE)
  d <- merge(d, dem, by = "subject_id", all.x = TRUE, suffixes = c("", ".dem"))
  d <- merge(d, first_dose, by = "subject_id", all.x = TRUE)
  d <- merge(d, last_dose, by = "subject_id", all.x = TRUE)
  d <- merge(d, end_study, by = "subject_id", all.x = TRUE)
  d <- merge(d, arms, by = "arm_code", all.x = TRUE)
  d <- d[order(d$subject_id), ]

  # End of participation: the last date of contact, the latest complete date in the
  # subject's records (a follow-up visit or event can come after the end-of-study page).
  last <- last_contact_301(raw)
  d$last_contact <- last$date[match(d$subject_id, last$subject_id)]

  randomised <- !is.na(d$arm_code)
  dosed <- randomised & !is.na(d$first)
  birth <- crf_to_iso(d$birth_date)
  consent <- crf_to_iso(d$consent_date)
  out <- data.frame(
    STUDYID = spec$study,
    DOMAIN = "DM",
    USUBJID = sdtm_usubjid(spec$study, d$subject_id),
    SUBJID = d$subject_id,
    RFSTDTC = ifelse(dosed, format(d$first), NA),
    RFENDTC = ifelse(dosed, crf_to_iso(d$last_dose_date), NA),
    RFXSTDTC = ifelse(dosed, format(d$first), NA),
    RFXENDTC = ifelse(dosed, crf_to_iso(d$last_dose_date), NA),
    RFICDTC = consent,
    RFPENDTC = format(d$last_contact),
    DTHDTC = ifelse(!is.na(d$end_reason) & d$end_reason == "DEATH", crf_to_iso(d$end_date), NA),
    DTHFL = ifelse(!is.na(d$end_reason) & d$end_reason == "DEATH", "Y", NA),
    SITEID = d$site_id,
    BRTHDTC = birth,
    AGE = whole_years(impute_mid_period(birth), iso_date(consent)),
    AGEU = ifelse(is.na(birth), NA, "YEARS"),
    SEX = d$sex,
    RACE = d$race,
    ETHNIC = d$ethnicity,
    ARMCD = ifelse(randomised, d$arm_code, NA),
    ARM = ifelse(randomised, d$arm_label, NA),
    ACTARMCD = ifelse(dosed, d$arm_code, NA),
    ACTARM = ifelse(dosed, d$arm_label, NA),
    ARMNRS = ifelse(!randomised, "SCREEN FAILURE", ifelse(!dosed, "ASSIGNED, NOT TREATED", NA)),
    ACTARMUD = NA_character_,
    COUNTRY = if (is.null(spec$country)) NA_character_ else spec$country
  )
  sdtm_conform(out, spec, "DM")
}

#' Last date of contact of each subject: the latest complete date in the raw tables.
#'
#' @param raw Named list of the study's raw tables.
#' @return A data frame with `subject_id` and `date`.
last_contact_301 <- function(raw) {
  cols <- list(
    enrolment = c("consent_date", "screening_date"), visits = "visit_date", lab_results = "collection_date",
    vital_signs = "measure_date", exposure = c("dispense_date", "return_date"), disposition = "event_date",
    adverse_events = c("start_date", "end_date"), hypoglycaemia = "event_date"
  )
  dates <- purrr::list_rbind(purrr::imap(cols, \(cc, table) {
    purrr::list_rbind(purrr::map(cc, \(col) {
      data.frame(subject_id = raw[[table]]$subject_id, date = iso_date(crf_to_iso(raw[[table]][[col]])))
    }))
  }))
  dates <- dates[!is.na(dates$date), ]
  stats::aggregate(list(date = dates$date), list(subject_id = dates$subject_id), max)
}

#' SUPPDM of FJF-2026-301: the randomisation stratum and number from the randomisation
#' system, which have no DM variable.
#'
#' @param raw Named list of the study's raw tables.
#' @param spec The SDTM specification.
#' @return The SUPPDM dataset.
map_suppdm_301 <- function(raw, spec) {
  r <- raw$randomisation
  r <- r[order(r$subject_id), ]
  usubjid <- sdtm_usubjid(spec$study, r$subject_id)
  qual <- function(qnam, qlabel, qval) {
    data.frame(
      STUDYID = spec$study, RDOMAIN = "DM", USUBJID = usubjid, IDVAR = NA_character_, IDVARVAL = NA_character_,
      QNAM = qnam, QLABEL = qlabel, QVAL = as.character(qval), QORIG = "Collected", QEVAL = NA_character_
    )
  }
  out <- rbind(
    qual("RANDNO", "Randomisation Number", r$randomisation_number),
    qual("STRATUM", "Randomisation Stratum", r$stratum)
  )
  sdtm_conform(out[order(out$USUBJID, out$QNAM), ], spec, "SUPPDM")
}
