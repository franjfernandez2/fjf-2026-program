# SDTM CM of FJF-2026-301: background antidiabetic therapy and rescue medication, coded with
# the project's reduced dictionary (ATC classes).

#' Map the raw concomitant medications of FJF-2026-301 to CM.
#'
#' @param raw Named list of the study's raw tables.
#' @param dm The DM dataset.
#' @param dictionary The coding dictionary (docs/specs/coding-dictionary.yml).
#' @param spec The SDTM specification.
#' @return The CM dataset.
map_cm_301 <- function(raw, dm, dictionary, spec) {
  cm <- raw$concomitant_meds
  cm$USUBJID <- sdtm_usubjid(spec$study, cm$subject_id)
  cm$CMSTDTC <- crf_to_iso(cm$start_date)
  cm <- cm[order(cm$USUBJID, cm$CMSTDTC, cm$cm_seq), ]
  coded <- code_terms(cm$drug_name, dictionary$medications, "medications")
  dose <- trimws(cm$daily_dose)
  dose[dose == ""] <- NA
  end <- crf_to_iso(cm$end_date)
  ref <- dm$RFSTDTC[match(cm$USUBJID, dm$USUBJID)]
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "CM", USUBJID = cm$USUBJID, CMSEQ = sdtm_seq(cm$USUBJID),
    CMSPID = as.character(cm$cm_seq), CMTRT = cm$drug_name, CMDECOD = coded$decod,
    CMCAT = ifelse(cm$rescue == "Y", "RESCUE MEDICATION", "BACKGROUND ANTIDIABETIC THERAPY"),
    CMCLAS = coded$class, CMCLASCD = coded$atc, CMINDC = cm$indication,
    CMDOSTOT = as.numeric(sub("^([0-9.]+) .*$", "\\1", dose)),
    CMDOSU = ifelse(is.na(dose), NA, tolower(sub("^[0-9.]+ ", "", dose))),
    CMSTDTC = cm$CMSTDTC, CMENDTC = end, CMSTDY = sdtm_study_day(cm$CMSTDTC, ref),
    CMENDY = sdtm_study_day(end, ref), CMENRTPT = ifelse(is.na(end), "ONGOING", NA),
    CMENTPT = ifelse(is.na(end), "END OF STUDY", NA)
  )
  sdtm_conform(out, spec, "CM")
}
