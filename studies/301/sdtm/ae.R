# SDTM AE and SUPPAE of FJF-2026-301. Terms are coded with the project's reduced dictionary.
# Partial dates stay partial (CR02); an end date before the start is moved to SUPPAE (CR08);
# a missing severity stays missing, and ADaM counts it as severe (CR09).

#' Map the raw adverse events of FJF-2026-301 to AE.
#'
#' @param raw Named list of the study's raw tables.
#' @param dm The DM dataset.
#' @param dictionary The coding dictionary (docs/specs/coding-dictionary.yml).
#' @param spec The SDTM specification.
#' @param se The SE dataset, for EPOCH.
#' @return A list with the AE and SUPPAE datasets.
map_ae_301 <- function(raw, dm, dictionary, spec, se) {
  ae <- raw$adverse_events
  ae$USUBJID <- sdtm_usubjid(spec$study, ae$subject_id)
  ae$AESTDTC <- crf_to_iso(ae$start_date)
  ae <- ae[order(ae$USUBJID, ae$AESTDTC, ae$ae_seq), ]
  coded <- code_terms(ae$verbatim_term, dictionary$adverse_events, "adverse event terms")
  end <- crf_to_iso(ae$end_date)
  ends_first <- !is.na(iso_date(end)) & !is.na(iso_date(ae$AESTDTC)) & iso_date(end) < iso_date(ae$AESTDTC)
  ref <- dm$RFSTDTC[match(ae$USUBJID, dm$USUBJID)]
  seq <- sdtm_seq(ae$USUBJID)
  sev <- trimws(ae$severity)
  sev[sev == ""] <- NA
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "AE", USUBJID = ae$USUBJID, AESEQ = seq,
    AESPID = as.character(ae$ae_seq), AETERM = ae$verbatim_term, AELLT = coded$llt,
    AEDECOD = coded$pt, AEBODSYS = coded$soc, AESOC = coded$soc, AESEV = sev, AESER = ae$serious,
    AEACN = ae$action_taken, AEREL = c(Y = "RELATED", N = "NOT RELATED")[ae$related],
    AEOUT = c(RECOVERED = "RECOVERED/RESOLVED", "NOT RECOVERED" = "NOT RECOVERED/NOT RESOLVED")[ae$outcome],
    EPOCH = epoch_of(ae$USUBJID, ae$AESTDTC, se), AESTDTC = ae$AESTDTC, AEENDTC = ifelse(ends_first, NA, end),
    AESTDY = sdtm_study_day(ae$AESTDTC, ref), AEENDY = sdtm_study_day(ifelse(ends_first, NA, end), ref),
    AEENRTPT = ifelse(ends_first, "UNKNOWN", ifelse(is.na(end), "ONGOING", NA)),
    AEENTPT = ifelse(ends_first | is.na(end), "END OF STUDY", NA)
  )
  supp <- data.frame(
    STUDYID = spec$study, RDOMAIN = "AE", USUBJID = ae$USUBJID[ends_first], IDVAR = "AESEQ",
    IDVARVAL = as.character(seq[ends_first]), QNAM = "AEENRAW", QLABEL = "End Date as Collected",
    QVAL = end[ends_first], QORIG = "Collected", QEVAL = rep(NA_character_, sum(ends_first))
  )
  list(ae = sdtm_conform(out, spec, "AE"), suppae = sdtm_conform(supp, spec, "SUPPAE"))
}
