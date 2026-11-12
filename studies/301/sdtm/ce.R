# SDTM CE of FJF-2026-301: the hypoglycaemic events of the dedicated CRF page, with their ADA
# level. The glucose value and whether assistance was needed go to SUPPCE.

#' Map the raw hypoglycaemic events of FJF-2026-301 to CE and SUPPCE.
#'
#' @param raw Named list of the study's raw tables.
#' @param dm The DM dataset.
#' @param spec The SDTM specification.
#' @param se The SE dataset, for EPOCH.
#' @return A list with the CE and SUPPCE datasets.
map_ce_301 <- function(raw, dm, spec, se) {
  hy <- raw$hypoglycaemia
  hy$USUBJID <- sdtm_usubjid(spec$study, hy$subject_id)
  hy$CESTDTC <- crf_to_iso(hy$event_date)
  hy <- hy[order(hy$USUBJID, hy$CESTDTC, hy$event_seq), ]
  seq <- sdtm_seq(hy$USUBJID)
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "CE", USUBJID = hy$USUBJID, CESEQ = seq,
    CESPID = as.character(hy$event_seq), CETERM = "HYPOGLYCAEMIA", CEDECOD = "Hypoglycaemia",
    CECAT = "HYPOGLYCAEMIA", CESCAT = paste("LEVEL", hy$level), CEPRESP = "Y", CEOCCUR = "Y",
    EPOCH = epoch_of(hy$USUBJID, hy$CESTDTC, se), CESTDTC = hy$CESTDTC, CESTDY = sdtm_study_day(
      hy$CESTDTC,
      dm$RFSTDTC[match(hy$USUBJID, dm$USUBJID)]
    )
  )
  qual <- function(qnam, qlabel, qval) {
    data.frame(
      STUDYID = spec$study, RDOMAIN = "CE", USUBJID = hy$USUBJID, IDVAR = "CESEQ",
      IDVARVAL = as.character(seq), QNAM = qnam, QLABEL = qlabel, QVAL = as.character(qval),
      QORIG = "Collected", QEVAL = NA_character_
    )
  }
  supp <- rbind(
    qual("CEGLUC", "Glucose at Event (mg/dL)", hy$glucose_mg_dl),
    qual("CEASSIST", "Assistance Required", hy$assistance)
  )
  supp <- supp[!is.na(supp$QVAL) & supp$QVAL != "", ]
  supp <- supp[order(supp$USUBJID, as.integer(supp$IDVARVAL), supp$QNAM), ]
  list(ce = sdtm_conform(out, spec, "CE"), suppce = sdtm_conform(supp, spec, "SUPPCE"))
}
