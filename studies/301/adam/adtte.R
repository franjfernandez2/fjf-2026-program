# ADTTE of FJF-2026-301: time to first rescue medication (SAP B3).

#' Derive ADTTE of FJF-2026-301.
#'
#' Days from randomisation to the first dose of rescue medication. Patients without rescue,
#' or rescued later, are censored on the day they leave the study or at week 26 (study day
#' 7 x 26 + 1), whichever comes first.
#'
#' @param adsl The ADSL dataset.
#' @param protocol The protocol design, for the primary visit.
#' @param spec The ADaM specification.
#' @return The ADTTE dataset.
derive_adtte_301 <- function(adsl, protocol, spec) {
  a <- adsl[adsl$FASFL == "Y", ]
  week26 <- a$RANDDT + 7 * protocol$primary_visit_week
  leave <- ifelse(!is.na(a$EOSDT) & a$EOSDT < week26, a$EOSDT, week26)
  leave <- as.Date(leave)
  event <- !is.na(a$RESCDT) & a$RESCDT <= leave
  adt <- as.Date(ifelse(event, a$RESCDT, leave))
  out <- data.frame(
    STUDYID = a$STUDYID, USUBJID = a$USUBJID, TRTP = a$TRT01P, TRTPN = a$TRT01PN, ARMCD = a$ARMCD,
    STRAT1R = a$STRAT1R, FASFL = a$FASFL, PPROTFL = a$PPROTFL, PARAMCD = "TTRESC",
    PARAM = "Time to first rescue medication (days)", STARTDT = a$RANDDT, ADT = adt,
    AVAL = as.numeric(adt - a$RANDDT) + 1, CNSR = ifelse(event, 0L, 1L),
    EVNTDESC = ifelse(event, "RESCUE MEDICATION", ifelse(leave < week26, "END OF STUDY", "WEEK 26")),
    SRCDOM = "ADSL", SRCVAR = ifelse(event, "RESCDT", ifelse(leave < week26, "EOSDT", "RANDDT"))
  )
  adam_conform(out, spec, "ADTTE")
}
