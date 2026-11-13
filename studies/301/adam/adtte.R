# ADTTE of FJF-2026-301: time to first rescue medication (SAP B3) and time to the first
# level 2 or 3 hypoglycaemic episode (SAP SF2).

#' Derive ADTTE of FJF-2026-301.
#'
#' TTRESC (full analysis set): days from randomisation to the first dose of rescue
#' medication; patients without rescue, or rescued later, are censored on the day they leave
#' the study or at week 26 (study day 7 x 26 + 1), whichever comes first.
#'
#' TTHYPO23 (safety set): days from the first dose to the first level 2 or 3 episode over the
#' whole study, censored at the last contact.
#'
#' @param adsl The ADSL dataset.
#' @param adhypo The ADHYPO dataset.
#' @param protocol The protocol design, for the primary visit.
#' @param spec The ADaM specification.
#' @return The ADTTE dataset.
derive_adtte_301 <- function(adsl, adhypo, protocol, spec) {
  subject <- function(a) {
    data.frame(
      STUDYID = a$STUDYID, USUBJID = a$USUBJID, TRTP = a$TRT01P, TRTPN = a$TRT01PN, TRTA = a$TRT01A,
      TRTAN = a$TRT01AN, ARMCD = a$ARMCD, STRATAR = a$STRATAR, FASFL = a$FASFL, SAFFL = a$SAFFL, PPROTFL = a$PPROTFL
    )
  }
  a <- adsl[adsl$FASFL == "Y", ]
  end_primary <- a$RANDDT + 7 * protocol$primary_visit_week
  leave <- as.Date(ifelse(!is.na(a$EOSDT) & a$EOSDT < end_primary, a$EOSDT, end_primary))
  event <- !is.na(a$RESCDT) & a$RESCDT <= leave
  adt <- as.Date(ifelse(event, a$RESCDT, leave))
  resc <- cbind(subject(a), data.frame(
    PARAMCD = "TTRESC", PARAM = "Time to first rescue medication (days)", STARTDT = a$RANDDT, ADT = adt,
    AVAL = as.numeric(adt - a$RANDDT) + 1, CNSR = ifelse(event, 0L, 1L),
    EVNTDESC = ifelse(
      event, "RESCUE MEDICATION",
      ifelse(leave < end_primary, "END OF STUDY", sprintf("WEEK %d", protocol$primary_visit_week))
    ),
    SRCDOM = "ADSL", SRCVAR = ifelse(event, "RESCDT", ifelse(leave < end_primary, "EOSDT", "RANDDT"))
  ))

  s <- adsl[adsl$SAFFL == "Y", ]
  h <- adhypo[adhypo$HYPOLVL >= 2 & !is.na(adhypo$ADT), ]
  h <- h[h$ADT >= s$TRTSDT[match(h$USUBJID, s$USUBJID)] %in% TRUE, ]
  first <- tapply(h$ADT, h$USUBJID, min)
  hdt <- as.Date(unname(first[s$USUBJID]))
  event <- !is.na(hdt) & hdt <= s$LSTCNDT
  adt <- as.Date(ifelse(event, hdt, s$LSTCNDT))
  hypo <- cbind(subject(s), data.frame(
    PARAMCD = "TTHYPO23", PARAM = "Time to first level 2 or 3 hypoglycaemia (days)", STARTDT = s$TRTSDT, ADT = adt,
    AVAL = as.numeric(adt - s$TRTSDT) + 1, CNSR = ifelse(event, 0L, 1L),
    EVNTDESC = ifelse(event, "LEVEL 2 OR 3 HYPOGLYCAEMIA", "LAST CONTACT"),
    SRCDOM = ifelse(event, "ADHYPO", "ADSL"), SRCVAR = ifelse(event, "ADT", "LSTCNDT")
  ))
  out <- rbind(resc, hypo)
  adam_conform(out[order(out$USUBJID, out$PARAMCD), ], spec, "ADTTE")
}
