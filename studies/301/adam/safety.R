# Safety ADaM datasets of FJF-2026-301 (SAP SF1-SF5): ADEX, ADAE, ADHYPO, ADLB and ADVS.
# All are for randomised patients and carry the treatment actually taken; the tables
# select the safety set with SAFFL.

#' Subject-level variables carried by every safety dataset.
#'
#' @param usubjid Subject of each record.
#' @param adsl The ADSL dataset.
#' @return A data frame, one row per record.
safety_subject_301 <- function(usubjid, adsl) {
  a <- adsl[match(usubjid, adsl$USUBJID), ]
  data.frame(
    STUDYID = a$STUDYID, USUBJID = usubjid, TRTA = a$TRT01A, TRTAN = a$TRT01AN, SAFFL = a$SAFFL,
    RANDDT = a$RANDDT, TRTSDT = a$TRTSDT, TRTEDT = a$TRTEDT, RESCDT = a$RESCDT
  )
}

#' On-treatment flag of the SAP: on or after the first dose and no later than 7 days after
#' the last.
#'
#' @param date Dates of the records.
#' @param trtsdt,trtedt First and last dose dates of each record's subject.
#' @param after_first If TRUE, the first-dose day itself is excluded (it holds the baseline).
#' @return "Y" or NA (record-level flags are Y or null).
on_treatment_301 <- function(date, trtsdt, trtedt, after_first = FALSE) {
  start <- if (after_first) date > trtsdt else date >= trtsdt
  ifelse(!is.na(date) & !is.na(trtsdt) & start & date <= trtedt + 7, "Y", NA)
}

#' Derive ADEX of FJF-2026-301 (SAP SF5).
#'
#' @param adsl The ADSL dataset.
#' @param spec The ADaM specification.
#' @return The ADEX dataset.
derive_adex_301 <- function(adsl, spec) {
  a <- adsl[adsl$SAFFL == "Y", ]
  out <- safety_subject_301(a$USUBJID, adsl)
  out$TRTDURD <- a$TRTDURD
  out$TRTDURY <- a$TRTDURD / 365.25
  out$COMPLPCT <- a$COMPLPCT
  adam_conform(out, spec, "ADEX")
}

#' Analysis start date of a partial adverse event start date.
#'
#' A date missing the day ("2028-09") or the month ("2028") is imputed to the first dose if
#' the period contains it and the event did not end before it, so that an event that may be
#' treatment emergent is counted as such; otherwise to the first day of the period.
#'
#' @param dtc ISO 8601 start dates, complete or partial.
#' @param trtsdt First dose dates.
#' @param endt Complete end dates, or NA.
#' @return A list with `date` and `flag` ("D", "M" or NA).
impute_ae_start_301 <- function(dtc, trtsdt, endt) {
  date <- adam_date(dtc)
  flag <- rep(NA_character_, length(dtc))
  ym <- is.na(date) & grepl("^[0-9]{4}-[0-9]{2}$", dtc)
  y <- is.na(date) & grepl("^[0-9]{4}$", dtc)
  period <- ifelse(ym, dtc, ifelse(y, dtc, NA))
  first_day <- as.Date(ifelse(ym, paste0(dtc, "-01"), ifelse(y, paste0(dtc, "-01-01"), NA)))
  same <- (ym & format(trtsdt, "%Y-%m") == period) | (y & format(trtsdt, "%Y") == period)
  use_dose <- same %in% TRUE & (is.na(endt) | endt >= trtsdt)
  date[ym | y] <- first_day[ym | y]
  date[use_dose] <- trtsdt[use_dose]
  flag[ym] <- "D"
  flag[y] <- "M"
  list(date = date, flag = flag)
}

#' Derive ADAE of FJF-2026-301 (SAP SF1).
#'
#' @param sdtm Named list of the SDTM datasets.
#' @param adsl The ADSL dataset.
#' @param spec The ADaM specification.
#' @return The ADAE dataset.
derive_adae_301 <- function(sdtm, adsl, spec) {
  ae <- sdtm$AE[sdtm$AE$USUBJID %in% adsl$USUBJID, ]
  out <- cbind(safety_subject_301(ae$USUBJID, adsl), ae[setdiff(names(ae), c("STUDYID", "USUBJID"))])
  # CR09: a missing severity counts as severe in the tables by severity, with a flag.
  out$ASEVIMFL <- ifelse(is.na(out$AESEV), "Y", NA)
  out$ASEV <- ifelse(is.na(out$AESEV), "SEVERE", out$AESEV)
  out$ASEVN <- match(out$ASEV, c("MILD", "MODERATE", "SEVERE"))
  out$AENDT <- adam_date(out$AEENDTC)
  start <- impute_ae_start_301(out$AESTDTC, out$TRTSDT, out$AENDT)
  out$ASTDT <- start$date
  out$ASTDTF <- start$flag
  out$ASTDY <- adam_study_day(out$ASTDT, out$RANDDT)
  out$AENDY <- adam_study_day(out$AENDT, out$RANDDT)
  emergent <- !is.na(out$TRTSDT) & out$ASTDT >= out$TRTSDT & out$ASTDT <= out$TRTEDT + 14
  out$TRTEMFL <- ifelse(emergent %in% TRUE, "Y", NA)
  adam_conform(out[order(out$USUBJID, out$AESEQ), ], spec, "ADAE")
}

#' Derive ADHYPO of FJF-2026-301 (SAP SF2).
#'
#' @param sdtm Named list of the SDTM datasets.
#' @param adsl The ADSL dataset.
#' @param spec The ADaM specification.
#' @return The ADHYPO dataset.
derive_adhypo_301 <- function(sdtm, adsl, spec) {
  ce <- sdtm$CE[sdtm$CE$CECAT == "HYPOGLYCAEMIA" & sdtm$CE$USUBJID %in% adsl$USUBJID, ]
  supp <- sdtm$SUPPCE
  qval <- function(qnam) {
    q <- supp[supp$QNAM == qnam, ]
    q$QVAL[match(paste(ce$USUBJID, ce$CESEQ), paste(q$USUBJID, q$IDVARVAL))]
  }
  out <- cbind(safety_subject_301(ce$USUBJID, adsl), ce[c("CESEQ", "CESCAT")])
  out$HYPOLVL <- as.integer(sub("^LEVEL ", "", out$CESCAT))
  out$GLUCOSE <- as.numeric(qval("CEGLUC"))
  out$ASSIST <- qval("CEASSIST")
  out$ADT <- adam_date(ce$CESTDTC)
  out$ADY <- adam_study_day(out$ADT, out$RANDDT)
  out$ONTRTFL <- on_treatment_301(out$ADT, out$TRTSDT, out$TRTEDT)
  out$PRERESFL <- ifelse(is.na(out$RESCDT) | out$ADT <= out$RESCDT, "Y", NA)
  adam_conform(out[order(out$USUBJID, out$CESEQ), ], spec, "ADHYPO")
}

#' Baseline, analysis visits and on-treatment flags of a safety BDS dataset.
#'
#' Baseline is the last value on or before the first dose (randomisation for a patient never
#' dosed). Values at planned post-baseline visits get the visit's week as AVISITN; values at
#' unscheduled visits keep an empty AVISIT but count for the worst value on treatment.
#'
#' @param obs Records with USUBJID, PARAMCD, PARAM, ADT, AVAL, VISITNUM, SRCDOM, SRCVAR,
#'   SRCSEQ (and, for laboratory data, ANRLO, ANRHI and ANRIND).
#' @param adsl The ADSL dataset.
#' @param protocol The protocol design, for the visit weeks.
#' @return The records with the subject variables, AVISIT, AVISITN, ADY, BASE, CHG, ABLFL,
#'   ONTRTFL and ANL01FL.
safety_bds_301 <- function(obs, adsl, protocol) {
  obs <- obs[obs$USUBJID %in% adsl$USUBJID & !is.na(obs$AVAL) & !is.na(obs$ADT), ]
  out <- cbind(safety_subject_301(obs$USUBJID, adsl), obs[setdiff(names(obs), "USUBJID")])
  ref <- as.Date(ifelse(is.na(out$TRTSDT), out$RANDDT, out$TRTSDT))
  out$ADY <- adam_study_day(out$ADT, out$RANDDT)
  pre <- which(out$ADT <= ref)
  pre <- pre[order(out$USUBJID[pre], out$PARAMCD[pre], out$ADT[pre], out$SRCSEQ[pre])]
  key <- paste(out$USUBJID, out$PARAMCD)
  base_row <- pre[!duplicated(key[pre], fromLast = TRUE)]
  out$ABLFL <- NA_character_
  out$ABLFL[base_row] <- "Y"
  out$BASE <- out$AVAL[base_row][match(key, key[base_row])]
  week <- protocol$visits$week[out$VISITNUM]
  planned <- !is.na(out$VISITNUM) & out$VISITNUM == round(out$VISITNUM) & out$ADT > ref
  planned <- planned %in% TRUE & week > 0
  out$AVISITN <- ifelse(planned, week, NA)
  out$AVISIT <- ifelse(planned, sprintf("Week %d", as.integer(week)), NA)
  out$AVISITN[base_row] <- 0L
  out$AVISIT[base_row] <- "Baseline"
  out$CHG <- ifelse(out$ADT > ref, out$AVAL - out$BASE, NA)
  out$ONTRTFL <- on_treatment_301(out$ADT, out$TRTSDT, out$TRTEDT, after_first = TRUE)
  out <- out[order(out$USUBJID, out$PARAMCD, out$ADT, out$SRCSEQ), ]
  sel <- !is.na(out$AVISITN) & out$AVISITN > 0
  first <- sel & !duplicated(paste(out$USUBJID, out$PARAMCD, out$AVISITN, sel))
  out$ANL01FL <- ifelse(first, "Y", NA)
  out
}

#' Derive ADLB of FJF-2026-301 (SAP SF3).
#'
#' @param sdtm Named list of the SDTM datasets.
#' @param adsl The ADSL dataset.
#' @param protocol The protocol design.
#' @param spec The ADaM specification.
#' @return The ADLB dataset.
derive_adlb_301 <- function(sdtm, adsl, protocol, spec) {
  lb <- sdtm$LB
  obs <- data.frame(
    USUBJID = lb$USUBJID, PARAMCD = lb$LBTESTCD, PARAM = sprintf("%s (%s)", lb$LBTEST, lb$LBSTRESU),
    ADT = adam_date(lb$LBDTC), AVAL = lb$LBSTRESN, ANRLO = lb$LBSTNRLO, ANRHI = lb$LBSTNRHI, ANRIND = lb$LBNRIND,
    VISITNUM = lb$VISITNUM, SRCDOM = "LB", SRCVAR = "LBSTRESN", SRCSEQ = lb$LBSEQ
  )
  out <- safety_bds_301(obs, adsl, protocol)
  key <- paste(out$USUBJID, out$PARAMCD)
  out$BNRIND <- out$ANRIND[!is.na(out$ABLFL)][match(key, key[!is.na(out$ABLFL)])]
  # Worst on treatment: the value furthest outside its range, as a multiple of the limit
  # crossed; with none outside, the last value on treatment.
  score <- ifelse(
    out$ANRIND %in% "HIGH", out$AVAL / out$ANRHI, ifelse(out$ANRIND %in% "LOW", out$ANRLO / out$AVAL, 0)
  )
  cand <- which(out$ONTRTFL %in% "Y")
  cand <- cand[order(key[cand], -score[cand], -as.numeric(out$ADT[cand]))]
  out$WORSTFL <- NA_character_
  out$WORSTFL[cand[!duplicated(key[cand])]] <- "Y"
  adam_conform(out, spec, "ADLB")
}

#' Derive ADVS of FJF-2026-301 (SAP SF4). Height is only a baseline characteristic (ADSL).
#'
#' @param sdtm Named list of the SDTM datasets.
#' @param adsl The ADSL dataset.
#' @param protocol The protocol design.
#' @param spec The ADaM specification.
#' @return The ADVS dataset.
derive_advs_301 <- function(sdtm, adsl, protocol, spec) {
  vs <- sdtm$VS[sdtm$VS$VSTESTCD != "HEIGHT", ]
  obs <- data.frame(
    USUBJID = vs$USUBJID, PARAMCD = vs$VSTESTCD, PARAM = sprintf("%s (%s)", vs$VSTEST, vs$VSSTRESU),
    ADT = adam_date(vs$VSDTC), AVAL = vs$VSSTRESN, VISITNUM = vs$VISITNUM, SRCDOM = "VS", SRCVAR = "VSSTRESN",
    SRCSEQ = vs$VSSEQ
  )
  adam_conform(safety_bds_301(obs, adsl, protocol), spec, "ADVS")
}
