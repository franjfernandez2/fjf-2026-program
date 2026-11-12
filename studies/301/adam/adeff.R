# ADEFF of FJF-2026-301: HbA1c and body weight by analysis visit, with the SAP's visit
# windows, baselines and the flags of the estimands' intercurrent events.

#' Analysis windows of the scheduled visits (SAP visit_windows), for any assessment.
#'
#' The same rule as [analysis_windows_301()] of the SAP checks, which covers HbA1c only:
#' target day 7 x week + 1 from randomisation, boundaries at the midpoints between targets
#' (a value exactly on a midpoint goes to the earlier visit), the first window from day 2,
#' and the last ending as far after its target as half the gap to the visit before it.
#'
#' @param weeks Weeks of the scheduled post-baseline visits.
#' @return A data frame with AVISITN, AVISIT, AWTARGET, AWLO and AWHI.
adam_windows_301 <- function(weeks) {
  target <- 7L * as.integer(weeks) + 1L
  n <- length(target)
  mid <- (target[-n] + target[-1]) / 2
  hi <- c(floor(mid), floor(target[n] + (target[n] - c(1L, target)[n]) / 2))
  lo <- c(2L, floor(mid) + 1L)
  data.frame(
    AVISITN = as.integer(weeks), AVISIT = sprintf("Week %d", weeks), AWTARGET = target, AWLO = as.integer(lo),
    AWHI = as.integer(hi)
  )
}

#' Window one parameter: baseline, the records of each window, the selected one, and a
#' missing-visit record where a window is empty.
#'
#' @param obs Observations: USUBJID, ADT, AVAL, SRCDOM, SRCVAR, SRCSEQ.
#' @param adsl The ADSL dataset.
#' @param windows Result of [adam_windows_301()].
#' @param base_from Function of (adsl, baseline values) giving BASE and BIMPFL per subject.
#' @return A data frame of BDS records.
window_parameter_301 <- function(obs, adsl, windows, base_from) {
  obs$RANDDT <- adsl$RANDDT[match(obs$USUBJID, adsl$USUBJID)]
  obs <- obs[!is.na(obs$RANDDT) & !is.na(obs$ADT) & !is.na(obs$AVAL), ]
  obs$ADY <- adam_study_day(obs$ADT, obs$RANDDT)
  pre <- obs[obs$ADY <= 1, ]
  pre <- pre[order(pre$USUBJID, pre$ADT, pre$SRCSEQ), ]
  bl <- pre[!duplicated(pre$USUBJID, fromLast = TRUE), ]
  base <- base_from(adsl, bl$AVAL[match(adsl$USUBJID, bl$USUBJID)])
  n <- nrow(bl)
  bl <- cbind(bl, data.frame(
    AVISIT = rep("Baseline", n), AVISITN = rep(0L, n), AWTARGET = rep(1L, n), AWLO = rep(NA_integer_, n),
    AWHI = rep(1L, n), ABLFL = rep("Y", n), ANL01FL = rep("Y", n), DTYPE = rep(NA_character_, n)
  ))

  post <- obs[obs$ADY >= windows$AWLO[1] & obs$ADY <= max(windows$AWHI), ]
  w <- findInterval(post$ADY, windows$AWLO)
  post <- cbind(post, windows[w, ])
  post$ABLFL <- rep(NA_character_, nrow(post))
  post$DTYPE <- rep(NA_character_, nrow(post))
  # The value closest to the target; if two are equally close, the later one.
  post <- post[order(post$USUBJID, post$AVISITN, abs(post$ADY - post$AWTARGET), -as.numeric(post$ADT)), ]
  post$ANL01FL <- ifelse(duplicated(post[c("USUBJID", "AVISITN")]), NA_character_, "Y")

  grid <- merge(adsl[c("USUBJID", "RANDDT")], windows)
  have <- paste(post$USUBJID, post$AVISITN)
  phantom <- grid[!paste(grid$USUBJID, grid$AVISITN) %in% have, ]
  if (nrow(phantom) > 0) {
    phantom$ADT <- as.Date(NA)
    phantom$ADY <- NA_integer_
    phantom$AVAL <- NA_real_
    phantom[c("SRCDOM", "SRCVAR")] <- NA_character_
    phantom$SRCSEQ <- NA_integer_
    phantom$ABLFL <- NA_character_
    phantom$ANL01FL <- "Y"
    phantom$DTYPE <- "PHANTOM"
  }
  cols <- c(
    "USUBJID", "RANDDT", "AVISIT", "AVISITN", "AWTARGET", "AWLO", "AWHI", "ADT", "ADY", "AVAL", "ABLFL", "ANL01FL",
    "DTYPE", "SRCDOM", "SRCVAR", "SRCSEQ"
  )
  out <- rbind(bl[cols], post[cols], phantom[cols])
  out$BASE <- base$BASE[match(out$USUBJID, adsl$USUBJID)]
  out$BIMPFL <- base$BIMPFL[match(out$USUBJID, adsl$USUBJID)]
  out$CHG <- ifelse(is.na(out$ABLFL), out$AVAL - out$BASE, NA)
  out
}

#' Derive ADEFF of FJF-2026-301.
#'
#' @param sdtm Named list of the SDTM datasets.
#' @param adsl The ADSL dataset (without PPROTFL is fine: it is added by [add_pps_301()]).
#' @param protocol The protocol design, for the HbA1c and weight visits.
#' @param spec The ADaM specification.
#' @return The ADEFF dataset.
derive_adeff_301 <- function(sdtm, adsl, protocol, spec) {
  lb <- sdtm$LB[sdtm$LB$LBTESTCD == "HBA1C", ]
  vs <- sdtm$VS[sdtm$VS$VSTESTCD == "WEIGHT", ]
  hba1c <- data.frame(
    USUBJID = lb$USUBJID, ADT = adam_date(lb$LBDTC), AVAL = lb$LBSTRESN, SRCDOM = "LB", SRCVAR = "LBSTRESN",
    SRCSEQ = lb$LBSEQ
  )
  weight <- data.frame(
    USUBJID = vs$USUBJID, ADT = adam_date(vs$VSDTC), AVAL = vs$VSSTRESN, SRCDOM = "VS", SRCVAR = "VSSTRESN",
    SRCSEQ = vs$VSSEQ
  )
  # SAP: a missing baseline HbA1c is imputed by the mean of the stratum. Weight is not imputed.
  stratum_mean <- function(adsl, base) {
    m <- tapply(base, adsl$STRAT1R, mean, na.rm = TRUE)
    imputed <- is.na(base)
    list(BASE = ifelse(imputed, round(m[adsl$STRAT1R], 4), base), BIMPFL = ifelse(imputed, "Y", NA))
  }
  as_is <- function(adsl, base) list(BASE = base, BIMPFL = rep(NA_character_, length(base)))
  out <- rbind(
    cbind(
      PARAMCD = "HBA1C", PARAM = "HbA1c (%)", PARAMN = 1L,
      window_parameter_301(hba1c, adsl, adam_windows_301(hba1c_visit_weeks(protocol)), stratum_mean)
    ),
    cbind(
      PARAMCD = "WEIGHT", PARAM = "Body weight (kg)", PARAMN = 2L,
      window_parameter_301(weight, adsl, adam_windows_301(assessment_visit_weeks(protocol, "weight")), as_is)
    )
  )
  a <- adsl[match(out$USUBJID, adsl$USUBJID), ]
  # A missing visit is judged at its target day.
  day <- ifelse(is.na(out$ADT), out$RANDDT + out$AWTARGET - 1, out$ADT)
  out$ONTRTFL <- ifelse(!is.na(a$TRTEDT) & day <= a$TRTEDT + 7 & !is.na(a$TRTSDT) & day >= a$TRTSDT, "Y", "N")
  out$ONTRTFL[!is.na(out$ABLFL)] <- NA
  out$PRERESFL <- ifelse(is.na(a$RESCDT) | day <= a$RESCDT, "Y", "N")
  out$PRERESFL[!is.na(out$ABLFL)] <- NA
  out$ANL02FL <- ifelse(
    !is.na(out$ANL01FL) & is.na(out$ABLFL) & out$ONTRTFL == "Y" & out$PRERESFL == "Y" & !is.na(out$AVAL), "Y", NA
  )
  out$STUDYID <- a$STUDYID
  out$TRTP <- a$TRT01P
  out$TRTPN <- a$TRT01PN
  out$ARMCD <- a$ARMCD
  out$STRAT1R <- a$STRAT1R
  out$FASFL <- a$FASFL
  out$PPROTFL <- if ("PPROTFL" %in% names(a)) a$PPROTFL else NA
  out <- out[order(out$USUBJID, out$PARAMN, out$AVISITN, out$ADT), ]
  adam_conform(out, spec, "ADEFF")
}

#' Add the per-protocol flag to ADSL and ADEFF (SAP PPS): full analysis set, compliance of
#' at least 80%, and a week-26 HbA1c on treatment and before rescue. Protocol deviations are
#' not collected, so none excludes a patient; the reviewer's guide says so.
#'
#' @param adsl The ADSL dataset.
#' @param adeff The ADEFF dataset.
#' @param protocol The protocol design, for the primary visit.
#' @return A list with `adsl` and `adeff`.
add_pps_301 <- function(adsl, adeff, protocol) {
  w26 <- adeff[adeff$PARAMCD == "HBA1C" & adeff$AVISITN == protocol$primary_visit_week & !is.na(adeff$ANL02FL), ]
  adsl$PPROTFL <- ifelse(
    adsl$FASFL == "Y" & !is.na(adsl$COMPLPCT) & adsl$COMPLPCT >= 80 & adsl$USUBJID %in% w26$USUBJID, "Y", "N"
  )
  adeff$PPROTFL <- adsl$PPROTFL[match(adeff$USUBJID, adsl$USUBJID)]
  list(adsl = adsl, adeff = adeff)
}

#' ADEFF records in the form the efficacy models read (R/efficacy_301.R): the full analysis
#' set, one record per subject and scheduled post-baseline visit, STRATUM for STRAT1R.
#'
#' @param adeff The ADEFF dataset.
#' @param paramcd Parameter code, "HBA1C" or "WEIGHT".
#' @return A data frame.
adeff_analysis_301 <- function(adeff, paramcd) {
  d <- adeff[adeff$PARAMCD == paramcd & !is.na(adeff$ANL01FL) & adeff$AVISITN > 0 & adeff$FASFL == "Y", ]
  d$STRATUM <- d$STRAT1R
  as.data.frame(d[c(
    "USUBJID", "ARMCD", "STRATUM", "PARAMCD", "BASE", "AVISITN", "AVISIT", "AVAL", "CHG", "ONTRTFL", "PRERESFL"
  )])
}
