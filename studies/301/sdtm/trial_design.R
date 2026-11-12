# SDTM trial design domains of FJF-2026-301 (TA, TE, TV, TI, TS), built from the protocol
# file, and the subject-level SE and IE. EPOCH is assigned from SE.

# Elements of the study: screening, the treatment of each arm, follow-up.
elements_301 <- function(protocol) {
  arms <- protocol$arms
  data.frame(
    ETCD = c("SCRN", purrr::map_chr(arms, "code"), "FU"),
    ELEMENT = c("Screening", purrr::map_chr(arms, "label"), "Follow-up"),
    EPOCH = c("SCREENING", rep("TREATMENT", length(arms)), "FOLLOW-UP"),
    TESTRL = c(
      "Informed consent", rep("First dose of study drug", length(arms)), "Day after the last dose of study drug"
    ),
    TEENRL = c(
      "Day before the first dose, or screen failure",
      rep(sprintf("Last dose of study drug (planned %d weeks)", protocol$treatment_weeks), length(arms)),
      sprintf("End of participation (planned %d weeks)", protocol$follow_up_weeks)
    ),
    TEDUR = c("P2W", rep(sprintf("P%dW", protocol$treatment_weeks), length(arms)), sprintf(
      "P%dW",
      protocol$follow_up_weeks
    ))
  )
}

#' TE: trial elements.
#' @param protocol The protocol design.
#' @param spec The SDTM specification.
#' @return The TE dataset.
map_te_301 <- function(protocol, spec) {
  e <- elements_301(protocol)
  sdtm_conform(cbind(STUDYID = spec$study, DOMAIN = "TE", e), spec, "TE")
}

#' TA: trial arms, three elements each.
#' @inheritParams map_te_301
#' @return The TA dataset.
map_ta_301 <- function(protocol, spec) {
  e <- elements_301(protocol)
  rows <- purrr::map(protocol$arms, \(a) {
    el <- e[e$ETCD %in% c("SCRN", a$code, "FU"), ]
    data.frame(
      STUDYID = spec$study, DOMAIN = "TA", ARMCD = a$code, ARM = a$label, TAETORD = 1:3, ETCD = el$ETCD,
      ELEMENT = el$ELEMENT, TABRANCH = c("Randomised to the arm", NA, NA), TATRANS = NA, EPOCH = el$EPOCH
    )
  })
  sdtm_conform(purrr::list_rbind(rows), spec, "TA")
}

#' TV: planned visits, with the planned study day (day 1 = randomisation, week 0).
#' @inheritParams map_te_301
#' @return The TV dataset.
map_tv_301 <- function(protocol, spec) {
  v <- protocol$visits
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "TV", VISITNUM = seq_len(nrow(v)), VISIT = toupper(v$label), ARMCD = NA,
    VISITDY = ifelse(v$week < 0, v$week * 7, v$week * 7 + 1),
    TVSTRL = sprintf("Week %s, window +/- %s days", v$week, v$window_days)
  )
  sdtm_conform(out, spec, "TV")
}

# Criteria of the protocol with their codes: IN01.. and EX01.., in protocol order.
criteria_301 <- function(protocol) {
  inc <- unlist(protocol$eligibility$inclusion)
  exc <- unlist(protocol$eligibility$exclusion)
  data.frame(
    IETESTCD = c(sprintf("IN%02d", seq_along(inc)), sprintf("EX%02d", seq_along(exc))),
    IETEST = c(inc, exc), IECAT = rep(c("INCLUSION", "EXCLUSION"), c(length(inc), length(exc)))
  )
}

#' TI: trial inclusion and exclusion criteria.
#' @inheritParams map_te_301
#' @return The TI dataset.
map_ti_301 <- function(protocol, spec) {
  sdtm_conform(cbind(STUDYID = spec$study, DOMAIN = "TI", criteria_301(protocol)), spec, "TI")
}

#' TS: trial summary parameters. Values with CDISC terminology carry its code (CT 2024-03-29).
#' @inheritParams map_te_301
#' @param dm The DM dataset, for the actual number of subjects and the study dates.
#' @param planned_subjects Planned number of randomised subjects.
#' @param secondary Labels of the key secondary endpoints of the SAP.
#' @return The TS dataset.
map_ts_301 <- function(protocol, spec, dm, planned_subjects, secondary) {
  ct <- "CDISC CT"
  p <- function(code, name, value, valcd = NA, ref = NA, nf = NA) {
    data.frame(TSPARMCD = code, TSPARM = name, TSVAL = value, TSVALNF = nf, TSVALCD = valcd, TSVCDREF = ref)
  }
  # A parameter that cannot have a value in a fictional study: empty, with its null flavor.
  none <- function(code, name, nf) p(code, name, NA, nf = nf)
  randomised <- !is.na(dm$ARMCD)
  ts <- rbind(
    p("TITLE", "Trial Title", gsub("\\s+", " ", protocol$title)),
    p("TPHASE", "Trial Phase Classification", "PHASE III TRIAL", "C15602", ct),
    p("TTYPE", "Trial Type", c("EFFICACY", "SAFETY"), c("C49666", "C49667"), ct),
    p("STYPE", "Study Type", "INTERVENTIONAL", "C98388", ct),
    p("INTMODEL", "Intervention Model", "PARALLEL", "C82639", ct),
    p("INTTYPE", "Intervention Type", "DRUG", "C1909", ct),
    p("TBLIND", "Trial Blinding Schema", "DOUBLE BLIND", "C15228", ct),
    p("TCNTRL", "Control Type", "PLACEBO", "C49648", ct),
    p("TINDTP", "Trial Intent Type", "TREATMENT", "C49656", ct),
    p("RANDOM", "Trial is Randomized", "Y", "C49488", ct),
    p("ADDON", "Added on to Existing Treatments", "Y", "C49488", ct),
    p("ADAPT", "Adaptive Design", "N", "C49487", ct),
    p("HLTSUBJI", "Healthy Subject Indicator", "N", "C49487", ct),
    p("EXTTIND", "Extension Trial Indicator", "N", "C49487", ct),
    p("SEXPOP", "Sex of Participants", "BOTH", "C49636", ct),
    p("AGEMIN", "Planned Minimum Age of Subjects", sprintf("P%dY", protocol$eligibility$age_years[[1]])),
    p("AGEMAX", "Planned Maximum Age of Subjects", sprintf("P%dY", protocol$eligibility$age_years[[2]])),
    p("LENGTH", "Trial Length", sprintf("P%dW", 2 + protocol$treatment_weeks + protocol$follow_up_weeks)),
    p("NARMS", "Planned Number of Arms", as.character(length(protocol$arms))),
    p("PLANSUB", "Planned Number of Subjects", as.character(planned_subjects)),
    p("ACTSUB", "Actual Number of Subjects", as.character(sum(randomised))),
    p("TDIGRP", "Diagnosis Group", "Type 2 diabetes mellitus"),
    p("INDIC", "Trial Disease/Condition Indication", "Type 2 diabetes mellitus inadequately controlled on metformin"),
    p("TRT", "Investigational Therapy or Treatment", "FJF-2026"),
    p("COMPTRT", "Comparative Treatment Name", "PLACEBO"),
    p("CURTRT", "Current Therapy or Treatment", "METFORMIN"),
    p("OBJPRIM", "Trial Primary Objective", "Change in HbA1c from baseline to week 26, FJF-2026 20 mg against placebo"),
    p("OBJSEC", "Trial Secondary Objective", secondary),
    p("OUTMSPRI", "Primary Outcome Measure", "Change from baseline in HbA1c at week 26"),
    p("STOPRULE", "Study Stop Rules", "None: there is no interim analysis"),
    p("THERAREA", "Therapeutic Area", "Endocrinology"),
    p("PDPSTIND", "Pediatric Postmarket Study Indicator", "N", "C49487", ct),
    p("PDSTIND", "Pediatric Study Indicator", "N", "C49487", ct),
    p("PIPIND", "Pediatric Investigation Plan Indicator", "N", "C49487", ct),
    p("RDIND", "Rare Disease Indicator", "N", "C49487", ct),
    p("DCUTDESC", "Data Cutoff Description", "Database lock (git tag dblock-301)"),
    none("DCUTDTC", "Data Cutoff Date", "NAV"),
    none("REGID", "Registry Identifier", "NA"),
    none("PCLAS", "Pharmacologic Class", "NAV"),
    none("FCNTRY", "Planned Country of Investigational Sites", "NASK"),
    none("NCOHORT", "Number of Groups/Cohorts", "NA"),
    p("SPONSOR", "Clinical Study Sponsor", "FJF-2026 programme (fictional sponsor)"),
    p("SSTDTC", "Study Start Date", min(dm$RFICDTC, na.rm = TRUE)),
    p("SENDTC", "Study End Date", max(dm$RFPENDTC, na.rm = TRUE)),
    p("SDTIGVER", "SDTM IG Version", spec$sdtmig),
    p("SDTMVER", "SDTM Version", "2.0")
  )
  # XPT holds 200 characters per value: the rest goes to TSVAL1, split at a space.
  long <- !is.na(ts$TSVAL) & nchar(ts$TSVAL) > 200
  cut <- vapply(ts$TSVAL[long], \(x) max(gregexpr(" ", substr(x, 1, 201))[[1]]), integer(1))
  ts$TSVAL1 <- NA_character_
  ts$TSVAL1[long] <- substring(ts$TSVAL[long], cut + 1)
  ts$TSVAL[long] <- substr(ts$TSVAL[long], 1, cut - 1)
  ts$TSVCDVER <- ifelse(is.na(ts$TSVCDREF), NA, "2024-03-29")
  ts$TSSEQ <- as.integer(stats::ave(seq_len(nrow(ts)), ts$TSPARMCD, FUN = seq_along))
  ts$STUDYID <- spec$study
  ts$DOMAIN <- "TS"
  sdtm_conform(ts, spec, "TS")
}

#' SE: the elements each subject went through, from DM's reference dates.
#' @param dm The DM dataset.
#' @param protocol The protocol design.
#' @param spec The SDTM specification.
#' @return The SE dataset.
map_se_301 <- function(dm, protocol, spec) {
  e <- elements_301(protocol)
  day <- function(dtc, shift) ifelse(is.na(dtc), NA, format(iso_date(dtc) + shift))
  dosed <- !is.na(dm$RFXSTDTC)
  scrn <- data.frame(
    USUBJID = dm$USUBJID, ETCD = "SCRN", SESTDTC = dm$RFICDTC,
    SEENDTC = ifelse(dosed, day(dm$RFXSTDTC, -1), dm$RFPENDTC)
  )
  trt <- data.frame(
    USUBJID = dm$USUBJID[dosed], ETCD = dm$ACTARMCD[dosed], SESTDTC = dm$RFXSTDTC[dosed],
    SEENDTC = dm$RFXENDTC[dosed]
  )
  fu_rows <- dosed & !is.na(dm$RFXENDTC) & !is.na(dm$RFPENDTC) & dm$RFPENDTC > dm$RFXENDTC
  fu <- data.frame(
    USUBJID = dm$USUBJID[fu_rows], ETCD = "FU", SESTDTC = day(dm$RFXENDTC[fu_rows], 1),
    SEENDTC = dm$RFPENDTC[fu_rows]
  )
  se <- rbind(scrn, trt, fu)
  i <- match(se$ETCD, e$ETCD)
  se$ELEMENT <- e$ELEMENT[i]
  se$EPOCH <- e$EPOCH[i]
  se$TAETORD <- match(se$EPOCH, c("SCREENING", "TREATMENT", "FOLLOW-UP"))
  se <- se[order(se$USUBJID, se$TAETORD), ]
  ref <- dm$RFSTDTC[match(se$USUBJID, dm$USUBJID)]
  se$SESTDY <- sdtm_study_day(se$SESTDTC, ref)
  se$SEENDY <- sdtm_study_day(se$SEENDTC, ref)
  se$SESEQ <- sdtm_seq(se$USUBJID)
  se$STUDYID <- spec$study
  se$DOMAIN <- "SE"
  sdtm_conform(se, spec, "SE")
}

#' EPOCH of each record, from the subject's elements: the element whose dates contain the
#' record's date. Partial dates, and dates outside every element, give NA.
#'
#' @param usubjid,dtc Subject and ISO 8601 date of each record.
#' @param se The SE dataset.
#' @return Character vector.
epoch_of <- function(usubjid, dtc, se) {
  date <- iso_date(dtc)
  out <- rep(NA_character_, length(usubjid))
  start <- iso_date(se$SESTDTC)
  end <- iso_date(se$SEENDTC)
  for (k in seq_len(nrow(se))) {
    rows <- is.na(out) & usubjid == se$USUBJID[k] & !is.na(date) & !is.na(start[k]) & date >= start[k] &
      (is.na(end[k]) | date <= end[k])
    out[rows] <- se$EPOCH[k]
  }
  out
}

#' IE: the criterion each screen failure did not meet, matched to TI by its text.
#' @param raw Named list of the study's raw tables.
#' @param dm The DM dataset.
#' @param protocol The protocol design.
#' @param spec The SDTM specification.
#' @return The IE dataset.
map_ie_301 <- function(raw, dm, protocol, spec) {
  enr <- raw$enrolment
  sf <- enr[enr$eligible == "N" & !is.na(enr$failed_criterion), ]
  crit <- criteria_301(protocol)
  i <- match(sf$failed_criterion, crit$IETEST)
  if (anyNA(i)) stop("Failed criteria not in the protocol: ", toString(unique(sf$failed_criterion[is.na(i)])))
  usubjid <- sdtm_usubjid(spec$study, sf$subject_id)
  dtc <- crf_to_iso(sf$screening_date)
  result <- ifelse(crit$IECAT[i] == "INCLUSION", "N", "Y")
  out <- data.frame(
    STUDYID = spec$study, DOMAIN = "IE", USUBJID = usubjid, IETESTCD = crit$IETESTCD[i], IETEST = crit$IETEST[i],
    IECAT = crit$IECAT[i], IEORRES = result, IESTRESC = result, EPOCH = "SCREENING", IEDTC = dtc,
    IEDY = sdtm_study_day(dtc, dm$RFSTDTC[match(usubjid, dm$USUBJID)])
  )
  out <- out[order(out$USUBJID, out$IETESTCD), ]
  out$IESEQ <- sdtm_seq(out$USUBJID)
  sdtm_conform(out, spec, "IE")
}
