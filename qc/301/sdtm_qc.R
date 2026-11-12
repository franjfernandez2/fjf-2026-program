# Independent second programming of the SDTM of FJF-2026-301, for QC (F3 day 5).
#
# Written from docs/specs/sdtm-301.yml and cleaning-rules.yml, in base R, without the
# production programs (studies/301/sdtm) or their helpers: it reads the raw CSV files itself
# and has its own date conversion, coding lookup and derivations. The only shared input is
# the assignment in use (dummy until the lock, ADR 0027). Both programs are written by the
# same author, which limits how independent they are (F3 risk register).
#
# Domains: DM, SE, DS, IE, EX, CM, AE, LB, VS, CE. SV, DA, the SUPP-- datasets and the trial
# design domains are not double programmed: they are copies or reshapes of one raw column or
# of the protocol file, and their tests check them.

qc_months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

qc_read <- function(dir, table) {
  utils::read.csv(file.path(dir, paste0(table, ".csv")), colClasses = "character", na.strings = "")
}

qc_iso <- function(x) {
  out <- rep(NA_character_, length(x))
  p <- strsplit(ifelse(is.na(x), "", x), "-", fixed = TRUE)
  for (i in seq_along(x)) {
    v <- p[[i]]
    if (length(v) == 3) {
      m <- match(v[2], qc_months)
      mm <- sprintf("%02d", m)
      out[i] <- if (is.na(m)) {
        v[3]
      } else if (v[1] %in% c("UN", "UNK")) {
        paste(v[3], mm, sep = "-")
      } else {
        paste(v[3],
          mm, v[1],
          sep = "-"
        )
      }
    } else if (length(v) == 1 && grepl("^[0-9]{4}$", v)) {
      out[i] <- v
    }
  }
  out
}

qc_date <- function(dtc) as.Date(ifelse(!is.na(dtc) & nchar(dtc) == 10, dtc, NA))

qc_dy <- function(dtc, ref) {
  d <- as.numeric(qc_date(dtc) - qc_date(ref))
  as.integer(ifelse(d >= 0, d + 1, d))
}

qc_seq <- function(id) as.integer(stats::ave(seq_along(id), id, FUN = seq_along))

qc_epoch <- function(id, dtc, se) {
  d <- qc_date(dtc)
  out <- rep(NA_character_, length(id))
  # A day two elements share (first or last dose) belongs to treatment.
  for (k in c(which(se$EPOCH == "TREATMENT"), which(se$EPOCH != "TREATMENT"))) {
    hit <- id == se$USUBJID[k] & !is.na(d) & d >= qc_date(se$SESTDTC[k]) &
      (is.na(se$SEENDTC[k]) | d <= qc_date(se$SEENDTC[k]))
    out[hit & is.na(out)] <- se$EPOCH[k]
  }
  out
}

#' Second programming of the SDTM domains of FJF-2026-301.
#'
#' @param dir Folder of the study's raw CSV files.
#' @param assignment The assignment in use ([assignment_in_use()]).
#' @param protocol The protocol design.
#' @param spec The SDTM specification.
#' @param dictionary The coding dictionary.
#' @return A named list of data frames.
qc_sdtm_301 <- function(dir, assignment, protocol, spec, dictionary) {
  st <- spec$study
  uid <- function(x) paste0(st, "-", x)
  enr <- qc_read(dir, "enrolment")
  dem <- qc_read(dir, "demography")
  dsp <- qc_read(dir, "disposition")
  exr <- qc_read(dir, "exposure")
  rnd <- assignment$randomisation
  arm_code <- vapply(protocol$arms, \(a) a$code, "")
  arm_label <- vapply(protocol$arms, \(a) a$label, "")
  arm_dose <- vapply(protocol$arms, \(a) as.numeric(a$dose_mg), 0)

  # DM --------------------------------------------------------------------------------
  first <- tapply(as.character(qc_date(qc_iso(exr$dispense_date))), exr$subject_id, min)
  trt_end <- dsp[dsp$event == "TREATMENT", ]
  st_end <- dsp[dsp$event == "STUDY", ]
  dates <- list(
    enr[c("subject_id", "consent_date")], enr[c("subject_id", "screening_date")],
    qc_read(dir, "visits")[c("subject_id", "visit_date")], qc_read(dir, "lab_results")[c(
      "subject_id",
      "collection_date"
    )],
    qc_read(dir, "vital_signs")[c("subject_id", "measure_date")], exr[c("subject_id", "dispense_date")],
    exr[c("subject_id", "return_date")], dsp[c("subject_id", "event_date")],
    qc_read(dir, "adverse_events")[c("subject_id", "start_date")], qc_read(dir, "adverse_events")[c(
      "subject_id",
      "end_date"
    )],
    qc_read(dir, "hypoglycaemia")[c("subject_id", "event_date")]
  )
  all_dates <- do.call(rbind, lapply(dates, \(d) data.frame(id = d[[1]], d = qc_date(qc_iso(d[[2]])))))
  all_dates <- all_dates[!is.na(all_dates$d), ]
  last <- tapply(as.character(all_dates$d), all_dates$id, max)
  ids <- sort(enr$subject_id)
  e <- enr[match(ids, enr$subject_id), ]
  g <- dem[match(ids, dem$subject_id), ]
  arm <- rnd$arm_code[match(ids, rnd$subject_id)]
  dosed <- !is.na(arm) & !is.na(first[ids])
  rfx_end <- ifelse(dosed, qc_iso(trt_end$event_date[match(ids, trt_end$subject_id)]), NA)
  death <- st_end$reason[match(ids, st_end$subject_id)]
  birth <- qc_iso(g$birth_date)
  consent <- qc_iso(e$consent_date)
  mid <- ifelse(nchar(birth) == 4, paste0(birth, "-07-01"), ifelse(nchar(birth) == 7, paste0(birth, "-15"), birth))
  b <- as.POSIXlt(qc_date(mid))
  cdt <- as.POSIXlt(qc_date(consent))
  age <- cdt$year - b$year - ((cdt$mon * 100 + cdt$mday) < (b$mon * 100 + b$mday))
  lbl <- arm_label[match(arm, arm_code)]
  dm <- data.frame(
    STUDYID = st, DOMAIN = "DM", USUBJID = uid(ids), SUBJID = ids,
    RFSTDTC = ifelse(dosed, first[ids], NA), RFENDTC = rfx_end, RFXSTDTC = ifelse(dosed, first[ids], NA),
    RFXENDTC = rfx_end, RFICDTC = consent, RFPENDTC = unname(last[ids]),
    DTHDTC = ifelse(!is.na(death) & death == "DEATH", qc_iso(st_end$event_date[match(ids, st_end$subject_id)]), NA),
    DTHFL = ifelse(!is.na(death) & death == "DEATH", "Y", NA), SITEID = e$site_id, BRTHDTC = birth,
    AGE = as.integer(age), AGEU = ifelse(is.na(birth), NA, "YEARS"), SEX = g$sex, RACE = g$race, ETHNIC = g$ethnicity,
    ARMCD = arm, ARM = lbl, ACTARMCD = ifelse(dosed, arm, NA), ACTARM = ifelse(dosed, lbl, NA),
    ARMNRS = ifelse(is.na(arm), "SCREEN FAILURE", ifelse(dosed, NA, "ASSIGNED, NOT TREATED")),
    ACTARMUD = NA_character_, COUNTRY = NA_character_
  )

  # SE --------------------------------------------------------------------------------
  se <- rbind(
    data.frame(
      USUBJID = dm$USUBJID, ETCD = "SCRN", ELEMENT = "Screening", TAETORD = 1L, EPOCH = "SCREENING",
      SESTDTC = dm$RFICDTC, SEENDTC = ifelse(is.na(dm$RFXSTDTC), dm$RFPENDTC, dm$RFXSTDTC)
    ),
    data.frame(
      USUBJID = dm$USUBJID, ETCD = dm$ACTARMCD, ELEMENT = dm$ACTARM, TAETORD = 2L, EPOCH = "TREATMENT",
      SESTDTC = dm$RFXSTDTC, SEENDTC = dm$RFXENDTC
    )[!is.na(dm$ACTARMCD), ],
    data.frame(
      USUBJID = dm$USUBJID, ETCD = "FU", ELEMENT = "Follow-up", TAETORD = 3L, EPOCH = "FOLLOW-UP",
      SESTDTC = dm$RFXENDTC, SEENDTC = dm$RFPENDTC
    )[!is.na(dm$RFXENDTC) & dm$RFPENDTC > dm$RFXENDTC, ]
  )
  se <- se[order(se$USUBJID, se$TAETORD), ]
  ref <- function(id) dm$RFSTDTC[match(id, dm$USUBJID)]
  se <- data.frame(
    STUDYID = st, DOMAIN = "SE", USUBJID = se$USUBJID, SESEQ = qc_seq(se$USUBJID), se[-1],
    SESTDY = qc_dy(se$SESTDTC, ref(se$USUBJID)), SEENDY = qc_dy(se$SEENDTC, ref(se$USUBJID))
  )

  # DS --------------------------------------------------------------------------------
  sf <- enr[enr$eligible == "N", ]
  scat <- ifelse(dsp$event == "TREATMENT", "STUDY TREATMENT", "STUDY PARTICIPATION")
  dterm <- ifelse(dsp$status == "COMPLETED", "COMPLETED", dsp$reason)
  ds <- rbind(
    data.frame(
      id = enr$subject_id, t = "INFORMED CONSENT OBTAINED", c = "PROTOCOL MILESTONE", s = NA,
      d = enr$consent_date
    ),
    data.frame(id = rnd$subject_id, t = "RANDOMIZED", c = "PROTOCOL MILESTONE", s = NA, d = rnd$randomisation_date),
    data.frame(
      id = sf$subject_id, t = "SCREEN FAILURE", c = "DISPOSITION EVENT", s = "STUDY PARTICIPATION",
      d = sf$screening_date
    ),
    data.frame(id = dsp$subject_id, t = dterm, c = "DISPOSITION EVENT", s = scat, d = dsp$event_date)
  )
  ds$term <- ds$t
  ds$term[ds$t == "SCREEN FAILURE"] <- ifelse(is.na(sf$failed_criterion), "SCREEN FAILURE", sf$failed_criterion)
  ds$USUBJID <- uid(ds$id)
  ds$DSSTDTC <- qc_iso(ds$d)
  ds <- ds[order(ds$USUBJID, ds$DSSTDTC, ds$c != "PROTOCOL MILESTONE"), ]
  ds <- data.frame(
    STUDYID = st, DOMAIN = "DS", USUBJID = ds$USUBJID, DSSEQ = qc_seq(ds$USUBJID), DSTERM = ds$term,
    DSDECOD = ds$t, DSCAT = ds$c, DSSCAT = ds$s, EPOCH = qc_epoch(ds$USUBJID, ds$DSSTDTC, se), DSSTDTC = ds$DSSTDTC,
    DSSTDY = qc_dy(ds$DSSTDTC, ref(ds$USUBJID))
  )

  # IE --------------------------------------------------------------------------------
  inc <- unlist(protocol$eligibility$inclusion)
  exc <- unlist(protocol$eligibility$exclusion)
  crit <- data.frame(
    code = c(sprintf("IN%02d", seq_along(inc)), sprintf("EX%02d", seq_along(exc))), text = c(inc, exc),
    cat = c(rep("INCLUSION", length(inc)), rep("EXCLUSION", length(exc)))
  )
  f <- sf[!is.na(sf$failed_criterion), ]
  k <- match(f$failed_criterion, crit$text)
  ie <- data.frame(
    USUBJID = uid(f$subject_id), IETESTCD = crit$code[k], IETEST = crit$text[k], IECAT = crit$cat[k],
    res = ifelse(crit$cat[k] == "INCLUSION", "N", "Y"), IEDTC = qc_iso(f$screening_date)
  )
  ie <- ie[order(ie$USUBJID, ie$IETESTCD), ]
  ie <- data.frame(
    STUDYID = st, DOMAIN = "IE", USUBJID = ie$USUBJID, IESEQ = qc_seq(ie$USUBJID), ie[2:4],
    IEORRES = ie$res, IESTRESC = ie$res, EPOCH = "SCREENING", IEDTC = ie$IEDTC, IEDY = qc_dy(ie$IEDTC, ref(ie$USUBJID))
  )

  # EX --------------------------------------------------------------------------------
  x <- exr
  x$USUBJID <- uid(x$subject_id)
  x$start <- qc_iso(x$dispense_date)
  x <- x[order(x$USUBJID, x$start), ]
  dose <- arm_dose[match(rnd$arm_code[match(x$subject_id, rnd$subject_id)], arm_code)]
  last_dose <- qc_date(dm$RFXENDTC[match(x$USUBJID, dm$USUBJID)])
  end <- qc_date(qc_iso(x$return_date)) - 1
  end <- as.Date(ifelse(!is.na(end) & !is.na(last_dose) & end > last_dose, last_dose, end))
  is_last <- !duplicated(x$USUBJID, fromLast = TRUE)
  end[is.na(end) & is_last] <- last_dose[is.na(end) & is_last]
  # CR10: the disposition's last dose is kept; the last bottle runs until it.
  late <- is_last & !is.na(end) & !is.na(last_dose) & end < last_dose
  end[late] <- last_dose[late]
  vnum <- match(x$visit_code, protocol$visits$code)
  ex <- data.frame(
    STUDYID = st, DOMAIN = "EX", USUBJID = x$USUBJID, EXSEQ = qc_seq(x$USUBJID), EXREFID = x$record_id,
    EXTRT = ifelse(dose == 0, "PLACEBO", "FJF-2026"), EXDOSE = dose, EXDOSU = "mg", EXDOSFRM = "TABLET",
    EXDOSFRQ = "QD",
    EXROUTE = "ORAL", VISITNUM = as.numeric(vnum), VISIT = toupper(protocol$visits$label[vnum]),
    EPOCH = qc_epoch(x$USUBJID, x$start, se), EXSTDTC = x$start, EXENDTC = as.character(end),
    EXSTDY = qc_dy(x$start, ref(x$USUBJID)), EXENDY = qc_dy(as.character(end), ref(x$USUBJID))
  )

  # Coding lookups ---------------------------------------------------------------------
  lookup <- function(entries, field) {
    keys <- unlist(lapply(entries, \(e) tolower(unlist(e$verbatim))))
    vals <- unlist(lapply(entries, \(e) rep(e[[field]], length(e$verbatim))))
    function(x) vals[match(tolower(trimws(x)), keys)]
  }

  # CM --------------------------------------------------------------------------------
  c0 <- qc_read(dir, "concomitant_meds")
  c0$USUBJID <- uid(c0$subject_id)
  c0$start <- qc_iso(c0$start_date)
  c0 <- c0[order(c0$USUBJID, c0$start, as.integer(c0$cm_seq)), ]
  med <- dictionary$medications
  dose_txt <- ifelse(is.na(c0$daily_dose), NA, c0$daily_dose)
  cend <- qc_iso(c0$end_date)
  cm <- data.frame(
    STUDYID = st, DOMAIN = "CM", USUBJID = c0$USUBJID, CMSEQ = qc_seq(c0$USUBJID), CMSPID = c0$cm_seq,
    CMTRT = c0$drug_name, CMDECOD = lookup(med, "decod")(c0$drug_name),
    CMCAT = ifelse(c0$rescue == "Y", "RESCUE MEDICATION", "BACKGROUND ANTIDIABETIC THERAPY"), CMINDC = c0$indication,
    CMCLAS = lookup(med, "class")(c0$drug_name), CMCLASCD = lookup(med, "atc")(c0$drug_name),
    CMDOSU = ifelse(is.na(dose_txt), NA, tolower(sapply(strsplit(dose_txt, " "), `[`, 2))),
    CMDOSTOT = as.numeric(sapply(strsplit(ifelse(is.na(dose_txt), "NA", dose_txt), " "), `[`, 1)),
    EPOCH = qc_epoch(c0$USUBJID, c0$start, se), CMSTDTC = c0$start, CMENDTC = cend,
    CMSTDY = qc_dy(c0$start, ref(c0$USUBJID)), CMENDY = qc_dy(cend, ref(c0$USUBJID)),
    CMENRTPT = ifelse(is.na(cend), "ONGOING", NA), CMENTPT = ifelse(is.na(cend), "END OF STUDY", NA)
  )

  # AE --------------------------------------------------------------------------------
  a <- qc_read(dir, "adverse_events")
  a$USUBJID <- uid(a$subject_id)
  a$start <- qc_iso(a$start_date)
  a <- a[order(a$USUBJID, a$start, as.integer(a$ae_seq)), ]
  aend <- qc_iso(a$end_date)
  bad <- !is.na(qc_date(aend)) & !is.na(qc_date(a$start)) & qc_date(aend) < qc_date(a$start)
  aend[bad] <- NA
  terms <- dictionary$adverse_events
  soc <- lookup(terms, "soc")(a$verbatim_term)
  ae <- data.frame(
    STUDYID = st, DOMAIN = "AE", USUBJID = a$USUBJID, AESEQ = qc_seq(a$USUBJID), AESPID = a$ae_seq,
    AETERM = a$verbatim_term, AELLT = lookup(terms, "llt")(a$verbatim_term), AEDECOD = lookup(
      terms,
      "pt"
    )(a$verbatim_term),
    AEBODSYS = soc, AESOC = soc, AESEV = a$severity, AESER = a$serious, AEACN = a$action_taken,
    AEREL = ifelse(a$related == "Y", "RELATED", "NOT RELATED"),
    AEOUT = ifelse(a$outcome == "RECOVERED", "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED"),
    EPOCH = qc_epoch(a$USUBJID, a$start, se), AESTDTC = a$start, AEENDTC = aend,
    AESTDY = qc_dy(a$start, ref(a$USUBJID)), AEENDY = qc_dy(aend, ref(a$USUBJID)),
    AEENRTPT = ifelse(bad, "UNKNOWN", ifelse(is.na(aend), "ONGOING", NA)),
    AEENTPT = ifelse(bad | is.na(aend), "END OF STUDY", NA)
  )

  # Last observation on or before the first dose, per subject and test.
  lobx <- function(id, test, dtc, value) {
    fd <- qc_date(dm$RFXSTDTC[match(id, dm$USUBJID)])
    d <- qc_date(dtc)
    ok <- which(!is.na(value) & !is.na(d) & !is.na(fd) & d <= fd)
    ok <- ok[order(id[ok], test[ok], d[ok])]
    flag <- rep(NA_character_, length(id))
    flag[ok[!duplicated(paste(id[ok], test[ok]), fromLast = TRUE)]] <- "Y"
    flag
  }
  first_entry <- function(d, key) {
    d <- d[order(as.integer(d$record_id)), ]
    d[!duplicated(d[key]), ]
  }

  # LB --------------------------------------------------------------------------------
  l <- first_entry(qc_read(dir, "lab_results"), c("subject_id", "visit_code", "test_code", "collection_date"))
  tests <- spec$lab_tests
  ti <- match(l$test_code, vapply(tests, \(t) t$code, ""))
  su <- vapply(tests, \(t) t$unit, "")[ti]
  fac <- ifelse(l$unit == su, 1, NA)
  off <- ifelse(l$unit == su, 0, NA)
  for (i in seq_along(tests)) {
    for (cv in tests[[i]]$convert) {
      hit <- ti == i & l$unit == cv$from
      fac[hit] <- cv$factor
      off[hit] <- cv$offset
    }
  }
  num <- suppressWarnings(as.numeric(l$result))
  sn <- round(num * fac + off, 4)
  lo <- round(as.numeric(l$ref_low) * fac + off, 4)
  hi <- round(as.numeric(l$ref_high) * fac + off, 4)
  l$USUBJID <- uid(l$subject_id)
  l$dtc <- qc_iso(l$collection_date)
  vn <- match(l$visit_code, protocol$visits$code)
  lb <- data.frame(
    STUDYID = st, DOMAIN = "LB", USUBJID = l$USUBJID, LBREFID = l$record_id,
    LBTESTCD = vapply(tests, \(t) if (is.null(t$testcd)) t$code else t$testcd, "")[ti],
    LBTEST = vapply(tests, \(t) t$name, "")[ti], LBCAT = vapply(tests, \(t) t$category, "")[ti],
    LBORRES = l$result, LBORRESU = l$unit, LBORNRLO = l$ref_low, LBORNRHI = l$ref_high,
    LBSTRESC = ifelse(is.na(num), l$result, ifelse(is.na(sn), NA, format(sn, trim = TRUE, drop0trailing = TRUE))),
    LBSTRESN = sn, LBSTRESU = su, LBSTNRLO = lo, LBSTNRHI = hi,
    LBNRIND = ifelse(
      is.na(sn), NA, ifelse(!is.na(lo) & sn < lo, "LOW", ifelse(!is.na(hi) & sn > hi, "HIGH", "NORMAL"))
    ),
    LBLOBXFL = lobx(l$USUBJID, l$test_code, l$dtc, sn), LBFAST = ifelse(!is.na(l$fasting) & l$fasting == "Y", "Y", NA),
    VISITNUM = as.numeric(vn), VISIT = toupper(protocol$visits$label[vn]), EPOCH = qc_epoch(l$USUBJID, l$dtc, se),
    LBDTC = l$dtc, LBDY = qc_dy(l$dtc, ref(l$USUBJID))
  )
  lb <- lb[order(lb$USUBJID, lb$LBTESTCD, lb$VISITNUM, lb$LBDTC), ]
  lb <- data.frame(lb[1:3], LBSEQ = qc_seq(lb$USUBJID), lb[-(1:3)])

  # VS --------------------------------------------------------------------------------
  v <- first_entry(qc_read(dir, "vital_signs"), c("subject_id", "visit_code"))
  vt <- spec$vital_signs
  rows <- list()
  for (t in vt[vapply(vt, \(t) t$code != "HEIGHT", TRUE)]) {
    u <- if (t$code == "WEIGHT") v$weight_unit else rep(t$unit, nrow(v))
    rows[[t$code]] <- data.frame(
      id = v$subject_id, vc = v$visit_code, d = v$measure_date, code = t$code, name = t$name,
      o = v[[t$column]], u = u, std = t$unit
    )
  }
  h <- as.numeric(dem$height_cm)
  rows$HEIGHT <- data.frame(
    id = dem$subject_id, vc = "V1", d = enr$screening_date[match(dem$subject_id, enr$subject_id)],
    code = "HEIGHT", name = "Height", o = dem$height_cm, u = ifelse(!is.na(h) & h < 3, "m", "cm"), std = "cm"
  )
  w <- do.call(rbind, rows)
  w <- w[!is.na(w$o), ]
  val <- as.numeric(w$o)
  sn <- ifelse(w$code == "WEIGHT" & w$u == "lb", round(val / 2.20462, 1), ifelse(w$code == "HEIGHT" & w$u == "m",
    val * 100, val
  ))
  sn[w$code == "HEIGHT" & (sn < 100 | sn > 230)] <- NA
  w$USUBJID <- uid(w$id)
  w$dtc <- qc_iso(w$d)
  vn <- match(w$vc, protocol$visits$code)
  vs <- data.frame(
    STUDYID = st, DOMAIN = "VS", USUBJID = w$USUBJID, VSTESTCD = w$code, VSTEST = w$name, VSORRES = w$o,
    VSORRESU = ifelse(w$u == "lb", "LB", w$u), VSSTRESC = ifelse(is.na(sn), NA, format(sn,
      trim = TRUE,
      drop0trailing = TRUE
    )),
    VSSTRESN = sn, VSSTRESU = w$std, VSLOBXFL = lobx(w$USUBJID, w$code, w$dtc, sn), VISITNUM = as.numeric(vn),
    VISIT = toupper(protocol$visits$label[vn]), EPOCH = qc_epoch(w$USUBJID, w$dtc, se), VSDTC = w$dtc,
    VSDY = qc_dy(w$dtc, ref(w$USUBJID))
  )
  vs <- vs[order(vs$USUBJID, vs$VSTESTCD, vs$VISITNUM), ]
  vs <- data.frame(vs[1:3], VSSEQ = qc_seq(vs$USUBJID), vs[-(1:3)])

  # CE --------------------------------------------------------------------------------
  hy <- qc_read(dir, "hypoglycaemia")
  hy$USUBJID <- uid(hy$subject_id)
  hy$dtc <- qc_iso(hy$event_date)
  hy <- hy[order(hy$USUBJID, hy$dtc, as.integer(hy$event_seq)), ]
  ce <- data.frame(
    STUDYID = st, DOMAIN = "CE", USUBJID = hy$USUBJID, CESEQ = qc_seq(hy$USUBJID), CESPID = hy$event_seq,
    CETERM = "HYPOGLYCAEMIA", CEDECOD = "Hypoglycaemia", CECAT = "HYPOGLYCAEMIA", CESCAT = paste("LEVEL", hy$level),
    CEPRESP = "Y", CEOCCUR = "Y", EPOCH = qc_epoch(hy$USUBJID, hy$dtc, se), CESTDTC = hy$dtc,
    CESTDY = qc_dy(hy$dtc, ref(hy$USUBJID))
  )

  list(DM = dm, SE = se, DS = ds, IE = ie, EX = ex, CM = cm, AE = ae, LB = lb, VS = vs, CE = ce)
}

#' Compare the production SDTM with the second programming, domain by domain.
#'
#' @param production Named list of the production datasets.
#' @param qc Named list from [qc_sdtm_301()].
#' @param spec The SDTM specification, for the record keys.
#' @return A data frame with one row per domain: rows, variables and differences.
compare_sdtm_301 <- function(production, qc, spec) {
  out <- lapply(names(qc), \(d) {
    p <- as.data.frame(production[[d]])
    q <- qc[[d]][names(p)]
    for (v in names(p)) if (is.numeric(p[[v]])) q[[v]] <- as.numeric(q[[v]]) else q[[v]] <- as.character(q[[v]])
    for (v in names(p)) if (is.integer(p[[v]])) p[[v]] <- as.numeric(p[[v]])
    seq <- grep("SEQ$", names(p), value = TRUE)
    keys <- c("USUBJID", seq)
    diff <- diffdf::diffdf(p, q, keys = keys, suppress_warnings = TRUE)
    data.frame(
      domain = d, rows = nrow(p), qc_rows = nrow(q), differences = length(diff),
      clean = !diffdf::diffdf_has_issues(diff)
    )
  })
  do.call(rbind, out)
}
