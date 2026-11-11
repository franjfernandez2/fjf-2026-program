# Independent QC of ADSL for the CDISC pilot (double programming).
#
# Written from the specification (the pilot's ADaM define.xml), the SAP and the decisions
# in studies/benchmark/discrepancy-log.csv, in base R. It does not use admiral, the
# production program (studies/benchmark/adam/adsl.R) or its helpers (R/adam_helpers.R).
# Only the inputs are shared: the verified SDTM files and the specification tables.
#
# Limit of this independence: the same AI assistant wrote both programs (docs/ai-assistance.md).
# The second program is still a different implementation, so it catches coding errors; it
# cannot catch a misreading of the specification that both programs share.

#' QC derivation of ADSL.
#'
#' @param paths Paths of the downloaded pilot files (`fetch_pilot_files()`).
#' @param spec Output of `read_define_v1()`.
#' @return A data frame with the 48 ADSL variables in specification order, one row per
#'   randomised subject, sorted by USUBJID.
qc_adsl <- function(paths, spec) {
  read <- function(domain) {
    x <- as.data.frame(haven::read_xpt(paths[endsWith(paths, paste0("sdtm/", domain, ".xpt"))]))
    for (v in names(x)) {
      if (is.character(x[[v]])) x[[v]][trimws(x[[v]]) == ""] <- NA
    }
    x
  }
  dm <- read("dm")
  sv <- read("sv")
  ds <- read("ds")
  ex <- read("ex")
  vs <- read("vs")
  sc <- read("sc")
  mh <- read("mh")
  qs <- read("qs")

  out <- dm[dm$ARM != "Screen Failure", ]
  out <- out[order(out$USUBJID), ]
  id <- out$USUBJID

  # One value per subject from the records that meet a condition, after sorting by `by`.
  one <- function(data, keep, value, by = NULL, last = FALSE) {
    data <- data[keep, ]
    if (!is.null(by)) data <- data[order(data$USUBJID, data[[by]], decreasing = c(FALSE, last), method = "radix"), ]
    data <- data[!duplicated(data$USUBJID), ]
    data[[value]][match(id, data$USUBJID)]
  }
  visit <- function(n) as.Date(one(sv, sv$VISITNUM == n, "SVSTDTC", by = "SVSTDTC"))

  # SAS ROUND: halves away from zero.
  r1 <- function(x) trunc(x * 10 + sign(x) * (0.5 + 1e-9)) / 10

  trt <- out$ARM
  armn_codes <- spec$codelists[spec$codelists$code_id == "ARMN", ]
  trtn <- as.numeric(armn_codes$code[match(trt, armn_codes$decode)])

  v1 <- visit(1)
  v3 <- visit(3)
  v4 <- visit(4)
  v8 <- visit(8)
  v10 <- visit(10)
  v12 <- visit(12)

  disp <- ds$DSCAT == "DISPOSITION EVENT"
  dcdecod <- one(ds, disp, "DSDECOD")
  dsterm <- one(ds, disp, "DSTERM")
  dsvisit <- one(ds, disp, "VISITNUM")
  dsdate <- as.Date(one(ds, disp, "DSSTDTC"))

  last_dose <- as.Date(one(ex, rep(TRUE, nrow(ex)), "EXENDTC", by = "EXSTDTC", last = TRUE))
  trtedt <- last_dose
  fill <- is.na(trtedt) & !is.na(dsvisit) & dsvisit > 3
  trtedt[fill] <- dsdate[fill]
  trtdur <- as.numeric(trtedt - v3) + 1

  # High dose: 54 mg to visit 4, 81 mg to visit 12, 54 mg after.
  cumdose <- trtn * trtdur
  hi <- which(trtn == 81)
  for (i in hi) {
    end <- trtedt[i]
    d1 <- if (!is.na(v4[i]) && end >= v4[i]) as.numeric(v4[i] - v3[i]) + 1 else trtdur[i]
    d2 <- 0
    d3 <- 0
    if (!is.na(v4[i]) && end >= v4[i]) {
      d2 <- if (!is.na(v12[i]) && end >= v12[i]) as.numeric(v12[i] - v4[i]) else as.numeric(end - v4[i])
    }
    if (!is.na(v12[i]) && end >= v12[i]) d3 <- as.numeric(end - v12[i])
    cumdose[i] <- 54 * d1 + 81 * d2 + 54 * d3
  }

  agegr1n <- ifelse(out$AGE < 65, 1, ifelse(out$AGE > 80, 3, 2))
  racen_codes <- spec$codelists[spec$codelists$code_id == "RACEN", ]

  reason <- c(
    "COMPLETED" = "Completed", "ADVERSE EVENT" = "Adverse Event", "DEATH" = "Death",
    "LACK OF EFFICACY" = "Lack of Efficacy", "LOST TO FOLLOW-UP" = "Lost to Follow-up",
    "PHYSICIAN DECISION" = "Physician Decision", "PROTOCOL VIOLATION" = "Protocol Violation",
    "STUDY TERMINATED BY SPONSOR" = "Sponsor Decision", "WITHDRAWAL BY SUBJECT" = "Withdrew Consent"
  )
  dcreascd <- unname(reason[dcdecod])
  dcreascd[dcdecod == "PROTOCOL VIOLATION" & dsterm == "PROTOCOL ENTRY CRITERIA NOT MET"] <- "I/E Not Met"

  height <- r1(one(vs, vs$VSTESTCD == "HEIGHT" & vs$VISITNUM == 1, "VSSTRESN"))
  weight <- r1(one(vs, vs$VSTESTCD == "WEIGHT" & vs$VISITNUM == 3, "VSSTRESN"))
  bmi <- r1(weight / (height / 100)^2)
  bmigr <- ifelse(bmi < 25, "<25", ifelse(bmi < 30, "25-<30", ">=30"))

  dison <- as.Date(one(mh, mh$MHCAT == "PRIMARY DIAGNOSIS", "MHSTDTC"))
  durdis <- r1((as.numeric(v1 - dison) + 1) / (365.25 / 12))

  mmse <- qs[qs$QSCAT == "MINI-MENTAL STATE", ]
  mmsetot <- as.numeric(tapply(as.numeric(mmse$QSORRES), mmse$USUBJID, sum)[id])

  has_post <- function(category) id %in% qs$USUBJID[qs$QSCAT == category & qs$VISITNUM > 3]
  rfendt <- as.Date(out$RFENDTC)
  yn <- function(x) ifelse(!is.na(x) & x, "Y", "N")
  blank_y <- function(x) ifelse(!is.na(x) & x, "Y", NA_character_)

  counts <- table(out$SITEID, trt)
  small <- rownames(counts)[apply(counts, 1, min) < 3]

  saffl <- yn(!is.na(v3))
  adas <- has_post("ALZHEIMER'S DISEASE ASSESSMENT SCALE")
  cibic <- has_post("CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)")
  qc <- data.frame(
    STUDYID = out$STUDYID, USUBJID = id, SUBJID = out$SUBJID, SITEID = out$SITEID,
    SITEGR1 = ifelse(out$SITEID %in% small, "900", out$SITEID),
    ARM = trt, TRT01P = trt, TRT01PN = trtn, TRT01A = trt, TRT01AN = trtn,
    TRTSDT = v3, TRTEDT = trtedt, TRTDUR = trtdur,
    AVGDD = r1(cumdose / trtdur), CUMDOSE = r1(cumdose),
    AGE = out$AGE,
    AGEGR1 = c("<65", "65-80", ">80")[agegr1n], AGEGR1N = agegr1n, AGEU = out$AGEU,
    RACE = out$RACE, RACEN = as.numeric(racen_codes$code[match(out$RACE, racen_codes$decode)]),
    SEX = out$SEX, ETHNIC = out$ETHNIC,
    SAFFL = saffl, ITTFL = "Y",
    EFFFL = yn(saffl == "Y" & adas & cibic),
    COMP8FL = yn(rfendt >= v8), COMP16FL = yn(rfendt >= v10), COMP24FL = yn(rfendt >= v12),
    DISCONFL = blank_y(dcreascd != "Completed"), DSRAEFL = blank_y(dcreascd == "Adverse Event"),
    DTHFL = out$DTHFL,
    BMIBL = bmi, BMIBLGR1 = bmigr, HEIGHTBL = height, WEIGHTBL = weight,
    EDUCLVL = one(sc, sc$SCTESTCD == "EDLEVEL", "SCSTRESN"),
    DISONSDT = dison, DURDIS = durdis, DURDSGR1 = ifelse(durdis < 12, "<12", ">=12"),
    VISIT1DT = v1, RFSTDTC = out$RFSTDTC, RFENDTC = out$RFENDTC,
    VISNUMEN = ifelse(dsvisit == 13, 12, dsvisit), RFENDT = rfendt,
    DCDECOD = dcdecod, DCREASCD = dcreascd, MMSETOT = mmsetot,
    stringsAsFactors = FALSE
  )
  spec_order <- spec$variables[spec$variables$dataset == "ADSL", ]
  qc[spec_order$variable[order(spec_order$order)]]
}
