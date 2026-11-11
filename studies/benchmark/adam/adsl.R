# ADSL for the CDISC pilot (CDISCPILOT01), derived from SDTM with admiral.
#
# Specification: the pilot's ADaM define.xml (ADR 0016), read into `spec`. Statistical
# analysis plan: inside the pilot's study report, pp. 418-468. Where the specification
# is silent or ambiguous, the reading taken here is stated next to the derivation and,
# if it differs from the official ADSL, recorded in studies/benchmark/discrepancy-log.csv.
#
# Nothing here is copied from the official ADSL: it is used only as the reference that
# compare_with_reference() checks this dataset against.

#' Derive ADSL from SDTM.
#'
#' @param sdtm Named list of SDTM domains from `read_sdtm()`: dm, ds, ex, mh, qs, sc, sv, vs.
#' @param spec Output of `read_define_v1()` for the pilot's ADaM define.xml.
#' @return ADSL with the specification's variables, order, labels and types.
derive_adsl <- function(sdtm, spec) {
  # Floats are stored with the decimal places the define.xml gives them, rounded as SAS
  # does. Derivations that name another ADSL variable use its stored, rounded value, as
  # the official ADSL does (discrepancy D-002).
  stored <- function(x, variable) round_to_spec(x, spec, "ADSL", variable)
  dm <- sdtm$dm
  sv <- sdtm$sv

  # Population: randomised subjects. The 52 screen failures in DM have no treatment and
  # no ADSL record (the define.xml describes ADSL as one record per randomised subject).
  adsl <- dm |>
    dplyr::filter(ARMCD != "Scrnfail") |>
    dplyr::select(
      STUDYID, USUBJID, SUBJID, SITEID, ARM, ARMCD, AGE, AGEU, RACE, SEX, ETHNIC, DTHFL,
      RFSTDTC, RFENDTC
    )

  # Visit dates. A subject can have two SV records for one visit number; the first counts.
  visit_date <- function(data, visit, new_var) {
    admiral::derive_vars_merged(
      data,
      dataset_add = sv,
      by_vars = admiral::exprs(USUBJID),
      filter_add = VISITNUM == !!visit,
      new_vars = admiral::exprs(!!new_var := as.Date(SVSTDTC)),
      order = admiral::exprs(SVSTDTC),
      mode = "first"
    )
  }
  adsl <- adsl |>
    visit_date(1, rlang::sym("VISIT1DT")) |>
    visit_date(3, rlang::sym("TRTSDT")) |>
    visit_date(4, rlang::sym("VISIT4DT")) |>
    visit_date(8, rlang::sym("VISIT8DT")) |>
    visit_date(10, rlang::sym("VISIT10DT")) |>
    visit_date(12, rlang::sym("VISIT12DT"))

  # Treatment. TRT01PN takes the code of the ARMN codelist (0, 54, 81 mg).
  adsl <- adsl |>
    dplyr::mutate(
      TRT01P = ARM,
      TRT01PN = as.numeric(codelist_lookup(spec, "ARMN", TRT01P, from = "decode")),
      TRT01A = TRT01P,
      TRT01AN = TRT01PN
    )

  # Disposition: the subject's one DISPOSITION EVENT record in DS.
  adsl <- admiral::derive_vars_merged(
    adsl,
    dataset_add = sdtm$ds,
    by_vars = admiral::exprs(USUBJID),
    filter_add = DSCAT == "DISPOSITION EVENT",
    new_vars = admiral::exprs(
      DCDECOD = DSDECOD, DSTERM_ = DSTERM, DSVISIT_ = VISITNUM, DCDT_ = as.Date(DSSTDTC)
    ),
    check_type = "error"
  )
  adsl <- adsl |>
    dplyr::mutate(
      # The define.xml gives no mapping for this grouping. Each DSDECOD value maps to the
      # DISCREAS term of the same meaning; "I/E Not Met" is the PROTOCOL VIOLATION records
      # whose term is "PROTOCOL ENTRY CRITERIA NOT MET".
      DCREASCD = dplyr::case_when(
        DCDECOD == "COMPLETED" ~ "Completed",
        DCDECOD == "ADVERSE EVENT" ~ "Adverse Event",
        DCDECOD == "DEATH" ~ "Death",
        DCDECOD == "PROTOCOL VIOLATION" & DSTERM_ == "PROTOCOL ENTRY CRITERIA NOT MET" ~ "I/E Not Met",
        DCDECOD == "LACK OF EFFICACY" ~ "Lack of Efficacy",
        DCDECOD == "LOST TO FOLLOW-UP" ~ "Lost to Follow-up",
        DCDECOD == "PHYSICIAN DECISION" ~ "Physician Decision",
        DCDECOD == "PROTOCOL VIOLATION" ~ "Protocol Violation",
        DCDECOD == "STUDY TERMINATED BY SPONSOR" ~ "Sponsor Decision",
        DCDECOD == "WITHDRAWAL BY SUBJECT" ~ "Withdrew Consent"
      ),
      DISCONFL = dplyr::if_else(DCREASCD != "Completed", "Y", NA_character_),
      DSRAEFL = dplyr::if_else(DCREASCD == "Adverse Event", "Y", NA_character_),
      # Visit 13 is the post-treatment follow-up: a completer's last visit on treatment is 12.
      VISNUMEN = dplyr::if_else(DSVISIT_ == 13, 12, DSVISIT_)
    )
  stopifnot(!anyNA(adsl$DCREASCD))
  codelist_lookup(spec, "DISCREAS", adsl$DCREASCD, from = "code")

  # Treatment end: EXENDTC on the subject's last EX record. If it is missing and the subject
  # discontinued after visit 3, the date of discontinuation.
  adsl <- admiral::derive_vars_merged(
    adsl,
    dataset_add = sdtm$ex,
    by_vars = admiral::exprs(USUBJID),
    new_vars = admiral::exprs(TRTEDT = as.Date(EXENDTC)),
    order = admiral::exprs(EXSTDTC),
    mode = "last"
  ) |>
    dplyr::mutate(
      TRTEDT = dplyr::if_else(is.na(TRTEDT) & DSVISIT_ > 3, DCDT_, TRTEDT),
      TRTDUR = as.numeric(TRTEDT - TRTSDT) + 1
    )

  # Cumulative dose. Placebo and low dose take one dose throughout. High dose is titrated:
  # 54 mg up to visit 4, 81 mg from visit 4 to visit 12, 54 mg after visit 12. The define.xml
  # calls the arms ARMN = 0, 1, 2; its ARMN codelist codes them 0, 54, 81.
  adsl <- adsl |>
    dplyr::mutate(
      reached4 = !is.na(VISIT4DT) & TRTEDT >= VISIT4DT,
      reached12 = !is.na(VISIT12DT) & TRTEDT >= VISIT12DT,
      days1 = dplyr::if_else(reached4, as.numeric(VISIT4DT - TRTSDT) + 1, TRTDUR),
      days2 = dplyr::case_when(
        reached12 ~ as.numeric(VISIT12DT - VISIT4DT),
        reached4 ~ as.numeric(TRTEDT - VISIT4DT),
        .default = 0
      ),
      days3 = dplyr::if_else(reached12, as.numeric(TRTEDT - VISIT12DT), 0),
      CUMDOSE = dplyr::if_else(
        TRT01PN == 81, 54 * days1 + 81 * days2 + 54 * days3, TRT01PN * TRTDUR
      ),
      CUMDOSE = stored(CUMDOSE, "CUMDOSE"),
      AVGDD = stored(CUMDOSE / TRTDUR, "AVGDD")
    )

  # Demographics.
  adsl <- adsl |>
    dplyr::mutate(
      AGEGR1N = dplyr::case_when(AGE < 65 ~ 1, AGE <= 80 ~ 2, AGE > 80 ~ 3),
      AGEGR1 = codelist_lookup(spec, "AGEGR1N", as.character(AGEGR1N), from = "code"),
      RACEN = as.numeric(codelist_lookup(spec, "RACEN", RACE, from = "decode"))
    )

  # Baseline measurements.
  adsl <- adsl |>
    admiral::derive_vars_merged(
      dataset_add = sdtm$vs, by_vars = admiral::exprs(USUBJID),
      filter_add = VSTESTCD == "HEIGHT" & VISITNUM == 1,
      new_vars = admiral::exprs(HEIGHTBL = VSSTRESN), check_type = "error"
    ) |>
    admiral::derive_vars_merged(
      dataset_add = sdtm$vs, by_vars = admiral::exprs(USUBJID),
      filter_add = VSTESTCD == "WEIGHT" & VISITNUM == 3,
      new_vars = admiral::exprs(WEIGHTBL = VSSTRESN), check_type = "error"
    ) |>
    # The define.xml writes WEIGHTBL / ((HEIGHTBL*100)**2). Height is in cm, so the
    # formula for kg/m2 is WEIGHTBL / (HEIGHTBL/100)**2 (discrepancy D-006).
    # BMIBLGR1 holds the codes of codelist BMICAT, except that the code for BMIBL >= 30 is
    # written ">=30", as the derivation says, not ">30", as the codelist says (D-003).
    # A missing BMIBL gives a missing group (D-004).
    dplyr::mutate(
      HEIGHTBL = stored(HEIGHTBL, "HEIGHTBL"),
      WEIGHTBL = stored(WEIGHTBL, "WEIGHTBL"),
      BMIBL = stored(WEIGHTBL / (HEIGHTBL / 100)^2, "BMIBL"),
      BMIBLGR1 = dplyr::case_when(
        BMIBL < 25 ~ "<25", BMIBL < 30 ~ "25-<30", BMIBL >= 30 ~ ">=30"
      )
    ) |>
    # The define.xml names SCTESTCD = YEARSEDU; the pilot's SC has only EDLEVEL (D-005).
    admiral::derive_vars_merged(
      dataset_add = sdtm$sc, by_vars = admiral::exprs(USUBJID),
      filter_add = SCTESTCD == "EDLEVEL",
      new_vars = admiral::exprs(EDUCLVL = SCSTRESN), check_type = "error"
    ) |>
    admiral::derive_vars_merged_summary(
      dataset_add = sdtm$qs, by_vars = admiral::exprs(USUBJID),
      filter_add = QSCAT == "MINI-MENTAL STATE",
      new_vars = admiral::exprs(MMSETOT = sum(as.numeric(QSORRES)))
    )

  # Disease duration: months from the primary diagnosis to visit 1.
  adsl <- adsl |>
    admiral::derive_vars_merged(
      dataset_add = sdtm$mh, by_vars = admiral::exprs(USUBJID),
      filter_add = MHCAT == "PRIMARY DIAGNOSIS",
      new_vars = admiral::exprs(DISONSDT = as.Date(MHSTDTC)), check_type = "error"
    ) |>
    admiral::derive_vars_duration(
      new_var = DURDIS, start_date = DISONSDT, end_date = VISIT1DT, out_unit = "months"
    ) |>
    dplyr::mutate(
      DURDIS = stored(DURDIS, "DURDIS"),
      DURDSGR1 = dplyr::if_else(DURDIS < 12, "<12", ">=12")
    )

  # Population flags. EFFFL needs ADAS-Cog and CIBIC+ assessments after baseline (visit 3).
  post_baseline <- function(category) {
    unique(sdtm$qs$USUBJID[sdtm$qs$QSCAT == category & sdtm$qs$VISITNUM > 3])
  }
  adas <- post_baseline("ALZHEIMER'S DISEASE ASSESSMENT SCALE")
  cibic <- post_baseline("CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)")
  yn <- function(x) dplyr::if_else(x, "Y", "N")
  adsl <- adsl |>
    dplyr::mutate(
      RFENDT = as.Date(RFENDTC),
      ITTFL = yn(!is.na(ARMCD)),
      SAFFL = yn(ITTFL == "Y" & !is.na(TRTSDT)),
      EFFFL = yn(SAFFL == "Y" & USUBJID %in% adas & USUBJID %in% cibic),
      # "ENDDT" in the define.xml is not an ADSL variable; the end of study, RFENDT, is used.
      COMP8FL = yn(!is.na(VISIT8DT) & RFENDT >= VISIT8DT),
      COMP16FL = yn(!is.na(VISIT10DT) & RFENDT >= VISIT10DT),
      COMP24FL = yn(!is.na(VISIT12DT) & RFENDT >= VISIT12DT)
    )

  adsl <- adsl |>
    dplyr::mutate(SITEGR1 = pool_sites(SITEID, TRT01P)) |>
    dplyr::arrange(USUBJID)

  apply_spec_attributes(adsl, spec, "ADSL")
}

#' Pool small sites (SAP section 7.1).
#'
#' Sites that enrol fewer than 3 patients in any treatment group are grouped into site
#' "900". The SAP's next step, adding the next smallest site while the pooled group is
#' still short, is not needed in the pilot: the function stops if it would be.
#' @param site Site identifiers, one per subject.
#' @param arm Treatment group, one per subject.
#' @return Pooled site identifiers.
pool_sites <- function(site, arm) {
  counts <- table(site, arm)
  small <- rownames(counts)[apply(counts, 1, min) < 3]
  pooled <- ifelse(site %in% small, "900", site)
  if (length(small) > 0 && min(table(factor(arm[pooled == "900"], levels = colnames(counts)))) < 3) {
    stop("Pooled site 900 still has fewer than 3 patients in a treatment group.", call. = FALSE)
  }
  pooled
}
