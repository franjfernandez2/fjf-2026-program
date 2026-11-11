# ADTTE for the CDISC pilot (CDISCPILOT01): time to the first dermatologic event, derived
# from our ADSL and ADAE with admiral.
#
# Specification: the pilot's ADaM define.xml (ADR 0016); analysis in SAP section 11.2
# (Figure 14-1). The official ADTTE is used only for comparison.

#' Derive ADTTE from ADSL and ADAE.
#'
#' The event is the first treatment-emergent dermatologic adverse event (ADAE.AOCC01FL).
#' A subject without one is censored at the end of study, RFENDT.
#' @param adsl Our ADSL, from `derive_adsl()`.
#' @param adae Our ADAE, from `derive_adae()`.
#' @param spec Output of `read_define_v1()`.
#' @return ADTTE with the specification's variables, order, labels and types.
derive_adtte <- function(adsl, adae, spec) {
  # The define.xml spells the event description "Dematologic Event Occured"; the official
  # value is kept, as it is the specified value.
  event <- admiral::event_source(
    dataset_name = "adae",
    filter = AOCC01FL == "Y",
    date = ASTDT,
    set_values_to = admiral::exprs(
      EVNTDESC = "Dematologic Event Occured", SRCDOM = "ADAE", SRCVAR = "ASTDT", SRCSEQ = AESEQ
    )
  )
  censor <- admiral::censor_source(
    dataset_name = "adsl",
    date = RFENDT,
    set_values_to = admiral::exprs(
      EVNTDESC = "Study Completion Date", SRCDOM = "ADSL", SRCVAR = "RFENDT", SRCSEQ = NA_real_
    )
  )
  adsl_start <- adsl |> dplyr::mutate(STARTDT = as.Date(RFSTDTC))

  adtte <- admiral::derive_param_tte(
    dataset_adsl = adsl_start,
    start_date = STARTDT,
    event_conditions = list(event),
    censor_conditions = list(censor),
    source_datasets = list(adsl = adsl_start, adae = adae),
    set_values_to = admiral::exprs(
      # As the derivation says; check_against_spec() checks both against their codelists.
      PARAMCD = "TTDE",
      PARAM = "Time to First Dermatologic Event"
    )
  )
  stopifnot(nrow(adtte) == nrow(adsl))

  adtte |>
    admiral::derive_vars_duration(new_var = AVAL, start_date = STARTDT, end_date = ADT) |>
    admiral::derive_vars_merged(
      dataset_add = adsl, by_vars = admiral::exprs(USUBJID),
      new_vars = admiral::exprs(
        SITEID, AGE, AGEGR1, AGEGR1N, RACE, RACEN, SEX, TRTSDT, TRTEDT, TRTDUR,
        TRTP = TRT01P, TRTA = TRT01A, TRTAN = TRT01AN, SAFFL
      )
    ) |>
    dplyr::arrange(USUBJID) |>
    apply_spec_attributes(spec, "ADTTE")
}
