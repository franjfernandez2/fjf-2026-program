# Synthetic raw data of FJF-2026-301 shared by the SDTM and ADaM tests: three subjects (one
# completer, one screen failure, one randomised but never dosed).
fixture_raw_301 <- function() {
  # Three subjects: one completer, one screen failure, one randomised but never dosed.
  raw <- list(
    enrolment = data.frame(
      subject_id = c("301-001-0001", "301-001-0002", "301-001-0003"), site_id = "301-001",
      consent_date = c("02-JAN-2028", "03-JAN-2028", "04-JAN-2028"),
      screening_date = c("05-JAN-2028", "06-JAN-2028", "07-JAN-2028"),
      eligible = c("Y", "N", "Y"),
      failed_criterion = c(NA, "Level 3 (severe) hypoglycaemia in the 6 months before screening", NA)
    ),
    demography = data.frame(
      subject_id = c("301-001-0001", "301-001-0002", "301-001-0003"),
      birth_date = c("10-JUN-1960", "UNK-MAR-1970", "1955"), sex = c("F", "M", "F"),
      race = "WHITE", ethnicity = "NOT HISPANIC OR LATINO", height_cm = c("170", "1.62", "")
    ),
    lab_results = data.frame(
      record_id = 1:6, subject_id = "301-001-0001", visit_code = c("V1", "V1", "V2", "V2", "V6", "V6"),
      collection_date = c("05-JAN-2028", "05-JAN-2028", "19-JAN-2028", "19-JAN-2028", "12-APR-2028", "12-APR-2028"),
      test_code = c("HBA1C", "HBA1C", "HBA1C", "BILI", "HBA1C", "CREAT"),
      result = c("8.1", "8.1", "64", "<8", "7.2", "1.0"), unit = c("%", "%", "mmol/mol", "umol/L", "%", "g/L"),
      ref_low = c("4", "4", "20", "2", "4", "0.5"), ref_high = c("5.6", "5.6", "38", "21", "5.6", "1.1"),
      fasting = c("", "", "", "", "Y", "")
    ),
    vital_signs = data.frame(
      record_id = 1:3, subject_id = "301-001-0001", visit_code = c("V1", "V2", "V2"),
      measure_date = c("05-JAN-2028", "19-JAN-2028", "19-JAN-2028"), sysbp = c(140, 135, 120), diabp = 80,
      pulse = 70, weight = c("180.0", NA, NA), weight_unit = c("lb", NA, NA)
    ),
    hypoglycaemia = data.frame(
      subject_id = "301-001-0001", event_seq = 1:2, event_date = c("03-MAR-2028", "01-FEB-2028"),
      glucose_mg_dl = c(50, 62), level = c(2, 1), assistance = c("Y", "N")
    ),
    exposure = data.frame(
      record_id = 1:2, subject_id = "301-001-0001", visit_code = c("V2", "V4"),
      dispense_date = c("19-JAN-2028", "16-FEB-2028"), return_date = c("16-FEB-2028", NA),
      tablets_dispensed = c(35L, 35L), tablets_returned = c(7L, NA)
    ),
    adverse_events = data.frame(
      subject_id = "301-001-0001", ae_seq = 1:3, verbatim_term = c(" HEADACHE", "Low blood sugar", "COLD"),
      start_date = c("25-JAN-2028", "UNK-FEB-2028", "10-MAR-2028"), end_date = c("27-JAN-2028", NA, "01-MAR-2028"),
      severity = c("MILD", "", "MODERATE"), serious = "N", related = c("N", "Y", "N"),
      action_taken = "DOSE NOT CHANGED", outcome = c("RECOVERED", "NOT RECOVERED", "RECOVERED")
    ),
    concomitant_meds = data.frame(
      subject_id = "301-001-0001", cm_seq = 1:2, drug_name = c("METFORMIN", "GLICLAZIDE"),
      indication = c("TYPE 2 DIABETES", "HYPERGLYCAEMIA (RESCUE)"), daily_dose = c("2000 MG", ""),
      start_date = c("UNK-MAY-2018", "02-MAY-2028"), end_date = NA, rescue = c("N", "Y")
    ),
    disposition = data.frame(
      subject_id = c("301-001-0001", "301-001-0001", "301-001-0003"),
      event = c("TREATMENT", "STUDY", "STUDY"), status = c("COMPLETED", "COMPLETED", "DISCONTINUED"),
      reason = c(NA, NA, "WITHDRAWAL BY SUBJECT"), event_date = c("18-JUL-2028", "01-AUG-2028", "20-JAN-2028")
    ),
    visits = data.frame(
      subject_id = "301-001-0001", visit_code = c("V1", "V2", "UNS", "V3", "UNS"),
      visit_date = c("05-JAN-2028", "19-JAN-2028", "25-JAN-2028", NA, "30-JAN-2028"),
      visit_status = c("DONE", "DONE", "DONE", "MISSED", "DONE")
    )
  )
  assignment <- list(
    randomisation = data.frame(
      subject_id = c("301-001-0001", "301-001-0003"), randomisation_date = c("19-JAN-2028", "19-JAN-2028"),
      stratum = "A", arm_code = c("FJF20", "PBO")
    ),
    blinded = TRUE
  )
  raw$randomisation <- cbind(assignment$randomisation, randomisation_number = c("1001", "1002"))
  list(raw = raw, assignment = assignment)
}

# The SDTM of the fixture, built with the production programs.
fixture_sdtm_301 <- function(root) {
  f <- fixture_raw_301()
  protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
  spec <- yaml::read_yaml(file.path(root, "docs", "specs", "sdtm-301.yml"))
  dictionary <- yaml::read_yaml(file.path(root, "docs", "specs", "coding-dictionary.yml"))
  raw_spec <- read_raw_spec(file.path(root, "docs", "specs", "raw-layer.yml"))
  raw <- f$raw
  dm <- map_dm_301(raw, f$assignment, protocol, spec)
  se <- map_se_301(dm, protocol, spec)
  ae <- map_ae_301(raw, dm, dictionary, spec, se)
  ce <- map_ce_301(raw, dm, spec, se)
  list(
    DM = dm, SUPPDM = map_suppdm_301(raw, spec), DS = map_ds_301(raw, f$assignment, dm, spec, se), SE = se,
    EX = map_ex_301(raw, f$assignment, dm, protocol, spec, se), DA = map_da_301(raw, dm, protocol, spec, se),
    CM = map_cm_301(raw, dm, dictionary, spec, se), AE = ae$ae,
    LB = map_lb_301(raw, raw_spec, dm, protocol, spec, se)$lb,
    VS = map_vs_301(raw, raw_spec, dm, protocol, spec, se)$vs,
    CE = ce$ce, SUPPCE = ce$suppce
  )
}
