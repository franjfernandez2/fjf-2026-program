root <- test_path("..", "..")
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
spec <- yaml::read_yaml(file.path(root, "docs", "specs", "sdtm-301.yml"))

# Three subjects: one completer, one screen failure, one randomised but never dosed.
raw <- list(
  enrolment = data.frame(
    subject_id = c("301-001-0001", "301-001-0002", "301-001-0003"), site_id = "301-001",
    consent_date = c("02-JAN-2028", "03-JAN-2028", "04-JAN-2028"),
    screening_date = c("05-JAN-2028", "06-JAN-2028", "07-JAN-2028"),
    eligible = c("Y", "N", "Y"), failed_criterion = c(NA, "EX03", NA)
  ),
  demography = data.frame(
    subject_id = c("301-001-0001", "301-001-0002", "301-001-0003"),
    birth_date = c("10-JUN-1960", "UNK-MAR-1970", "1955"), sex = c("F", "M", "F"),
    race = "WHITE", ethnicity = "NOT HISPANIC OR LATINO"
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
dm <- map_dm_301(raw, assignment, protocol, spec)
ds <- map_ds_301(raw, assignment, dm, spec)
sv <- map_sv_301(raw, dm, protocol, spec)
dictionary <- yaml::read_yaml(file.path(root, "docs", "specs", "coding-dictionary.yml"))
ex <- map_ex_301(raw, assignment, dm, protocol, spec)
da <- map_da_301(raw, dm, protocol, spec)
cm <- map_cm_301(raw, dm, dictionary, spec)
ae <- map_ae_301(raw, dm, dictionary, spec)

spec_names <- function(domain) purrr::map_chr(sdtm_variables(spec, domain), "name")

test_that("each domain has exactly the variables of the specification, in order", {
  expect_identical(names(dm), spec_names("DM"))
  expect_identical(names(ds), spec_names("DS"))
  expect_identical(names(sv), spec_names("SV"))
  expect_identical(names(ex), spec_names("EX"))
  expect_identical(names(da), spec_names("DA"))
  expect_identical(names(cm), spec_names("CM"))
  expect_identical(names(ae$ae), spec_names("AE"))
  expect_identical(names(ae$suppae), spec_names("SUPPAE"))
})

test_that("DM has one record per screened subject", {
  expect_identical(nrow(dm), 3L)
  expect_false(anyDuplicated(dm$USUBJID) > 0)
  expect_identical(dm$USUBJID[1], "FJF-2026-301-301-001-0001")
  expect_identical(dm$ARMNRS, c(NA, "SCREEN FAILURE", "ASSIGNED, NOT TREATED"))
  expect_identical(dm$ACTARMCD, c("FJF20", NA, NA))
  expect_identical(dm$RFSTDTC[1], "2028-01-19")
  expect_identical(dm$BRTHDTC, c("1960-06-10", "1970-03", "1955"))
  expect_identical(dm$AGE, c(67L, 57L, 72L))
})

test_that("DS numbers each subject's records and dates them against the first dose", {
  expect_false(anyDuplicated(paste(ds$USUBJID, ds$DSSEQ)) > 0)
  s1 <- ds[ds$USUBJID == dm$USUBJID[1], ]
  expect_identical(s1$DSSEQ, seq_len(nrow(s1)))
  expect_identical(s1$DSSTDY[s1$DSDECOD == "RANDOMIZED"], 1L)
  expect_identical(s1$DSSTDY[s1$DSDECOD == "INFORMED CONSENT OBTAINED"], -17L)
  expect_identical(ds$DSTERM[ds$DSDECOD == "SCREEN FAILURE"], "EX03")
})

test_that("SV numbers unscheduled visits after the previous planned visit", {
  expect_false(anyDuplicated(paste(sv$USUBJID, sv$VISITNUM)) > 0)
  expect_identical(sv$VISITNUM, c(1, 2, 2.1, 2.2, 3))
  expect_identical(sv$VISIT[3], "UNSCHEDULED 2.1")
  expect_identical(sv$SVOCCUR, c("Y", "Y", NA, NA, "N"))
  expect_true(is.na(sv$SVSTDTC[5]))
})

test_that("EX takes the treatment of the assignment and ends the day before the return", {
  expect_identical(ex$EXTRT, c("FJF-2026", "FJF-2026"))
  expect_identical(ex$EXDOSE, c(20, 20))
  expect_identical(ex$EXENDTC, c("2028-02-15", "2028-07-18"))
  expect_identical(ex$EXSTDY, c(1L, 29L))
  placebo <- assignment
  placebo$randomisation$arm_code <- "PBO"
  expect_identical(map_ex_301(raw, placebo, dm, protocol, spec)$EXTRT, c("PLACEBO", "PLACEBO"))
})

test_that("DA keeps each tablet count, and a count not done stays missing", {
  expect_identical(da$DATESTCD, c("DISPAMT", "RETAMT", "DISPAMT", "RETAMT"))
  expect_identical(da$DASTRESN, c(35, 7, 35, NA))
  expect_identical(da$DASTAT, c(NA, NA, NA, "NOT DONE"))
})

test_that("CM codes medications and marks rescue and ongoing ones", {
  expect_identical(cm$CMCLASCD, c("A10BA", "A10BB"))
  expect_identical(cm$CMCAT, c("BACKGROUND ANTIDIABETIC THERAPY", "RESCUE MEDICATION"))
  expect_identical(cm$CMSTDTC, c("2018-05", "2028-05-02"))
  expect_identical(cm$CMDOSTOT, c(2000, NA))
  expect_identical(cm$CMENRTPT, c("ONGOING", "ONGOING"))
})

test_that("AE codes terms, keeps partial dates and moves an end before the start to SUPPAE", {
  a <- ae$ae
  expect_identical(a$AEDECOD, c("Headache", "Hypoglycaemia", "Nasopharyngitis"))
  expect_identical(a$AESTDTC, c("2028-01-25", "2028-02", "2028-03-10"))
  expect_identical(a$AESTDY, c(7L, NA, 52L))
  expect_identical(a$AESEV, c("MILD", NA, "MODERATE"))
  expect_true(is.na(a$AEENDTC[3]))
  expect_identical(ae$suppae$IDVARVAL, "3")
  expect_identical(ae$suppae$QVAL, "2028-03-01")
})

test_that("an uncoded term stops the build", {
  bad <- raw
  bad$adverse_events$verbatim_term[1] <- "Something new"
  expect_error(map_ae_301(bad, dm, dictionary, spec), "Uncoded adverse event terms: Something new")
})
