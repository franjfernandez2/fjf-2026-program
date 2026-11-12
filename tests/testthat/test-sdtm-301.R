root <- test_path("..", "..")
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
spec <- yaml::read_yaml(file.path(root, "docs", "specs", "sdtm-301.yml"))

f <- fixture_raw_301()
raw <- f$raw
assignment <- f$assignment
dm <- map_dm_301(raw, assignment, protocol, spec)
se <- map_se_301(dm, protocol, spec)
ds <- map_ds_301(raw, assignment, dm, spec, se)
sv <- map_sv_301(raw, dm, protocol, spec)
dictionary <- yaml::read_yaml(file.path(root, "docs", "specs", "coding-dictionary.yml"))
ex <- map_ex_301(raw, assignment, dm, protocol, spec, se)
da <- map_da_301(raw, dm, protocol, spec, se)
cm <- map_cm_301(raw, dm, dictionary, spec, se)
ae <- map_ae_301(raw, dm, dictionary, spec, se)
raw_spec <- read_raw_spec(file.path(root, "docs", "specs", "raw-layer.yml"))
lb <- map_lb_301(raw, raw_spec, dm, protocol, spec, se)
vs <- map_vs_301(raw, raw_spec, dm, protocol, spec, se)
ce <- map_ce_301(raw, dm, spec, se)
ie <- map_ie_301(raw, dm, protocol, spec)
ts <- map_ts_301(protocol, spec, dm, 300L, c("Secondary one", "Secondary two"))

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
  expect_identical(names(lb$lb), spec_names("LB"))
  expect_identical(names(vs$vs), spec_names("VS"))
  expect_identical(names(ce$ce), spec_names("CE"))
  expect_identical(names(ce$suppce), spec_names("SUPPCE"))
  expect_identical(names(se), spec_names("SE"))
  expect_identical(names(ie), spec_names("IE"))
  expect_identical(names(ts), spec_names("TS"))
  for (d in c("TA", "TE", "TV", "TI")) {
    expect_identical(names(get(paste0("map_", tolower(d), "_301"))(protocol, spec)), spec_names(d))
  }
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
  expect_identical(ds$DSTERM[ds$DSDECOD == "SCREEN FAILURE"], raw$enrolment$failed_criterion[2])
  expect_identical(ds$DSSCAT[ds$DSDECOD == "SCREEN FAILURE"], "STUDY PARTICIPATION")
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
  expect_identical(map_ex_301(raw, placebo, dm, protocol, spec, se)$EXTRT, c("PLACEBO", "PLACEBO"))
})

test_that("a last bottle returned before the last dose runs until it, and is listed (CR10)", {
  expect_identical(nrow(ex_issues_301(raw, dm, spec)), 0L)
  early <- raw
  last <- nrow(early$exposure)
  early$exposure$return_date[last] <- "10-JUL-2028"
  expect_identical(map_ex_301(early, assignment, dm, protocol, spec, se)$EXENDTC, c("2028-02-15", "2028-07-18"))
  issues <- ex_issues_301(early, dm, spec)
  expect_identical(issues$rule, "CR10")
  expect_match(issues$detail, "last dose 2028-07-18 .* 9 day\\(s\\) after")
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
  expect_error(map_ae_301(bad, dm, dictionary, spec, se), "Uncoded adverse event terms: Something new")
})

test_that("duplicates keep the first entry and are listed (CR01)", {
  d <- data.frame(record_id = c(3, 1, 2), subject_id = "S1", visit = c("A", "A", "B"), value = c(5, 4, 1))
  first <- keep_first_entry(d, c("subject_id", "visit"), "t")
  expect_identical(first$data$record_id, c(1, 2))
  expect_identical(first$issues$record_id, 3)
  expect_match(first$issues$detail, "duplicate of record 1, with different values")
})

test_that("LB converts units, keeps qualified results as text and lists unknown units", {
  l <- lb$lb
  expect_identical(nrow(l), 5L)
  expect_identical(lb$issues$rule, c("CR01", "CR06"))
  h <- l[l$LBTESTCD == "HBA1C", ]
  expect_equal(h$LBSTRESN, c(8.1, 0.0915 * 64 + 2.15, 7.2))
  expect_identical(h$LBSTRESU, rep("%", 3))
  expect_identical(h$LBLOBXFL, c(NA, "Y", NA))
  expect_identical(h$LBFAST, c(NA, NA, "Y"))
  b <- l[l$LBTESTCD == "BILI", ]
  expect_identical(c(b$LBORRES, b$LBSTRESC), c("<8", "<8"))
  expect_true(is.na(b$LBSTRESN))
  expect_true(is.na(l$LBSTRESN[l$LBTESTCD == "CREAT"]))
})

test_that("VS converts pounds and metres, and drops measurements not taken", {
  v <- vs$vs
  expect_identical(vs$issues$rule, "CR01")
  w <- v[v$VSTESTCD == "WEIGHT", ]
  expect_identical(c(w$VSORRES, w$VSORRESU, w$VSSTRESU), c("180.0", "LB", "kg"))
  expect_identical(w$VSSTRESN, round(180 / 2.20462, 1))
  ht <- v[v$VSTESTCD == "HEIGHT", ]
  expect_identical(ht$VSSTRESN, c(170, 162))
  expect_identical(ht$VSORRESU, c("cm", "m"))
  expect_identical(v$VSSTRESN[v$VSTESTCD == "SYSBP"], c(140, 135))
})

test_that("CE orders hypoglycaemic events by date and keeps glucose and assistance in SUPPCE", {
  expect_identical(ce$ce$CESTDTC, c("2028-02-01", "2028-03-03"))
  expect_identical(ce$ce$CESCAT, c("LEVEL 1", "LEVEL 2"))
  expect_identical(ce$suppce$QVAL, c("N", "62", "Y", "50"))
})

test_that("end of participation is the last date of contact", {
  expect_identical(dm$RFPENDTC, c("2028-08-01", "2028-01-06", "2028-01-20"))
})

test_that("SE follows each subject through screening, treatment and follow-up", {
  s1 <- se[se$USUBJID == dm$USUBJID[1], ]
  expect_identical(s1$ETCD, c("SCRN", "FJF20", "FU"))
  expect_identical(s1$SESTDTC, c("2028-01-02", "2028-01-19", "2028-07-18"))
  expect_identical(s1$SEENDTC, c("2028-01-19", "2028-07-18", "2028-08-01"))
  # Each element ends when the next starts; the shared days belong to treatment.
  expect_identical(s1$SEENDTC[-3], s1$SESTDTC[-1])
  expect_identical(
    epoch_of(rep(s1$USUBJID[1], 4), c("2028-01-18", "2028-01-19", "2028-07-18", "2028-07-19"), se),
    c("SCREENING", "TREATMENT", "TREATMENT", "FOLLOW-UP")
  )
  expect_identical(se$ETCD[se$USUBJID != dm$USUBJID[1]], c("SCRN", "SCRN"))
})

test_that("EPOCH comes from SE, and a partial date has none", {
  expect_identical(ae$ae$EPOCH, c("TREATMENT", NA, "TREATMENT"))
  expect_identical(ds$EPOCH[ds$DSDECOD == "INFORMED CONSENT OBTAINED"], rep("SCREENING", 3))
})

test_that("IE records the criterion a screen failure did not meet", {
  expect_identical(ie$IETESTCD, "EX08")
  expect_identical(c(ie$IECAT, ie$IEORRES), c("EXCLUSION", "Y"))
  bad <- raw
  bad$enrolment$failed_criterion[2] <- "Not a criterion"
  expect_error(map_ie_301(bad, dm, protocol, spec), "Failed criteria not in the protocol")
})

test_that("TS keeps values within 200 characters and codes CDISC terms", {
  expect_true(all(nchar(ts$TSVAL[!is.na(ts$TSVAL)]) <= 200))
  title <- ts[ts$TSPARMCD == "TITLE", ]
  expect_identical(paste(title$TSVAL, title$TSVAL1), gsub("\\s+", " ", protocol$title))
  expect_identical(ts$TSVALCD[ts$TSPARMCD == "TPHASE"], "C15602")
  expect_identical(ts$TSVAL[ts$TSPARMCD == "OBJSEC"], c("Secondary one", "Secondary two"))
  expect_identical(ts$TSSEQ[ts$TSPARMCD == "OBJSEC"], 1:2)
  expect_true(all(is.na(ts$TSVAL) == !is.na(ts$TSVALNF)))
})

test_that("the XPT files keep the specification's labels, and define.xml describes every variable", {
  dir <- withr::local_tempdir()
  files <- write_sdtm_domain(dm, spec, "DM", dir)
  x <- haven::read_xpt(files[1])
  expect_identical(attr(x, "label"), "Demographics")
  expect_identical(attr(x$USUBJID, "label"), "Unique Subject Identifier")
  expect_identical(nrow(arrow::read_parquet(files[2])), nrow(dm))
  datasets <- list(
    DM = dm, DS = ds, SV = sv, EX = ex, DA = da, CM = cm, AE = ae$ae, SUPPAE = ae$suppae,
    LB = lb$lb, VS = vs$vs, CE = ce$ce, SUPPCE = ce$suppce, IE = ie, SE = se, TA = map_ta_301(protocol, spec),
    TE = map_te_301(protocol, spec), TV = map_tv_301(protocol, spec), TI = map_ti_301(protocol, spec), TS = ts
  )
  path <- write_sdtm_define(datasets, spec, file.path(dir, "define.xml"), "2026-11-12T00:00:00")
  doc <- xml2::xml_ns_strip(xml2::read_xml(path))
  expect_length(xml2::xml_find_all(doc, "//ItemGroupDef"), length(spec$domains))
  refs <- xml2::xml_attr(xml2::xml_find_all(doc, "//ItemRef"), "ItemOID")
  defs <- xml2::xml_attr(xml2::xml_find_all(doc, "//ItemDef"), "OID")
  expect_setequal(refs, defs)
  aestdtc <- xml2::xml_find_first(doc, "//ItemDef[@OID='IT.AE.AESTDTC']")
  expect_identical(xml2::xml_attr(aestdtc, "DataType"), "partialDate")
})
