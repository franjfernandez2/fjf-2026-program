# Helpers shared by the ADaM programs (R/adam_helpers.R).

fixture <- testthat::test_path("fixtures", "define-v1-mini.xml")

test_that("round_sas rounds halves away from zero, as SAS does", {
  expect_equal(round_sas(c(56.25, 171.45, 74.25, 0.15), 1), c(56.3, 171.5, 74.3, 0.2))
  expect_equal(round_sas(c(-2.5, 2.5, 2.4999), 0), c(-3, 3, 2))
  expect_equal(round_sas(NA_real_, 1), NA_real_)
  # R's own round() gives 56.2: the reason this helper exists (discrepancy D-002).
  expect_equal(round(56.25, 1), 56.2)
})

test_that("round_to_spec uses the variable's SignificantDigits, and stops without one", {
  spec <- read_define_v1(fixture)
  spec$variables$sig_dig[spec$variables$variable == "AVAL"] <- 1L
  expect_equal(round_to_spec(3.25, spec, "ADXX", "AVAL"), 3.3)
  expect_error(round_to_spec(3.25, spec, "ADXX", "USUBJID"), "no SignificantDigits")
})

test_that("codelist_lookup translates both ways and refuses unknown values", {
  spec <- read_define_v1(fixture)
  expect_equal(codelist_lookup(spec, "PARAMCD", c("Item 1", NA), from = "decode"), c("ITM1", NA))
  expect_equal(codelist_lookup(spec, "PARAMCD", "TOT", from = "code"), "Total Score")
  expect_error(codelist_lookup(spec, "PARAMCD", "Item 2"), "Not in codelist PARAMCD: Item 2")
  expect_error(codelist_lookup(spec, "NOPE", "x"), "not in the specification")
})

test_that("apply_spec_attributes orders, labels and types the variables", {
  spec <- read_define_v1(fixture)
  data <- data.frame(
    EXTRA = 1, ADT = as.Date("2026-01-05"), AVAL = c(10L, 3L), PARAMCD = c("TOT", "ITM1"),
    USUBJID = "01-001"
  )
  out <- apply_spec_attributes(data, spec, "ADXX")
  expect_equal(names(out), c("USUBJID", "PARAMCD", "AVAL", "ADT"))
  expect_type(out$AVAL, "double")
  expect_s3_class(out$ADT, "Date")
  expect_equal(attr(out$AVAL, "label"), "Analysis Value")
  expect_equal(nrow(check_against_spec(out, spec, "ADXX")), 0)
  expect_error(apply_spec_attributes(data[c("USUBJID", "AVAL")], spec, "ADXX"), "missing specified variables")
})

test_that("compare_with_reference reports each kind of difference", {
  reference <- data.frame(USUBJID = c("1", "2", "3"), X = c(1, 2, 3), C = c("a", "", "c"))
  ours <- data.frame(USUBJID = c("1", "2", "4"), X = c(1, 2.5, 3), C = c("a", NA, "c"), Y = 1)
  res <- compare_with_reference(ours, reference, keys = "USUBJID")
  expect_setequal(res$issue, c(
    "record only in reference", "record only in ours", "variable only in ours", "different values"
  ))
  # A blank in the reference equals NA in ours; only X differs, for subject 2.
  values <- res[res$issue == "different values", ]
  expect_equal(values$variable, "X")
  expect_equal(values$examples, "2: 2 -> 2.5")
})

test_that("identical datasets give no findings, whatever their attributes", {
  data <- data.frame(USUBJID = c("1", "2"), D = as.Date(c("2026-01-01", "2026-01-02")))
  labelled <- data
  attr(labelled$D, "label") <- "A date"
  res <- compare_with_reference(labelled, data, keys = "USUBJID")
  expect_equal(nrow(res), 0)
  expect_named(res, c("variable", "issue", "n", "examples"))
})

test_that("pool_sites pools sites with fewer than 3 patients in any group", {
  site <- c(rep("A", 6), rep("B", 2), rep("C", 4))
  arm <- c(rep(c("P", "D"), 3), "P", "D", "P", "D", "P", "D")
  expect_equal(unique(pool_sites(site, arm)[site == "A"]), "A")
  expect_equal(unique(pool_sites(site, arm)[site != "A"]), "900")
  # B and C together still have only 3 per group, which is enough.
  expect_error(pool_sites(c("A", "A", "A", "A", "A", "A", "B"), c(rep(c("P", "D"), 3), "P")), "still has fewer")
})
