root <- test_path("..", "..")
outputs_iss <- read_outputs(file.path(root, "docs", "sap", "fjf-2026-iss-outputs.yml"))

# A study with two arms of two patients; `drug` names the arms so that a test can check that no
# arm reaches the pooled tables.
toy_study_iss <- function(study, drug) {
  ids <- sprintf("FJF-2026-%s-%d", study, 1:4)
  arm <- rep(c(drug, "Placebo"), each = 2)
  adsl <- data.frame(
    STUDYID = sprintf("FJF-2026-%s", study), USUBJID = ids, SAFFL = "Y", FASFL = "Y",
    TRT01A = arm, TRT01AN = rep(c(20, 0), each = 2), TRT01P = arm, TRT01PN = rep(c(20, 0), each = 2)
  )
  adae <- data.frame(
    USUBJID = ids[c(1, 1, 3)], SAFFL = "Y", TRTEMFL = "Y", TRTA = arm[c(1, 1, 3)], TRTAN = c(20, 20, 0),
    AEREL = c("RELATED", "NOT RELATED", "NOT RELATED"), ASEV = "MILD", AESER = c("N", "Y", "N"),
    AEACN = "DOSE NOT CHANGED", AEOUT = "RECOVERED"
  )
  others <- lapply(c("ADHYPO", "ADLB", "ADVS", "ADEX"), function(ds) data.frame(USUBJID = ids, TRTA = arm))
  c(list(ADSL = adsl, ADAE = adae), stats::setNames(others, c("ADHYPO", "ADLB", "ADVS", "ADEX")))
}

test_that("every ISS table names an FJF-2026-301 program, and its own program exists", {
  ids <- purrr::map_chr(outputs_iss$outputs, "id")
  expect_false(anyDuplicated(ids) > 0)
  for (o in outputs_iss$outputs) {
    expect_true(o$source %in% names(iss_source_functions()), info = o$id)
    expect_true(file.exists(file.path(root, o$program)), info = o$program)
    expect_true(all(sub("\\..*", "", unlist(o$adam)) %in% outputs_iss$adam_datasets), info = o$id)
  }
})

test_that("pooling puts each patient in their study and in the pooled column, and drops the arms", {
  pooled <- pool_iss(list(`201` = toy_study_iss("201", "Dose A"), `301` = toy_study_iss("301", "Dose B")), outputs_iss)
  adsl <- pooled$ADSL
  expect_identical(nrow(adsl), 16L)
  expect_setequal(unique(adsl$TRT01A), c("FJF-2026-201", "FJF-2026-301", "All patients"))
  expect_identical(as.vector(table(adsl$TRT01A)[c("FJF-2026-201", "FJF-2026-301", "All patients")]), c(4L, 4L, 8L))
  for (d in pooled) {
    arms <- unlist(d[intersect(iss_treatment_vars, names(d))])
    expect_false(any(arms %in% c("Dose A", "Dose B", "Placebo")))
  }
  # The pooled column comes first (highest code).
  expect_identical(arms_301(adsl, "TRT01A")$label[1], "All patients")
})

test_that("a patient in two studies stops the pooling", {
  a <- toy_study_iss("201", "Dose A")
  b <- a
  expect_error(pool_iss(list(`201` = a, `301` = b), outputs_iss), "more than one study")
})

test_that("a pooled table counts every patient once per column and says it is not by treatment", {
  pooled <- pool_iss(list(`201` = toy_study_iss("201", "Dose A"), `301` = toy_study_iss("301", "Dose B")), outputs_iss)
  spec <- outputs_iss
  spec$outputs <- purrr::keep(spec$outputs, function(o) o$id == "T-ISS-3")
  t <- tables_iss(pooled, spec)[["T-ISS-3"]]
  expect_identical(t$id, "T-ISS-3")
  expect_identical(unname(t$columns), c("All patients|(N=8)", "FJF-2026-301|(N=4)", "FJF-2026-201|(N=4)"))
  any_teae <- t$rows[t$rows$label == "Any TEAE", ]
  expect_identical(c(any_teae$arm1, any_teae$arm2, any_teae$arm3), c("4 (50.0%)", "2 (50.0%)", "2 (50.0%)"))
  expect_true(any(grepl("no result is by treatment", t$footnotes)))
  path <- write_tlf_iss(list(`T-ISS-3` = t), file.path(tempdir(), "iss"))
  text <- readLines(path)
  expect_match(text[2], "^Table ISS-3: Overview of treatment-emergent adverse events, pooled over arms$")
  expect_false(any(grepl("dummy assignment", text)))
})

test_that("the footnotes of the 301 programs lose their reference to arms", {
  f <- iss_footnotes(c(
    "SD: standard deviation. The stratum is the one used at randomisation.",
    "Sorted by the number of patients with the event on FJF-2026. MedDRA system organ class and preferred term."
  ))
  expect_false(any(grepl("on FJF-2026[.]|randomisation", f)))
  expect_true(any(grepl("pooled column", f)))
})
