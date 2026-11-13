root <- test_path("..", "..")
protocol <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
sap <- read_sap(file.path(root, "docs", "sap", "fjf-2026-301-sap.yml"))
outputs <- read_outputs(file.path(root, "docs", "sap", "fjf-2026-301-outputs.yml"))
adam_spec <- yaml::read_yaml(file.path(root, "docs", "specs", "adam-301.yml"))
sdtm <- fixture_sdtm_301(root)
adsl0 <- derive_adsl_301(sdtm, protocol, adam_spec)
pps <- add_pps_301(adsl0, derive_adeff_301(sdtm, adsl0, protocol, adam_spec), protocol)
adhypo <- derive_adhypo_301(sdtm, pps$adsl, adam_spec)
adam <- list(
  ADSL = pps$adsl, ADEFF = pps$adeff, ADTTE = derive_adtte_301(pps$adsl, adhypo, protocol, adam_spec),
  ADEX = derive_adex_301(pps$adsl, adam_spec), ADAE = derive_adae_301(sdtm, pps$adsl, adam_spec), ADHYPO = adhypo,
  ADLB = derive_adlb_301(sdtm, pps$adsl, protocol, adam_spec),
  ADVS = derive_advs_301(sdtm, pps$adsl, protocol, adam_spec)
)

# Analysis results in the shape the analyses return; the fixture is too small to fit them.
result <- function(id, ...) {
  tibble::tibble(analysis = id, estimate = -0.5, se = 0.1, lower = -0.7, upper = -0.3, p_value = 0.00002, ...)
}
analysis <- list(
  efficacy = list(
    results = dplyr::bind_rows(
      result("A1", strategy = "JR"), result("A2", strategy = "MMRM", df = 230, covariance = "us"),
      result("S1", strategy = "MAR"), result("S2", strategy = "CR")
    ),
    tipping = tibble::tibble(
      delta = c(0, 0.5, 1), estimate = c(-0.5, -0.2, 0.1), se = 0.1, lower = c(-0.7, -0.4, -0.1),
      upper = c(-0.3, 0, 0.3), p_value = 0.1
    ),
    tipping_point = 0.5
  ),
  secondary = list(
    results = dplyr::bind_rows(
      result("B1",
        proportion_reference = 0.2, proportion_active = 0.4, difference = 0.2, difference_lower = 0.1,
        difference_upper = 0.3
      ),
      result("B2"), result("B3")
    ),
    subgroups = tibble::tibble(
      subgroup = "Age", level = c("<65", ">=65"), n = 1L, estimate = -0.5, lower = -1, upper = 0, interaction_p = 0.5
    )
  ),
  multiplicity = tibble::tibble(
    hypothesis = c("H1", "H2"), label = c("HbA1c change (primary)", "HbA1c below 7%"), analysis = c("A1", "B1"),
    p_one_sided = c(0.00001, 0.03), adjusted_p = c(0.00001, 0.03), rejected = c(TRUE, FALSE)
  ),
  lsmeans = tibble::tibble(
    analysis = rep(c("A1", "A2", "B2"), each = 2), ARMCD = rep(c("PBO", "FJF20"), 3), lsmean = rep(c(-0.2, -0.7), 3),
    se = 0.05
  ),
  sap = sap, protocol = protocol
)

test_that("every output of the specification has its program, and no program is extra", {
  ids <- purrr::map_chr(outputs$outputs, "id")
  expect_setequal(names(tlf_functions_301()), ids)
  for (o in outputs$outputs) {
    expect_true(file.exists(file.path(root, o$program)), info = o$program)
    ext <- if (o$type == "figure") ".png" else ".txt"
    expect_identical(output_file_301(o$id, "x"), file.path("x", paste0(sub("[.]R$", "", basename(o$program)), ext)))
  }
})

tables <- tables_301(adam, analysis, outputs)

test_that("each table carries its specification's title and population", {
  for (id in names(tables)) {
    spec <- output_spec_301(outputs, id)
    expect_identical(tables[[id]]$title, spec$title)
    expect_identical(tables[[id]]$population, spec$population)
    expect_true(nrow(tables[[id]]$rows) > 0, info = id)
  }
})

test_that("the efficacy table prints the analysis's estimate and each arm's LS mean", {
  t <- tables[["T-14.2.1"]]$rows
  expect_identical(t$arm1[t$label == "Difference from placebo (95% CI)"], "-0.50 (-0.70, -0.30)")
  expect_identical(t$arm1[t$label == "p-value (two-sided)"], "<0.0001")
  expect_identical(t$arm1[t$label == "LS mean change (SE)"], "-0.70 (0.050)")
  expect_identical(t$arm2[t$label == "LS mean change (SE)"], "-0.20 (0.050)")
})

test_that("every output says it is blinded until the lock, and not after", {
  dir <- withr::local_tempdir()
  # The two-patient fixture leaves some figure layers empty, which ggplot2 warns about.
  files <- suppressWarnings(write_tlf_301(tables, adam, analysis, outputs, locked = FALSE, dir = dir))
  expect_length(files, length(outputs$outputs))
  expect_true(all(file.exists(files)))
  text <- readLines(files[[1]])
  expect_true(any(grepl("^BLINDED: ", text)))
  locked <- readLines(write_table_301(tables[[1]], file.path(dir, "locked.txt"), locked = TRUE))
  expect_false(any(grepl("BLINDED", locked)))
})

test_that("the second programming agrees with the tables, and finds a changed cell", {
  expected <- qc_tlf_301(adam, analysis)
  comparison <- compare_tlf_301(tables, expected)
  wrong <- comparison$cells[!comparison$cells$match, ]
  expect_true(comparison$clean, info = paste(utils::capture.output(print(wrong)), collapse = "\n"))
  changed <- tables
  r <- changed[["T-14.3.1"]]$rows
  r$arm1[r$label == "Any TEAE"] <- "99 (99.0%)"
  changed[["T-14.3.1"]]$rows <- r
  expect_false(compare_tlf_301(changed, expected)$clean)
})

test_that("number formats follow the shells", {
  expect_identical(npct_301(c(0, 3), c(10, 10)), c("0", "3 (30.0%)"))
  expect_identical(p_301(c(0.00001, 0.04567, NA)), c("<0.0001", "0.0457", ""))
  rows <- continuous_rows_301(c(1, 2, 4), rep("a", 3), "a", digits = 0)
  expect_identical(rows$a, c("3", "2.3 (1.53)", "2.0", "1, 4"))
})
