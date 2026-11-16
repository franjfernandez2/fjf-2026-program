root <- test_path("..", "..")
sap_iss <- read_iss_sap(file.path(root, "docs", "sap", "fjf-2026-iss-sap.yml"))
outputs_trt <- read_outputs(file.path(root, "docs", "sap", "fjf-2026-iss-treatment-outputs.yml"))
toy_201 <- toy_study_trt("201", c(0, 5, 10, 20, 40), 30, seed = 11, weeks = 12)
toy_301 <- toy_study_trt("301", c(0, 20), 60, seed = 12, weeks = 26)
pool <- pool_iss_treatment(
  list(`201` = toy_201, `301` = toy_301), sap_iss, unlist(outputs_trt$adam_datasets)
)

test_that("the explorer's safety data hold every column, the exposure and 20 mg against placebo", {
  s <- explorer_safety(pool, sap_iss)
  expect_identical(vapply(s$columns, `[[`, "", "id"), c("PBO", "FJF5", "FJF10", "FJF20", "FJF40", "FJFALL"))
  expect_identical(vapply(s$columns, `[[`, 0L, "patients"), c(90L, 30L, 30L, 90L, 30L, 180L))
  expect_identical(length(s$rows), 8L)
  any_teae <- s$rows[[1]]
  expect_identical(any_teae$label, "Any TEAE")
  ae <- rbind(toy_201$ADAE, toy_301$ADAE)
  ae <- ae[ae$TRTEMFL %in% "Y", ]
  n_pbo <- length(unique(ae$USUBJID[ae$TRTAN == 0]))
  expect_identical(any_teae$columns[[1]]$n, n_pbo)
  expect_equal(any_teae$columns[[1]]$percent, 100 * n_pbo / 90)
  expect_true(any_teae$rd$lower < any_teae$rd$estimate && any_teae$rd$estimate < any_teae$rd$upper)
  # The same risk difference as the table prints.
  two <- iss_two_arms(iss_safety_set(pool, sap_iss))
  printed <- iss_comparison_cells(unique(ae$USUBJID), two)$rd
  expect_identical(printed, iss_est_ci(any_teae$rd$estimate, any_teae$rd$lower, any_teae$rd$upper, 1))
})

test_that("the explorer page embeds the data and nothing at patient level", {
  s <- explorer_safety(pool, sap_iss)
  data <- list(unblinded = TRUE, safety = s)
  path <- write_explorer(data, file.path(root, "app", "template.html"), file.path(tempdir(), "explorer", "index.html"))
  html <- paste(readLines(path), collapse = "\n")
  expect_false(grepl("__DATA__", html, fixed = TRUE))
  expect_match(html, "Simulated data", fixed = TRUE)
  json <- sub(";\\s*const NS = .*$", "", sub("^.*const DATA = ", "", gsub("\n", " ", html)))
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_true(parsed$unblinded)
  expect_identical(length(parsed$safety$columns), 6L)
  # No subject identifier reaches the page.
  expect_false(grepl("FJF-2026-201-0", html, fixed = TRUE))
  expect_false(grepl("FJF-2026-301-0", html, fixed = TRUE))
})
