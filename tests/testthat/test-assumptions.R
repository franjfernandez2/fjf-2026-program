test_that("the design assumptions file is complete", {
  a <- read_assumptions(test_path("..", "..", "docs", "design", "assumptions.yml"))
  expect_equal(check_assumptions(a), character())
  expect_gt(nrow(a$parameters), 10)
})

test_that("check_assumptions reports each kind of problem", {
  a <- list(
    sources = list(s1 = list(citation = "A", url = "https://example.org"), s2 = list(citation = "B")),
    parameters = tibble::tibble(
      id = c("ok", "outside", "nosource", "unknown", "ok"),
      description = "d", unit = "u",
      planning = c(1, 5, 1, 1, 1), truth_low = 0, truth_high = 2,
      sources = list("s1", "s1", "none", "s9", "s1"),
      evidence = c("e", "e", "short", "e", "e")
    ),
    design_rules = list(list(id = "rule", sources = list())),
    calibration_targets = list()
  )
  problems <- check_assumptions(a)
  expect_true(any(grepl("duplicated parameter: ok", problems)))
  expect_true(any(grepl("^outside: planning value outside", problems)))
  expect_true(any(grepl("^nosource: no source and no justification", problems)))
  expect_true(any(grepl("^unknown: unknown or missing source", problems)))
  expect_true(any(grepl("^rule: unknown or missing source", problems)))
  expect_true(any(grepl("source s2: citation or URL missing", problems)))
})
