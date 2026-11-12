rules <- yaml::read_yaml(file.path(test_path("..", ".."), "docs", "specs", "cleaning-rules.yml"))$rules

test_that("every cleaning rule is complete and has a unique id", {
  ids <- purrr::map_chr(rules, "id")
  expect_false(anyDuplicated(ids) > 0)
  for (r in rules) {
    expect_true(all(c("id", "check", "rule", "reason", "acts_in") %in% names(r)), info = r$id)
    expect_true(r$acts_in %in% c("SDTM", "ADaM", "SDTM and ADaM"), info = r$id)
  }
})

test_that("every check of the data-quality report has a cleaning rule", {
  expect_setequal(intersect(raw_quality_checks, purrr::map_chr(rules, "check")), raw_quality_checks)
})
