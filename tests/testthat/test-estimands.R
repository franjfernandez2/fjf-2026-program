root <- test_path("..", "..")
p301 <- read_protocol(file.path(root, "docs", "protocols", "fjf-2026-301.yml"))
e301 <- read_estimands(file.path(root, "docs", "protocols", "fjf-2026-301-estimands.yml"))

test_that("the 301 estimands are complete", {
  expect_equal(check_estimands(e301, p301), character())
  primary <- purrr::keep(e301$estimands, \(e) e$role == "primary")[[1]]
  expect_equal(unique(purrr::map_chr(primary$intercurrent_events, "strategy")), "treatment policy")
})

test_that("check_estimands finds an incomplete estimand", {
  bad <- e301
  bad$estimands[[2]]$role <- "primary"
  bad$estimands[[2]]$summary <- ""
  bad$estimands[[3]]$intercurrent_events[[2]] <- NULL
  bad$estimands[[4]]$intercurrent_events[[1]]$strategy <- "ignore it"
  problems <- check_estimands(bad, p301)
  expect_true("there must be exactly one primary estimand" %in% problems)
  expect_true("E2: attribute 'summary' missing" %in% problems)
  expect_true("E3: no strategy for discontinuation" %in% problems)
  expect_true("E4: unknown strategy ignore it" %in% problems)
})

test_that("estimand_table gives the five attributes", {
  t <- estimand_table(e301$estimands[[1]], e301$intercurrent_events)
  expect_equal(nrow(t), 5)
  expect_match(t$Definition[4], "treatment policy")
})
