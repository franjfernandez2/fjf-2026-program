# Runs the whole test suite and fails (non-zero exit) if any test fails.
#   docker compose run --rm analysis Rscript tests/run-tests.R
testthat::test_dir(
  here::here("tests", "testthat"),
  reporter = testthat::ProgressReporter$new(show_praise = FALSE),
  stop_on_failure = TRUE
)
