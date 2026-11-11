# Code quality checks run by continuous integration. Exits non-zero on any problem.
#   docker compose run --rm analysis Rscript tests/check-style.R

# 1. Lint: possible bugs and style problems (rules in .lintr).
lints <- lintr::lint_dir(".", pattern = "[.]R$")
if (length(lints) > 0) {
  print(lints)
  stop(length(lints), " lint(s) found.", call. = FALSE)
}
cli::cli_alert_success("lintr: no lints.")

# 2. Format: the code must already be formatted as styler (tidyverse style) would format it.
files <- c(
  list.files(c("R", "studies", "tests", "tools"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
  "_targets.R"
)
unstyled <- styler::style_file(files, dry = "on")
changed <- unstyled$file[unstyled$changed]
if (length(changed) > 0) {
  stop("Not formatted with styler: ", paste(changed, collapse = ", "),
    "\nRun styler::style_file() on them.",
    call. = FALSE
  )
}
cli::cli_alert_success("styler: {length(files)} file(s) correctly formatted.")
