# The design files of the repository, read once for all tests.
design <- read_design(testthat::test_path("..", "..", ".."))
planning <- planning_truth(design$assumptions)

# Codelists of a study, as the raw layer enforces them.
study_codelists <- function(study) {
  p <- design$protocols[[study]]
  c(
    lapply(design$raw_spec$codelists, as.character),
    list(arm = purrr::map_chr(p$arms, "code"), visit = c(purrr::map_chr(p$visits, "code"), "UNS"))
  )
}
