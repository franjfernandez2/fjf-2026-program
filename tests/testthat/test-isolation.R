# The analysis side cannot reach the simulator or the sealed seed (ADR 0018). The code under
# R/, studies/ and qc/, comments removed, must not load the simulator package, source its
# files, read the seed file or call the functions that draw or read the truth.

forbidden <- "fjfsim|truth-seed|truth_seed|draw_truth|read_sealed_seed|simulate_study|simulator/|[\"']simulator[\"']"

test_that("analysis code never touches the simulator or the sealed seed", {
  root <- test_path("..", "..")
  dirs <- file.path(root, c("R", "studies", "qc"))
  files <- list.files(dirs, pattern = "[.](R|qmd)$", recursive = TRUE, full.names = TRUE)
  expect_gt(length(files), 10)
  hits <- character()
  for (f in files) {
    code <- sub("#.*$", "", readLines(f, warn = FALSE))
    bad <- grep(forbidden, code)
    if (length(bad) > 0) hits <- c(hits, sprintf("%s:%d", f, bad))
  }
  expect_identical(hits, character())
})

test_that("the isolation check would catch a violation", {
  code <- c(
    "pkgload::load_all('simulator')", "x <- readLines('~/.fjf-2026/truth-seed')",
    "# simulator/ is fine in a comment"
  )
  code <- sub("#.*$", "", code)
  expect_identical(grep(forbidden, code), 1:2)
})
