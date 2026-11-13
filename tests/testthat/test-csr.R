toy_table <- function() {
  list(
    id = "T-14.9.9", study = "FJF-2026-301", title = "A toy table", population = "SAF", program = "x.R",
    columns = c(arm1 = "Drug|(N=2)", arm2 = "Placebo|(N=1)"),
    rows = data.frame(
      label = c("Any event", "Pain | ache", "Total"), indent = c(0L, 1L, 0L),
      arm1 = c("", "1 (50.0%)", "2"), arm2 = c("", "0", "1")
    ),
    footnotes = "A footnote."
  )
}

test_that("a pipeline table becomes a Markdown table with its headings, indents and footnotes", {
  md <- csr_table(toy_table())
  expect_match(md[1], "^\\*\\*Table 14.9.9: A toy table\\*\\* \\(Safety set\\)$")
  expect_true("|  | Drug<br>(N=2) | Placebo<br>(N=1) |" %in% md)
  expect_true("| **Any event** |  |  |" %in% md)
  expect_true(any(grepl("^\\| &nbsp;&nbsp;&nbsp;Pain \\\\\\| ache \\| 1 \\(50.0%\\) \\| 0 \\|$", md)))
  expect_true("| Total | 2 | 1 |" %in% md)
  expect_true(any(grepl("A footnote.", md, fixed = TRUE)))
})

test_that("a blinded report says so on its first page, in its results and in its discussion", {
  b <- csr_blinding(FALSE)
  expect_true(b$blinded)
  expect_true(any(grepl("BLINDED REPORT", b$banner)))
  expect_true(any(grepl("Dummy arms", b$results_note)))
  expect_match(paste(csr_discussion(FALSE), collapse = ""), "Pending the unblinding")
  u <- csr_blinding(TRUE)
  expect_false(u$blinded)
  expect_length(u$banner, 0)
  expect_length(u$results_note, 0)
  expect_no_match(paste(csr_discussion(TRUE), collapse = ""), "Pending")
})

test_that("the second-programming summary is clean only when every level agrees", {
  sdtm <- data.frame(domain = c("DM", "AE"), rows = c(10L, 5L), clean = c(TRUE, TRUE))
  adam <- data.frame(dataset = "ADSL", rows = 10L, clean = TRUE)
  tlf <- list(cells = data.frame(id = c("T-1", "T-1", "T-2")), clean = TRUE)
  q <- csr_qc_summary(sdtm, adam, tlf)
  expect_identical(q$clean, c(TRUE, TRUE, TRUE))
  expect_identical(q$units, c("2 domains", "1 datasets", "3 cells"))
  expect_identical(q$compared[3], "2 tables")
  tlf$clean <- FALSE
  expect_false(csr_qc_summary(sdtm, adam, tlf)$clean[3])
})

test_that("figures are found among the files the pipeline wrote", {
  files <- c("studies/301/output/tlf/f_14_2_1.png", "studies/301/output/tlf/t_14_2_1.txt")
  expect_identical(csr_figure("F-14.2.1", files, "/r")[1], "![](/r/studies/301/output/tlf/f_14_2_1.png){width=100%}")
  expect_error(csr_figure("F-14.3.1", files, "/r"), "no single file")
})

test_that("data frames become Markdown tables, with pipes escaped", {
  md <- csr_markdown(data.frame(a = c("x|y", "z"), b = 1:2))
  expect_identical(md[1:4], c("| a | b |", "| --- | --- |", "| x\\|y | 1 |", "| z | 2 |"))
})
