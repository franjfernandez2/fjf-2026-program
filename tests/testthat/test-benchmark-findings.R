# Gate 1: each benchmark finding must be linked to the discrepancy-log row that explains it.

toy_log <- function() {
  tibble::tibble(
    id = c("D-001", "D-002", "D-003", "D-004"),
    source = c(
      "ADSL vs official ADSL", "ADQSADAS vs official ADQSADAS", "Table 14-5.01 vs study report",
      "ADSL vs official ADSL"
    ),
    dataset = c("ADSL", "ADQSADAS", "ADAE", "ADSL"),
    variable = c("BMIBLGR1", "AVAL; ADT", "(p-values)", "AGE"),
    status = c("explained", "explained", "explained", "open")
  )
}

test_that("a finding is linked by source, dataset and variable", {
  log <- toy_log()
  expect_equal(link_to_log("ADSL", "BMIBLGR1", log, "^ADSL vs official ADSL"), "D-001")
  # Same variable, but a different comparison: not explained by that row.
  expect_equal(link_to_log("ADSL", "BMIBLGR1", log, "^ADAE vs official ADAE"), "")
  # An open row explains nothing.
  expect_equal(link_to_log("ADSL", "AGE", log, "^ADSL vs official ADSL"), "")
})

test_that("unmatched records are linked through the comparison's keys", {
  log <- toy_log()
  pattern <- "^ADQSADAS vs official ADQSADAS"
  expect_equal(link_to_log("ADQSADAS", NA, log, pattern, keys = c("USUBJID", "ADT")), "D-002")
  expect_equal(link_to_log("ADQSADAS", NA, log, pattern, keys = c("USUBJID", "AVISIT")), "")
})

test_that("a printed row is linked by the table's source alone", {
  expect_equal(link_to_log(NA, NA, toy_log(), "^Table 14-5[.]01 vs study report"), "D-003")
})

test_that("collect_benchmark_findings leaves QC differences and unlogged findings unexplained", {
  log <- toy_log()
  finding <- function(variable, issue = "different values") {
    tibble::tibble(variable = variable, issue = issue, n = 1, examples = "")
  }
  none <- finding(character())
  spec <- data.frame(dataset = character(), variable = character(), check = character(), found = character())
  printed <- tibble::tibble(row = 5, label = "VOMITING", ours = "a", printed = "b")
  result <- collect_benchmark_findings(
    adam = list(ADSL = finding(c("BMIBLGR1", "SEX")), ADQSADAS = none),
    keys = list(ADSL = "USUBJID", ADQSADAS = "USUBJID"),
    xpt = tibble::add_column(finding("BMIBLGR1"), dataset = "ADSL", .before = 1),
    spec = spec,
    csr = list(`14-5.01` = printed, `14-2.01` = tibble::tibble(row = integer(), label = character())),
    qc = finding("AGE"),
    log = log
  )
  expect_equal(result$explained_by, c("D-001", "", "D-001", "D-003", ""))
  expect_equal(result$comparison[5], "QC vs production")
})

test_that("every finding of the last pipeline run is explained in the discrepancy log", {
  store <- here::here("_targets")
  skip_if_not("benchmark_findings" %in% targets::tar_objects(store = store), "pipeline not run yet")
  findings <- targets::tar_read(benchmark_findings, store = store)
  unexplained <- findings[findings$explained_by == "", ]
  expect_equal(nrow(unexplained), 0, info = paste(unexplained$comparison, unexplained$dataset, unexplained$item))
})
