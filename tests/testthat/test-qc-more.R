# Helpers of the second programming of the tables (qc/301/tlf_qc_more.R, qc/301/tlf_qc.R).

test_that("the product-limit proportion agrees with survival::survfit", {
  set.seed(7)
  time <- sample(1:60, 40, replace = TRUE)
  event <- rbinom(40, 1, 0.6)
  for (day in c(10, 30, 60, 100)) {
    km <- summary(survival::survfit(survival::Surv(time, event) ~ 1), times = day, extend = TRUE)
    expect_equal(qc_km_event_by(time, event, day), 1 - km$surv)
  }
  expect_equal(qc_km_event_by(c(5, 8), c(0, 0), 10), 0)
})

test_that("arm keys follow the order of the tables, highest dose first", {
  adsl <- data.frame(TRT01A = c("Placebo", "20 mg", "40 mg", "20 mg"), TRT01AN = c(0, 20, 40, 20))
  expect_equal(qc_arm_keys(adsl, "TRT01A"), c("40 mg" = "arm1", "20 mg" = "arm2", Placebo = "arm3"))
})

test_that("a table in long form is found by its header and by its column key, rows two levels down by path", {
  table <- list(
    columns = c(low = "Worst on|low", high = "||high"),
    rows = data.frame(
      label = c("ALT", "Placebo (n=2)", "Baseline low"), indent = c(0L, 1L, 2L),
      low = c("", "", "3"), high = c("", "", "1")
    )
  )
  long <- long_table_qc_301(table)
  hit <- long[long$label == "Baseline low" & long$section == "ALT / Placebo (n=2)", ]
  expect_setequal(hit$column, c("Worst on", "low", "high"))
  expect_equal(hit$value[hit$column == "high"], "1")
})

test_that("a changed cell makes the comparison fail", {
  table <- list(
    id = "T-X", columns = c(arm1 = "Placebo|(N=2)"),
    rows = data.frame(label = "Any", indent = 0L, arm1 = "1 (50.0%)")
  )
  expected <- qc_cell_301("T-X", "Any", "Any", "Placebo", "1 (50.0%)")
  expect_true(compare_tlf_301(list("T-X" = table), expected)$clean)
  expect_false(compare_tlf_301(list("T-X" = table), qc_cell_301("T-X", "Any", "Any", "Placebo", "2 (100.0%)"))$clean)
})

test_that("the product-limit curve agrees with survival::survfit and starts at 1", {
  set.seed(11)
  time <- sample(1:40, 30, replace = TRUE)
  event <- rbinom(30, 1, 0.5)
  curve <- qc_km_curve(time, event)
  fit <- survival::survfit(survival::Surv(time, event) ~ 1)
  expect_equal(curve$time, c(0, fit$time))
  expect_equal(curve$surv, c(1, fit$surv))
})

test_that("a figure's data are compared point by point and by number of points", {
  expected <- data.frame(dose = c(0, 10, 20), est = c(0, -0.5, NA))
  same <- qc_figure_compare("F-X", expected, expected, "dose")
  expect_true(all(same$match))
  changed <- expected
  changed$est[2] <- -0.6
  expect_false(all(qc_figure_compare("F-X", expected, changed, "dose")$match))
  expect_false(all(qc_figure_compare("F-X", expected, expected[1:2, ], "dose")$match))
  # Within the relative tolerance the values agree.
  near <- expected
  near$est[2] <- -0.5 * (1 + 1e-10)
  expect_true(all(qc_figure_compare("F-X", expected, near, "dose")$match))
})

test_that("the data a figure plots are written next to it", {
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 4, 6), g = factor(c("a", "b", "a"))), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  path <- withr::local_tempfile(fileext = ".png")
  out <- write_figure_data_301(plot, path)
  expect_equal(utils::read.csv(out)$y, c(2, 4, 6))
})
