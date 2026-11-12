test_that("the truth lies inside the public ranges and depends only on the seed", {
  p <- design$assumptions$parameters
  a <- draw_truth(design$assumptions, 1234567890)
  expect_identical(names(a), p$id)
  values <- unlist(unclass(a))
  expect_true(all(values >= p$truth_low & values <= p$truth_high))
  expect_identical(draw_truth(design$assumptions, 1234567890), a)
  expect_false(identical(draw_truth(design$assumptions, 1234567891), a))
})

test_that("the truth and the sealed seed never print their values", {
  a <- draw_truth(design$assumptions, 42)
  expect_output(print(a), "not shown")
  expect_false(any(grepl(format(a$change_sd), capture.output(print(a)), fixed = TRUE)))
  f <- withr::local_tempfile()
  writeLines("0123456789", f)
  s <- read_sealed_seed(f)
  expect_identical(format(s), "<sealed seed>")
  expect_false(any(grepl("0123456789", capture.output(print(s)))))
  writeLines("12345", f)
  expect_error(read_sealed_seed(f), "10-digit")
})

test_that("streams of one seed give different random numbers", {
  expect_false(seed_for(42, 1L) == seed_for(42, 2L))
  f <- withr::local_tempfile()
  writeLines("0000000042", f)
  expect_identical(seed_for(read_sealed_seed(f), 3L), seed_for("0000000042", 3L))
})

test_that("the true curve reproduces the assumptions it comes from", {
  curve <- true_curve(planning)
  expect_equal(dose_effect(curve, 20), planning$effect_target_dose)
  expect_equal(dose_effect(curve, 5) / dose_effect(curve, 20), planning$lowest_dose_effect_ratio)
  expect_equal(build_up(12, curve$tau), planning$effect_ratio_week12, tolerance = 1e-8)
  expect_equal(build_up(26, curve$tau), 1)
  expect_equal(rate_over(0.2, 26), 0.2)
})
