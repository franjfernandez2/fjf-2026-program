rand <- data.frame(
  subject_id = sprintf("S%02d", 1:12),
  stratum = rep(c("A", "B"), each = 6),
  arm_code = rep(c("PBO", "FJF20"), 6)
)

test_that("the dummy assignment keeps the arm counts of each stratum", {
  dummy <- dummy_randomisation(rand, seed = 301301)
  expect_identical(dummy$subject_id, rand$subject_id)
  expect_identical(table(dummy$stratum, dummy$arm_code), table(rand$stratum, rand$arm_code))
  expect_false(identical(dummy$arm_code, rand$arm_code))
  expect_identical(dummy, dummy_randomisation(rand, seed = 301301))
})

test_that("the real assignment is used only once the database is locked", {
  expect_true(assignment_in_use(rand, locked = FALSE, seed = 301301)$blinded)
  real <- assignment_in_use(rand, locked = TRUE, seed = 301301)
  expect_false(real$blinded)
  expect_identical(real$randomisation, rand)
})

test_that("only a lock file with status locked counts as a lock", {
  dir <- withr::local_tempdir()
  path <- lock_file("301", root = dir)
  expect_false(database_locked(path))
  dir.create(dirname(path), recursive = TRUE)
  yaml::write_yaml(list(status = "draft"), path)
  expect_false(database_locked(path))
  yaml::write_yaml(list(status = "locked"), path)
  expect_true(database_locked(path))
})

test_that("a locked study stays blinded until the reviewer authorises unblinding", {
  dir <- withr::local_tempdir()
  expect_false(unblinding_authorised("301", root = dir))
  dir.create(file.path(dir, "docs", "lock"), recursive = TRUE)
  yaml::write_yaml(list(status = "locked"), lock_file("301", root = dir))
  expect_false(unblinding_authorised("301", root = dir))
  dir.create(file.path(dir, "docs", "unblind"), recursive = TRUE)
  yaml::write_yaml(list(status = "draft"), unblind_file("301", root = dir))
  expect_false(unblinding_authorised("301", root = dir))
  yaml::write_yaml(list(status = "authorised"), unblind_file("301", root = dir))
  expect_true(unblinding_authorised("301", root = dir))
  expect_false(unblinding_authorised("201", root = dir))
})

test_that("an authorisation without a lock does not unblind", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "docs", "unblind"), recursive = TRUE)
  yaml::write_yaml(list(status = "authorised"), unblind_file("201", root = dir))
  expect_false(unblinding_authorised("201", root = dir))
})
