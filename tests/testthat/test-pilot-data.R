# The benchmark's third-party data: manifest, verification and download (ADR 0015).

manifest_path <- here::here("data", "external", "cdiscpilot01-manifest.csv")

# A fake downloader, so the download logic is tested without the network.
fake_download <- function(content, calls) {
  function(url, destfile) {
    calls$n <- calls$n + 1
    writeBin(charToRaw(content), destfile)
  }
}

one_file_manifest <- function(content) {
  source_file <- tempfile()
  writeBin(charToRaw(content), source_file)
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      file = "sdtm/xx.xpt", role = "input", source_path = "x/xx.xpt",
      bytes = file.size(source_file), git_blob_sha1 = git_blob_sha1(source_file),
      sha256 = file_sha256(source_file)
    ),
    path,
    row.names = FALSE
  )
  path
}

test_that("the manifest lists the 21 pinned files with complete hashes", {
  manifest <- read_pilot_manifest(manifest_path)
  expect_equal(nrow(manifest), 21)
  expect_false(anyDuplicated(manifest$file) > 0)
  expect_true(all(grepl("^[0-9a-f]{64}$", manifest$sha256)))
  expect_true(all(grepl("^[0-9a-f]{40}$", manifest$git_blob_sha1)))
  expect_true(all(manifest$bytes > 0))
  expect_setequal(unique(manifest$role), c("input", "reference", "metadata", "report"))
  expect_equal(sum(manifest$role == "input"), 14)
  expect_equal(sum(manifest$role == "reference"), 4)
})

test_that("hashes match published reference values", {
  path <- tempfile()
  writeBin(charToRaw("abc"), path)
  expect_equal(file_sha256(path), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  # The value `git hash-object` gives for a file containing "abc".
  expect_equal(git_blob_sha1(path), "f2ba8f84ab5c1bce84a7b441cb1959cfc7093b7f")
})

test_that("URLs point at the pinned commit, never at a moving branch", {
  url <- pilot_url("a/b c.xpt")
  expect_match(url, pilot_source$commit, fixed = TRUE)
  expect_match(url, "b%20c.xpt", fixed = TRUE)
  expect_no_match(url, "/(main|master|HEAD)/")
})

test_that("a correct download is kept, and not downloaded again", {
  dest <- withr::local_tempdir()
  calls <- new.env()
  calls$n <- 0
  manifest <- one_file_manifest("good bytes")

  paths <- fetch_pilot_files(manifest, dest, download = fake_download("good bytes", calls))
  expect_true(file.exists(paths))
  expect_equal(calls$n, 1)

  fetch_pilot_files(manifest, dest, download = fake_download("good bytes", calls))
  expect_equal(calls$n, 1)
})

test_that("a download that fails verification is discarded", {
  dest <- withr::local_tempdir()
  calls <- new.env()
  calls$n <- 0
  manifest <- one_file_manifest("good bytes")

  expect_error(
    fetch_pilot_files(manifest, dest, download = fake_download("tampered", calls)),
    "does not match the manifest"
  )
  expect_length(list.files(dest, recursive = TRUE, all.files = TRUE, no.. = TRUE), 0)
})

test_that("a local file that was altered is replaced by a verified copy", {
  dest <- withr::local_tempdir()
  calls <- new.env()
  calls$n <- 0
  manifest <- one_file_manifest("good bytes")
  local <- file.path(dest, "sdtm", "xx.xpt")
  dir.create(dirname(local), recursive = TRUE)
  writeBin(charToRaw("edited by hand"), local)

  expect_warning(
    fetch_pilot_files(manifest, dest, download = fake_download("good bytes", calls)),
    "failed verification"
  )
  expect_equal(rawToChar(readBin(local, "raw", 100)), "good bytes")
})

test_that("downloaded pilot files, when present, all pass verification", {
  manifest <- read_pilot_manifest(manifest_path)
  local <- here::here(pilot_dir(), manifest$file)
  skip_if_not(all(file.exists(local)), "pilot files not downloaded yet")
  for (i in seq_len(nrow(manifest))) {
    expect_length(file_problems(local[i], manifest$bytes[i], manifest$git_blob_sha1[i], manifest$sha256[i]), 0)
  }
})
