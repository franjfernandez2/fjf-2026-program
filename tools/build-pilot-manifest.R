# Fills the sha256 column of data/external/cdiscpilot01-manifest.csv. Run once, when the
# pinned commit or the list of files changes:
#   docker compose run --rm --no-deps analysis Rscript tools/build-pilot-manifest.R
#
# Each file is accepted only if its size and git blob SHA-1 match the pinned commit's tree,
# as listed by the GitHub API. Its SHA-256 is then recorded, and from then on the pipeline
# checks all three on every run.

source("R/pilot_data.R")

manifest_path <- file.path("data", "external", "cdiscpilot01-manifest.csv")
manifest <- read_pilot_manifest(manifest_path)
local <- file.path(pilot_dir(), manifest$file)

for (i in which(manifest$sha256 == "")) {
  dir.create(dirname(local[i]), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile(tmpdir = dirname(local[i]))
  download_file(pilot_url(manifest$source_path[i]), temp)

  if (file.size(temp) != manifest$bytes[i] || git_blob_sha1(temp) != manifest$git_blob_sha1[i]) {
    unlink(temp)
    cli::cli_abort("{.file {manifest$file[i]}} does not match the pinned commit; discarded.")
  }
  manifest$sha256[i] <- file_sha256(temp)
  file.rename(temp, local[i])
  cli::cli_inform("{.file {manifest$file[i]}}: matches commit {substr(pilot_source$commit, 1, 7)}.")
}

manifest$bytes <- format(manifest$bytes, scientific = FALSE, trim = TRUE)
utils::write.csv(manifest, manifest_path, row.names = FALSE, quote = FALSE)
cli::cli_alert_success("{nrow(manifest)} files recorded in {.file {manifest_path}}.")
