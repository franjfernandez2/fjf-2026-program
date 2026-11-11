# Third-party data for the benchmark: the CDISC SDTM/ADaM Pilot Project (CDISCPILOT01).
#
# The files are not redistributed in this repository (CDISC Terms of Use; ADR 0015).
# The pipeline downloads them from one pinned commit of CDISC's repository and accepts
# a file only if three checks pass against data/external/cdiscpilot01-manifest.csv:
#   - its size in bytes;
#   - its git blob SHA-1, which ties the bytes to the pinned commit on GitHub;
#   - its SHA-256.
# A file that fails any check is never kept.

pilot_source <- list(
  repo = "cdisc-org/sdtm-adam-pilot-project",
  commit = "667511d4b183871d74392ba691c935c38d431d39"
)

pilot_dir <- function() file.path("data", "external", "cdiscpilot01")

manifest_columns <- c("file", "role", "source_path", "bytes", "git_blob_sha1", "sha256")

read_pilot_manifest <- function(path) {
  manifest <- utils::read.csv(path, colClasses = "character")
  if (!identical(names(manifest), manifest_columns)) {
    cli::cli_abort("Manifest {.file {path}} must have columns {.val {manifest_columns}}.")
  }
  manifest$bytes <- as.numeric(manifest$bytes)
  manifest
}

pilot_url <- function(source_path, source = pilot_source) {
  sprintf(
    "https://raw.githubusercontent.com/%s/%s/%s",
    source$repo, source$commit, utils::URLencode(source_path)
  )
}

# The hash git gives a file's contents: SHA-1 of "blob <size>\0" followed by the bytes.
git_blob_sha1 <- function(path) {
  size <- file.size(path)
  header <- c(charToRaw(sprintf("blob %.0f", size)), as.raw(0))
  content <- readBin(path, what = "raw", n = size)
  digest::digest(c(header, content), algo = "sha1", serialize = FALSE)
}

file_sha256 <- function(path) unname(cli::hash_file_sha256(path))

# Returns a character vector of problems; empty when the file passes every check.
file_problems <- function(path, bytes, git_blob_sha1, sha256) {
  if (!file.exists(path)) {
    return("missing")
  }
  problems <- character()
  if (file.size(path) != bytes) problems <- c(problems, "size")
  if (git_blob_sha1(path) != git_blob_sha1) problems <- c(problems, "git blob SHA-1")
  if (file_sha256(path) != sha256) problems <- c(problems, "SHA-256")
  problems
}

download_file <- function(url, destfile) {
  utils::download.file(url, destfile, mode = "wb", quiet = TRUE, method = "libcurl")
}

# Downloads every file in the manifest that is missing or fails a check, and returns the
# local paths (for a targets file target). `download` is injectable so tests run offline.
fetch_pilot_files <- function(manifest_path, dest = pilot_dir(), download = download_file) {
  manifest <- read_pilot_manifest(manifest_path)
  local <- file.path(dest, manifest$file)

  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i, ]
    if (length(file_problems(local[i], row$bytes, row$git_blob_sha1, row$sha256)) == 0) next

    if (file.exists(local[i])) {
      cli::cli_warn("{.file {local[i]}} failed verification; downloading it again.")
    }
    dir.create(dirname(local[i]), recursive = TRUE, showWarnings = FALSE)
    temp <- tempfile(tmpdir = dirname(local[i]))
    on.exit(unlink(temp), add = TRUE)
    download(pilot_url(row$source_path), temp)

    problems <- file_problems(temp, row$bytes, row$git_blob_sha1, row$sha256)
    if (length(problems) > 0) {
      cli::cli_abort(c(
        "Downloaded {.file {row$file}} does not match the manifest.",
        x = "Failed: {problems}.",
        i = "The file was discarded. Nothing unverified is kept."
      ))
    }
    file.rename(temp, local[i])
    cli::cli_inform("Verified {.file {row$file}}.")
  }
  local
}

# The local path of one pilot file, picked from the verified paths that fetch_pilot_files()
# returns, so that targets reading it depend on the verification.
pilot_file <- function(paths, file) {
  match <- paths[endsWith(paths, paste0("/", file))]
  if (length(match) != 1) cli::cli_abort("{.file {file}} is not in the pilot manifest.")
  match
}

# One row per SAS transport file: what it is and how big it is.
inventory_xpt <- function(paths) {
  paths <- paths[grepl("[.]xpt$", paths)]
  rows <- lapply(paths, function(path) {
    data <- haven::read_xpt(path)
    data.frame(
      layer = basename(dirname(path)),
      dataset = toupper(tools::file_path_sans_ext(basename(path))),
      rows = nrow(data),
      columns = ncol(data),
      subjects = if ("USUBJID" %in% names(data)) length(unique(data$USUBJID)) else NA_integer_
    )
  })
  do.call(rbind, rows)
}
