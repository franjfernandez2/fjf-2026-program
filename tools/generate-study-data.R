# Generates the raw data of FJF-2026-201 and FJF-2026-301 (ADR 0018).
#
# Sealed run, once, after the SAPs are frozen: use tools/generate-study-data.sh, which checks
# the tags and mounts the seed file read-only at /run/secrets/truth-seed.
#
# Rehearsal with a public seed, into any directory but data/raw:
#   docker compose run --rm analysis Rscript tools/generate-study-data.R --public-seed 2026 --out <dir>
#
# The script prints row counts only. It never prints the seed or the truth.
args <- commandArgs(trailingOnly = TRUE)
arg <- function(name) {
  i <- match(name, args)
  if (is.na(i)) NULL else args[i + 1]
}
pkgload::load_all(here::here("simulator"), quiet = TRUE)
design <- read_design(here::here())
store <- here::here("_targets")
n_per_arm <- c(
  `201` = targets::tar_read(sample_size_201, store = store),
  `301` = targets::tar_read(sample_size_301, store = store)
)
public <- arg("--public-seed")
if (is.null(public)) {
  if (!identical(Sys.getenv("FJF_TAGS_CHECKED"), "1")) {
    stop("A sealed run goes through tools/generate-study-data.sh, which checks the SAP tags.", call. = FALSE)
  }
  seed <- read_sealed_seed("/run/secrets/truth-seed")
  out <- here::here("data", "raw")
} else {
  seed <- as.integer(public)
  out <- arg("--out")
  same <- function(a, b) normalizePath(a, mustWork = FALSE) == normalizePath(b, mustWork = FALSE)
  if (is.null(out) || same(out, here::here("data", "raw"))) {
    stop("A rehearsal needs --out, and it cannot be data/raw.", call. = FALSE)
  }
}
if (file.exists(file.path(out, "manifest.csv"))) stop("Raw data already exist in that directory.", call. = FALSE)
manifest <- write_programme(generate_programme(design, seed, n_per_arm), out)
rm(seed)
print(manifest[c("file", "rows")], row.names = FALSE)
