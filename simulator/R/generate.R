# Generation of the programme's raw data: both studies from one seed, with data errors,
# written as CSV files with a manifest of hashes. The truth is drawn, used and dropped: it
# is never returned or written.

#' Generate the raw data of both studies.
#'
#' @param design The result of [read_design()].
#' @param seed The sealed seed ([read_sealed_seed()]), or a public seed for a rehearsal.
#' @param n_per_arm Named vector of patients per arm, `c("201" = ..., "301" = ...)`.
#' @return A named list (`"201"`, `"301"`) of raw-table lists.
generate_programme <- function(design, seed, n_per_arm) {
  truth <- draw_truth(design$assumptions, seed)
  out <- lapply(c("201", "301"), function(study) {
    tables <- simulate_study(design, truth, study, n_per_arm[[study]], seed)
    add_data_errors(tables, seed, study, design$raw_spec)$tables
  })
  stats::setNames(out, c("201", "301"))
}

#' Write the raw tables of the programme as CSV files, with a manifest.
#'
#' Files go to `<dir>/<study>/<table>.csv`: UTF-8, every field quoted, missing values empty.
#' The manifest (`<dir>/manifest.csv`) lists each file with its rows and SHA-256 hash.
#'
#' @param programme The result of [generate_programme()].
#' @param dir Output directory.
#' @return The manifest, invisibly.
write_programme <- function(programme, dir) {
  rows <- list()
  for (study in names(programme)) {
    sdir <- file.path(dir, study)
    dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
    for (t in names(programme[[study]])) {
      f <- file.path(sdir, paste0(t, ".csv"))
      utils::write.csv(programme[[study]][[t]], f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      rows[[length(rows) + 1]] <- data.frame(
        file = file.path(study, paste0(t, ".csv")), rows = nrow(programme[[study]][[t]]), sha256 = sha256_file(f)
      )
    }
  }
  manifest <- do.call(rbind, rows)
  utils::write.csv(manifest, file.path(dir, "manifest.csv"), row.names = FALSE)
  invisible(manifest)
}

#' SHA-256 of a file.
#'
#' @param path File path.
#' @return The hash as a hexadecimal string.
sha256_file <- function(path) digest::digest(file = path, algo = "sha256")
