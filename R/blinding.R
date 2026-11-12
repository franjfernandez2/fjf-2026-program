# Blinded data review (ADR 0027). Until a study's database is locked, the pipeline does not
# use the real treatment assignment: it uses a dummy one, the real arms shuffled within each
# stratum with a public seed. SDTM, ADaM and the outputs are programmed and checked on the
# dummy arms, so nobody on the analysis side sees a result by arm before the lock.
#
# The lock is a file, docs/lock/dblock-<study>.yml, committed by the reviewer together with
# the tag dblock-<study> (tools/lock-database.sh). The pipeline watches the file; the tag
# proves, in the history, that the lock came before any unblinded output.

#' Path of a study's database-lock file.
#'
#' @param study Study code, for example "301".
#' @param root Project root.
#' @return The path, whether or not the file exists.
lock_file <- function(study, root = ".") file.path(root, "docs", "lock", sprintf("dblock-%s.yml", study))

#' Is a study's database locked?
#'
#' @param path The lock file ([lock_file()]).
#' @return TRUE if the file exists and says the database is locked.
database_locked <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  lock <- yaml::read_yaml(path)
  identical(lock$status, "locked")
}

#' Dummy treatment assignment for the blinded data review.
#'
#' The real arm codes are shuffled within each stratum, so the number of patients per arm
#' and stratum is kept and nothing else of the real assignment is.
#'
#' @param randomisation The raw randomisation table.
#' @param seed Public seed.
#' @return `randomisation` with `arm_code` replaced by the dummy assignment.
dummy_randomisation <- function(randomisation, seed) {
  withr::with_seed(seed, {
    for (s in sort(unique(randomisation$stratum))) {
      rows <- which(randomisation$stratum == s)
      randomisation$arm_code[rows] <- randomisation$arm_code[rows][sample.int(length(rows))]
    }
  })
  randomisation
}

#' The treatment assignment the pipeline uses: real after the lock, dummy before it.
#'
#' @param randomisation The raw randomisation table.
#' @param locked Result of [database_locked()].
#' @param seed Public seed of the dummy assignment.
#' @return A list: `randomisation` (the table to use) and `blinded` (TRUE before the lock).
assignment_in_use <- function(randomisation, locked, seed) {
  if (isTRUE(locked)) {
    list(randomisation = randomisation, blinded = FALSE)
  } else {
    list(randomisation = dummy_randomisation(randomisation, seed), blinded = TRUE)
  }
}
