# The sealed truth (ADR 0018): the true value of each design parameter, drawn inside its
# public truth range from the sealed seed. The truth is recomputed from the seed whenever it
# is needed and never written to disk, printed or logged before the unblinding.

#' Read the sealed seed without showing it.
#'
#' The file holds one 10-digit number. Its value is returned with a class whose print and
#' format methods hide it, so it cannot reach a console or a log by accident.
#'
#' @param path Path to the seed file.
#' @return An object of class `fjf_sealed_seed`.
read_sealed_seed <- function(path) {
  if (!file.exists(path)) stop("The sealed seed file does not exist.", call. = FALSE)
  value <- readLines(path, n = 1, warn = FALSE)
  if (!grepl("^[0-9]{10}$", value)) stop("The sealed seed file does not hold a 10-digit number.", call. = FALSE)
  structure(list(value = value), class = "fjf_sealed_seed")
}

#' @export
format.fjf_sealed_seed <- function(x, ...) "<sealed seed>"

#' @export
print.fjf_sealed_seed <- function(x, ...) {
  cat("<sealed seed>\n")
  invisible(x)
}

#' An integer seed for R from a seed, and a stream number.
#'
#' Different streams (the truth, each study's patients) get unrelated random numbers from
#' the same seed.
#'
#' @param seed A number, or an `fjf_sealed_seed`.
#' @param stream A small integer naming the use.
#' @return An integer usable by [set.seed()].
seed_for <- function(seed, stream) {
  value <- if (inherits(seed, "fjf_sealed_seed")) seed$value else as.character(seed)
  digits <- as.numeric(strsplit(value, "")[[1]])
  # A polynomial hash of the digits and the stream, modulo a prime below the integer limit.
  h <- 0
  for (d in c(digits, stream)) h <- (h * 31 + d + 1) %% 2147483629
  as.integer(h)
}

#' Draw the truth from a seed.
#'
#' Each parameter is drawn uniformly inside its truth range, in the order of the assumptions
#' file. With a public seed this is a test truth; with the sealed seed it is the sealed truth.
#'
#' @param assumptions The parsed assumptions file (`read_design()$assumptions`).
#' @param seed A public seed, or the result of [read_sealed_seed()].
#' @return An object of class `fjf_truth`: a named list of values.
draw_truth <- function(assumptions, seed) {
  p <- assumptions$parameters
  values <- withr::with_seed(seed_for(seed, 1L), stats::runif(nrow(p), p$truth_low, p$truth_high))
  structure(as.list(stats::setNames(values, p$id)), class = "fjf_truth")
}

#' The planning values as a truth, for tests and comparisons.
#'
#' @param assumptions The parsed assumptions file.
#' @return An object of class `fjf_truth`.
planning_truth <- function(assumptions) {
  p <- assumptions$parameters
  structure(as.list(stats::setNames(p$planning, p$id)), class = "fjf_truth")
}

#' @export
print.fjf_truth <- function(x, ...) {
  cat("<truth: ", length(x), " parameters, not shown>\n", sep = "")
  invisible(x)
}
