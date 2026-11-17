# Recovery report: the estimates of the final analyses against the sealed truth (ADR 0018).
#
# This is the reviewer's action. It draws the sealed truth from the seed, computes the true
# quantities that the analyses estimate and writes a table of estimate, 95% interval, truth and
# whether the interval holds the truth. Run it only when the analyses are final, through
# tools/recovery-report.sh, which mounts the seed read-only.
#
# Rehearsal with a public seed (the numbers mean nothing for the study; it only tests the script):
#   docker compose run --rm analysis Rscript tools/recovery-report.R --public-seed 2026 --out <file>
#
# The script was written without ever running on the sealed seed. It needs the pipeline results
# in the targets store (tar_make() done, with the real arms).
args <- commandArgs(trailingOnly = TRUE)
arg <- function(name) {
  i <- match(name, args)
  if (is.na(i)) NULL else args[i + 1]
}
pkgload::load_all(here::here("simulator"), quiet = TRUE)
design <- read_design(here::here())
public <- arg("--public-seed")
out <- arg("--out")
if (is.null(public)) {
  seed <- read_sealed_seed("/run/secrets/truth-seed")
  if (is.null(out)) out <- here::here("docs", "recovery", "recovery-report.md")
} else {
  seed <- as.integer(public)
  if (is.null(out)) stop("A rehearsal needs --out.", call. = FALSE)
}
truth <- draw_truth(design$assumptions, seed)
rm(seed)
curve <- true_curve(truth)
store <- here::here("_targets")
a301 <- targets::tar_read(tlf_301_analysis, store = store)
a201 <- targets::tar_read(tlf_201_analysis, store = store)
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
row <- function(what, estimate, lower, upper, true, note = "") {
  data.frame(
    quantity = what, estimate = fmt(estimate), interval = sprintf("(%s, %s)", fmt(lower), fmt(upper)),
    truth = fmt(true), holds = if (lower <= true && true <= upper) "yes" else "NO", note = note
  )
}

rows <- list()
# FJF-2026-301: 20 mg against placebo at week 26. The truth is the effect on treatment without
# rescue, which is what the hypothetical estimand A2 estimates; A1 (treatment policy) also
# carries the dilution of discontinuation and rescue, so it is not expected to match it.
res <- a301$efficacy$results
true_26 <- dose_effect(curve, 20)
for (id in c("A2", "A1")) {
  r <- res[res$analysis == id, ]
  note <- if (id == "A2") {
    "hypothetical estimand: the matching truth"
  } else {
    "treatment-policy estimand: expected to differ"
  }
  rows[[length(rows) + 1]] <- row(
    sprintf("301 %s, 20 mg vs placebo, week 26", id), r$estimate, r$lower, r$upper, true_26, note
  )
}
# FJF-2026-201: difference from placebo of each dose at week 12 (MMRM means; normal 95% interval).
m <- a201$mcpmod$means
p <- which(m$doses == 0)
z <- stats::qnorm(0.975)
effect_12 <- function(d) dose_effect(curve, d) * build_up(12, curve$tau)
for (i in setdiff(seq_along(m$doses), p)) {
  est <- m$means[i] - m$means[p]
  se <- sqrt(m$S[i, i] - 2 * m$S[i, p] + m$S[p, p])
  rows[[length(rows) + 1]] <- row(
    sprintf("201 %s mg vs placebo, week 12", m$doses[i]), est, est - z * se, est + z * se, effect_12(m$doses[i]),
    "MMRM, hypothetical estimand"
  )
}
table <- do.call(rbind, rows)

# The target dose: the smallest studied dose whose true effect reaches the clinical relevance.
relevance <- a201$sap$primary$mod$target_dose$clinical_relevance
doses <- m$doses[m$doses > 0]
true_target <- doses[effect_12(doses) <= -relevance][1]
chosen <- a201$mcpmod$a1$target_dose
target_text <- sprintf(
  paste(
    "Target dose of FJF-2026-201: chosen %s mg, true %s mg (smallest studied dose whose true effect is at least",
    "%s points better than placebo)."
  ),
  chosen, if (is.na(true_target)) "none" else true_target, relevance
)

lines <- c(
  "# Recovery report: estimates against the sealed truth",
  "",
  paste(
    "**Simulated data · fictional compound.** Produced by `tools/recovery-report.R` by the reviewer,",
    "after the analyses were final."
  ),
  if (!is.null(public)) "**REHEARSAL with a public seed: these numbers mean nothing for the study.**",
  "",
  "| Quantity | Estimate | 95% interval | Truth | Interval holds the truth | Note |",
  "| --- | --- | --- | --- | --- | --- |",
  apply(table, 1, function(r) sprintf("| %s |", paste(r, collapse = " | "))),
  "",
  target_text,
  "",
  "Differences are in points of HbA1c (negative is better). One study gives one interval each: a miss is",
  "expected about one time in twenty and is not by itself a flaw; the E2 study (`studies/e2`) measures how",
  "often each method is expected to hold. Nothing here changes a frozen file or a tagged analysis."
)
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
writeLines(lines[!vapply(lines, is.null, TRUE)], out)
cat("Wrote", out, "\n")
