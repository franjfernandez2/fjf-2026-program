# A study with the ADaM datasets the integrated analysis reads. `doses` gives the dose of each
# arm; every arm has `n` patients. Events, episodes and laboratory values come from `seed`.
toy_study_trt <- function(study, doses, n, seed, weeks) {
  set.seed(seed)
  dose <- rep(doses, each = n)
  ids <- sprintf("FJF-2026-%s-%03d", study, seq_along(dose))
  arm <- ifelse(dose == 0, "Placebo + metformin", sprintf("FJF-2026 %d mg + metformin", dose))
  days <- pmax(1L, as.integer(round(weeks * 7 - stats::rexp(length(ids), 1 / 10))))
  adsl <- data.frame(
    STUDYID = sprintf("FJF-2026-%s", study), USUBJID = ids, SAFFL = "Y", TRT01A = arm, TRT01AN = dose,
    TRT01P = arm, TRT01PN = dose
  )
  adex <- data.frame(
    USUBJID = ids, TRTA = arm, TRTAN = dose, SAFFL = "Y", TRTDURD = days, TRTDURY = days / 365.25,
    COMPLPCT = round(stats::runif(length(ids), 70, 125), 1)
  )
  terms <- data.frame(
    AEDECOD = c("Nausea", "Diarrhoea", "Headache", "Dizziness", "Nasopharyngitis"),
    AEBODSYS = c(
      "GASTROINTESTINAL DISORDERS", "GASTROINTESTINAL DISORDERS", "NERVOUS SYSTEM DISORDERS",
      "NERVOUS SYSTEM DISORDERS", "INFECTIONS AND INFESTATIONS"
    )
  )
  n_ae <- stats::rpois(length(ids), 0.8 + dose / 40)
  who <- rep(seq_along(ids), n_ae)
  pick <- sample(nrow(terms), length(who), replace = TRUE)
  adae <- data.frame(
    USUBJID = ids[who], TRTA = arm[who], TRTAN = dose[who], SAFFL = "Y",
    TRTEMFL = sample(c("Y", NA), length(who), replace = TRUE, prob = c(0.9, 0.1)),
    AEDECOD = terms$AEDECOD[pick], AEBODSYS = terms$AEBODSYS[pick],
    AEREL = sample(c("RELATED", "NOT RELATED"), length(who), replace = TRUE),
    ASEV = sample(c("MILD", "MODERATE", "SEVERE"), length(who), replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    AESER = sample(c("Y", "N"), length(who), replace = TRUE, prob = c(0.05, 0.95)),
    AEACN = sample(
      c("DOSE NOT CHANGED", "DRUG WITHDRAWN", "DRUG INTERRUPTED"), length(who),
      replace = TRUE, prob = c(0.9, 0.05, 0.05)
    ),
    AEOUT = "RECOVERED"
  )
  n_hypo <- stats::rnbinom(length(ids), size = 0.5, mu = 0.4 + dose / 50)
  who <- rep(seq_along(ids), n_hypo)
  adhypo <- data.frame(
    USUBJID = ids[who], TRTA = arm[who], TRTAN = dose[who], SAFFL = "Y",
    HYPOLVL = sample(1:3, length(who), replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    ONTRTFL = sample(c("Y", NA), length(who), replace = TRUE, prob = c(0.9, 0.1))
  )
  lab <- expand.grid(i = seq_along(ids), PARAMCD = c("ALT", "AST", "BILI"), visit = 1:3, stringsAsFactors = FALSE)
  adlb <- data.frame(
    USUBJID = ids[lab$i], TRTA = arm[lab$i], TRTAN = dose[lab$i], SAFFL = "Y", PARAMCD = lab$PARAMCD,
    ANRHI = ifelse(lab$PARAMCD == "BILI", 1.2, 40), ONTRTFL = ifelse(lab$visit == 3, NA, "Y")
  )
  adlb$AVAL <- adlb$ANRHI * stats::rlnorm(nrow(adlb), log(0.6), 0.5)
  # One patient of the first arm is a potential Hy's law case.
  case <- adlb$USUBJID == ids[1] & adlb$ONTRTFL %in% "Y"
  adlb$AVAL[case] <- adlb$ANRHI[case] * 4
  list(ADSL = adsl, ADAE = adae, ADHYPO = adhypo, ADLB = adlb, ADEX = adex)
}
