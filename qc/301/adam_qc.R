# Second programming of the ADaM datasets of FJF-2026-301: an independent derivation of the
# variables the analyses depend on, from the SDTM XPT files, in base R, without the
# production helpers. compare_adam_301() compares it with production.

qa_date <- function(x) as.Date(ifelse(!is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x), substr(x, 1, 10), NA))
qa_day <- function(d, r) {
  k <- as.integer(d - r)
  ifelse(k >= 0, k + 1L, k)
}
qa_first <- function(id, date) {
  v <- tapply(as.numeric(date), id, \(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE))
  v
}

# SF4 limits: CRIT1FL high (or weight up by the limit, in %), CRIT2FL low (or weight down).
qa_vs_flags <- function(x, limits) {
  x$PCHG <- NA_real_
  x$CRIT1FL <- NA_character_
  x$CRIT2FL <- NA_character_
  ev <- is.na(x$ABLFL) & !is.na(x$CHG)
  for (p in names(limits)) {
    k <- which(ev & x$PARAMCD == p)
    if (p == "WEIGHT") {
      x$PCHG[k] <- x$CHG[k] / x$BASE[k] * 100
      x$CRIT1FL[k] <- c("N", "Y")[1 + (x$PCHG[k] >= limits[[p]]$increase_pct)]
      x$CRIT2FL[k] <- c("N", "Y")[1 + (x$PCHG[k] <= -limits[[p]]$decrease_pct)]
    } else {
      x$CRIT1FL[k] <- c("N", "Y")[1 + (x$AVAL[k] >= limits[[p]]$high)]
      x$CRIT2FL[k] <- c("N", "Y")[1 + (x$AVAL[k] <= limits[[p]]$low)]
    }
  }
  x
}

#' Second programming of ADaM for FJF-2026-301.
#'
#' @param dir Folder of the SDTM XPT files.
#' @param protocol The protocol design (visits, primary week).
#' @param limits The `vital_sign_limits` of the ADaM specification (SAP SF4).
#' @param tablets_per_day Tablets due per day, for the compliance (2 in the double-dummy FJF-2026-201, ADR 0037).
#' @return Named list with the QC versions of ADSL, ADEFF, ADTTE, ADAE, ADHYPO, ADLB, ADVS,
#'   each with the variables compared.
qc_adam_301 <- function(dir, protocol, limits, tablets_per_day = 1) {
  rd <- function(d) as.data.frame(haven::read_xpt(file.path(dir, paste0(tolower(d), ".xpt"))))
  dm <- rd("DM")
  ds <- rd("DS")
  dm <- dm[dm$ARMCD != "" & !is.na(dm$ARMCD), ]
  id <- dm$USUBJID
  one <- function(keys, vals) vals[match(id, keys)]
  r <- ds[ds$DSDECOD == "RANDOMIZED", ]
  randdt <- one(r$USUBJID, qa_date(r$DSSTDTC))
  trtsdt <- qa_date(dm$RFXSTDTC)
  trtedt <- qa_date(dm$RFXENDTC)
  s <- ds[ds$DSSCAT == "STUDY PARTICIPATION", ]
  eosdt <- one(s$USUBJID, qa_date(s$DSSTDTC))

  ex <- rd("EX")
  da <- rd("DA")
  amt <- function(code) da$DASTRESN[da$DATESTCD == code][match(ex$EXREFID, da$DAREFID[da$DATESTCD == code])]
  ndays <- as.numeric(qa_date(ex$EXENDTC) - qa_date(ex$EXSTDTC)) + 1
  ok <- !is.na(amt("RETAMT")) & !is.na(ndays)
  taken <- rowsum((amt("DISPAMT") - amt("RETAMT"))[ok], ex$USUBJID[ok])
  used <- rowsum(ndays[ok], ex$USUBJID[ok])
  complpct <- unname(round(100 * taken[, 1] / (tablets_per_day * used[, 1]), 1)[id])

  cm <- rd("CM")
  cm <- cm[cm$CMCAT == "RESCUE MEDICATION", ]
  rescdt <- as.Date(unname(qa_first(cm$USUBJID, qa_date(cm$CMSTDTC))[id]))

  lb <- rd("LB")
  vs <- rd("VS")
  baseline <- function(d, code, var, testcd, date, ref) {
    x <- d[d[[var]] == code & !is.na(d[[testcd]]), ]
    x$dt <- qa_date(x[[date]])
    x$ref <- ref[match(x$USUBJID, id)]
    x <- x[!is.na(x$dt) & !is.na(x$ref) & x$dt <= x$ref, ]
    best <- sapply(split(x, x$USUBJID), \(z) z[[testcd]][which.max(as.numeric(z$dt) + seq_len(nrow(z)) / 1e4)])
    unname(best[id])
  }
  hba1cbl <- baseline(lb, "HBA1C", "LBTESTCD", "LBSTRESN", "LBDTC", randdt)
  weightbl <- baseline(vs, "WEIGHT", "VSTESTCD", "VSSTRESN", "VSDTC", randdt)
  height <- one(vs$USUBJID[vs$VSTESTCD == "HEIGHT"], vs$VSSTRESN[vs$VSTESTCD == "HEIGHT"])
  stratum <- {
    supp <- rd("SUPPDM")
    one(supp$USUBJID[supp$QNAM == "STRATUM"], supp$QVAL[supp$QNAM == "STRATUM"])
  }
  adsl <- data.frame(
    USUBJID = id, STRATAR = stratum, RANDDT = randdt, SAFFL = ifelse(is.na(trtsdt), "N", "Y"), TRTSDT = trtsdt,
    TRTEDT = trtedt, TRTDURD = as.integer(trtedt - trtsdt) + 1L, COMPLPCT = complpct, RESCDT = rescdt,
    EOSDT = eosdt, HBA1CBL = hba1cbl, WEIGHTBL = weightbl, BMIBL = round(weightbl / (height / 100)^2, 1)
  )

  # ADEFF: the selected record of each scheduled visit.
  weeks_of <- function(a) {
    w <- protocol$visits$week[vapply(protocol$visits$assessments, \(x) a %in% x, logical(1))]
    w[w > 0 & w <= protocol$primary_visit_week]
  }
  eff <- function(d, var, code, val, date, seq, weeks, impute) {
    x <- d[d[[var]] == code & !is.na(d[[val]]), ]
    x <- data.frame(USUBJID = x$USUBJID, dt = qa_date(x[[date]]), AVAL = x[[val]], seq = x[[seq]])
    x$ADY <- qa_day(x$dt, randdt[match(x$USUBJID, id)])
    t <- 7 * weeks + 1
    edges <- c(1.5, (head(t, -1) + tail(t, -1)) / 2, tail(t, 1) + (tail(t, 1) - c(1, t)[length(t)]) / 2)
    # A day on a midpoint belongs to the earlier visit.
    x$k <- cut(x$ADY, edges, labels = FALSE, right = TRUE)
    x$k[x$ADY <= 1] <- NA
    x <- x[!is.na(x$k), ]
    x$dist <- abs(x$ADY - t[x$k])
    x <- x[order(x$USUBJID, x$k, x$dist, -x$ADY), ]
    x <- x[!duplicated(x[c("USUBJID", "k")]), ]
    grid <- expand.grid(k = seq_along(weeks), USUBJID = id, stringsAsFactors = FALSE)
    g <- merge(grid, x, all.x = TRUE)
    g$AVISITN <- weeks[g$k]
    i <- match(g$USUBJID, id)
    base <- if (code == "HBA1C") hba1cbl else weightbl
    if (impute) {
      m <- tapply(base, stratum, mean, na.rm = TRUE)
      base <- ifelse(is.na(base), round(m[stratum], 4), base)
    }
    g$BASE <- base[i]
    g$CHG <- g$AVAL - g$BASE
    day <- ifelse(is.na(g$dt), randdt[i] + 7 * g$AVISITN, g$dt)
    g$ONTRTFL <- ifelse(!is.na(trtsdt[i]) & day >= trtsdt[i] & day <= trtedt[i] + 7, "Y", NA)
    g$PRERESFL <- ifelse(is.na(rescdt[i]) | day <= rescdt[i], "Y", NA)
    g$ANL02FL <- ifelse(g$ONTRTFL %in% "Y" & g$PRERESFL %in% "Y" & !is.na(g$AVAL), "Y", NA)
    g$CRIT1FL <- if (code == "HBA1C") ifelse(is.na(g$AVAL), NA, ifelse(g$AVAL < 7, "Y", "N")) else NA
    g$PARAMCD <- code
    g[c("USUBJID", "PARAMCD", "AVISITN", "AVAL", "BASE", "CHG", "ONTRTFL", "PRERESFL", "ANL02FL", "CRIT1FL")]
  }
  adeff <- rbind(
    eff(lb, "LBTESTCD", "HBA1C", "LBSTRESN", "LBDTC", "LBSEQ", weeks_of("hba1c"), TRUE),
    eff(vs, "VSTESTCD", "WEIGHT", "VSSTRESN", "VSDTC", "VSSEQ", weeks_of("weight"), FALSE)
  )
  w26 <- adeff$USUBJID[adeff$PARAMCD == "HBA1C" & adeff$AVISITN == protocol$primary_visit_week & adeff$ANL02FL %in% "Y"]
  adsl$PPROTFL <- ifelse(!is.na(adsl$COMPLPCT) & adsl$COMPLPCT >= 80 & adsl$USUBJID %in% w26, "Y", "N")

  # ADHYPO and ADTTE.
  ce <- rd("CE")
  ce <- ce[ce$USUBJID %in% id, ]
  i <- match(ce$USUBJID, id)
  cdt <- qa_date(ce$CESTDTC)
  adhypo <- data.frame(
    USUBJID = ce$USUBJID, CESEQ = ce$CESEQ, HYPOLVL = as.integer(substring(ce$CESCAT, 7)),
    ONTRTFL = ifelse(!is.na(trtsdt[i]) & cdt >= trtsdt[i] & cdt <= trtedt[i] + 7, "Y", NA),
    PRERESFL = ifelse(is.na(rescdt[i]) | cdt <= rescdt[i], "Y", NA)
  )
  end26 <- randdt + 7 * protocol$primary_visit_week
  cens <- pmin(end26, eosdt, na.rm = TRUE)
  ev <- !is.na(rescdt) & rescdt <= cens
  resc <- data.frame(
    USUBJID = id, PARAMCD = "TTRESC",
    AVAL = ifelse(ev, as.numeric(rescdt), as.numeric(cens)) - as.numeric(randdt) + 1, CNSR = 1L - ev
  )
  lstcn <- one(dm$USUBJID, qa_date(dm$RFPENDTC))
  h23 <- ce[as.integer(substring(ce$CESCAT, 7)) >= 2, ]
  h23 <- h23[qa_date(h23$CESTDTC) >= trtsdt[match(h23$USUBJID, id)] %in% TRUE, ]
  first <- as.Date(unname(qa_first(h23$USUBJID, qa_date(h23$CESTDTC))[id]))
  saf <- !is.na(trtsdt)
  ev <- !is.na(first) & first <= lstcn
  hyp <- data.frame(
    USUBJID = id, PARAMCD = "TTHYPO23",
    AVAL = ifelse(ev, as.numeric(first), as.numeric(lstcn)) - as.numeric(trtsdt) + 1, CNSR = 1L - ev
  )[saf, ]
  adtte <- rbind(resc, hyp)

  # ADAE.
  ae <- rd("AE")
  ae <- ae[ae$USUBJID %in% id, ]
  i <- match(ae$USUBJID, id)
  st <- qa_date(ae$AESTDTC)
  en <- qa_date(ae$AEENDTC)
  for (j in which(is.na(st))) {
    p <- ae$AESTDTC[j]
    fd <- trtsdt[i[j]]
    lo <- as.Date(if (nchar(p) == 7) paste0(p, "-01") else paste0(p, "-01-01"))
    hi <- if (nchar(p) == 7) seq(lo, by = "month", length.out = 2)[2] - 1 else as.Date(paste0(p, "-12-31"))
    st[j] <- if (!is.na(fd) && fd >= lo && fd <= hi && (is.na(en[j]) || en[j] >= fd)) fd else lo
  }
  adae <- data.frame(
    USUBJID = ae$USUBJID, AESEQ = ae$AESEQ, ASTDT = st,
    ASEV = ifelse(ae$AESEV %in% c("", NA), "SEVERE", ae$AESEV),
    ASEVIMFL = ifelse(ae$AESEV %in% c("", NA), "Y", NA),
    TRTEMFL = ifelse(!is.na(trtsdt[i]) & st >= trtsdt[i] & st <= trtedt[i] + 14, "Y", NA)
  )

  # ADLB and ADVS.
  bds <- function(d, pre, lab) {
    d <- d[d$USUBJID %in% id & !is.na(d[[paste0(pre, "STRESN")]]), ]
    if (!lab) d <- d[d$VSTESTCD != "HEIGHT", ]
    i <- match(d$USUBJID, id)
    x <- data.frame(
      USUBJID = d$USUBJID, PARAMCD = d[[paste0(pre, "TESTCD")]], SRCSEQ = d[[paste0(pre, "SEQ")]],
      dt = qa_date(d[[paste0(pre, "DTC")]]), AVAL = d[[paste0(pre, "STRESN")]], vnum = d$VISITNUM
    )
    x <- x[!is.na(x$dt), ]
    i <- match(x$USUBJID, id)
    ref <- as.Date(ifelse(is.na(trtsdt[i]), randdt[i], trtsdt[i]))
    x$ABLFL <- NA
    for (g in split(seq_len(nrow(x)), paste(x$USUBJID, x$PARAMCD))) {
      cand <- g[x$dt[g] <= ref[g]]
      if (length(cand)) {
        cand <- cand[order(x$dt[cand], x$SRCSEQ[cand])]
        x$ABLFL[tail(cand, 1)] <- "Y"
      }
    }
    b <- x[x$ABLFL %in% "Y", ]
    x$BASE <- b$AVAL[match(paste(x$USUBJID, x$PARAMCD), paste(b$USUBJID, b$PARAMCD))]
    post <- x$dt > ref
    x$CHG <- ifelse(post, x$AVAL - x$BASE, NA)
    x$ONTRTFL <- ifelse(!is.na(trtsdt[i]) & x$dt > trtsdt[i] & x$dt <= trtedt[i] + 7, "Y", NA)
    wk <- protocol$visits$week[x$vnum]
    x$AVISITN <- ifelse(post & x$vnum %% 1 == 0 & wk > 0, wk, NA)
    x$AVISITN[x$ABLFL %in% "Y"] <- 0
    x <- x[order(x$USUBJID, x$PARAMCD, x$dt, x$SRCSEQ), ]
    visit <- paste(x$USUBJID, x$PARAMCD, x$AVISITN)
    x$ANL01FL <- ifelse(!is.na(x$AVISITN) & x$AVISITN > 0 & !duplicated(visit), "Y", NA)
    if (lab) {
      lo <- d$LBSTNRLO[match(paste(x$USUBJID, x$SRCSEQ), paste(d$USUBJID, d$LBSEQ))]
      hi <- d$LBSTNRHI[match(paste(x$USUBJID, x$SRCSEQ), paste(d$USUBJID, d$LBSEQ))]
      nr <- d$LBNRIND[match(paste(x$USUBJID, x$SRCSEQ), paste(d$USUBJID, d$LBSEQ))]
      sc <- ifelse(nr == "HIGH", x$AVAL / hi, ifelse(nr == "LOW", lo / x$AVAL, 0))
      x$WORSTFL <- NA
      on <- which(x$ONTRTFL %in% "Y")
      for (g in split(on, paste(x$USUBJID[on], x$PARAMCD[on]))) {
        best <- g[order(-sc[g], -as.numeric(x$dt[g]))][1]
        x$WORSTFL[best] <- "Y"
      }
    }
    x$dt <- NULL
    x$vnum <- NULL
    x
  }
  list(
    ADSL = adsl, ADEFF = adeff, ADTTE = adtte, ADAE = adae, ADHYPO = adhypo,
    ADLB = bds(lb, "LB", TRUE), ADVS = qa_vs_flags(bds(vs, "VS", FALSE), limits)
  )
}

#' Compare the production ADaM with the second programming.
#'
#' @param production Named list of the production ADaM datasets.
#' @param qc Result of [qc_adam_301()].
#' @return A data frame with one row per dataset: rows compared, variables, differences.
compare_adam_301 <- function(production, qc) {
  keys <- list(
    ADSL = "USUBJID", ADEFF = c("USUBJID", "PARAMCD", "AVISITN"), ADTTE = c("USUBJID", "PARAMCD"),
    ADAE = c("USUBJID", "AESEQ"), ADHYPO = c("USUBJID", "CESEQ"), ADLB = c("USUBJID", "PARAMCD", "SRCSEQ"),
    ADVS = c("USUBJID", "PARAMCD", "SRCSEQ")
  )
  out <- lapply(names(qc), \(d) {
    p <- as.data.frame(production[[d]])
    if (d == "ADEFF") p <- p[!is.na(p$ANL01FL) & p$AVISITN > 0, ]
    q <- qc[[d]]
    p <- p[names(q)]
    for (v in names(q)) {
      if (inherits(p[[v]], "Date")) {
        p[[v]] <- as.numeric(p[[v]])
        q[[v]] <- as.numeric(q[[v]])
      } else if (is.numeric(p[[v]])) {
        p[[v]] <- as.numeric(p[[v]])
        q[[v]] <- as.numeric(q[[v]])
      } else {
        p[[v]] <- as.character(p[[v]])
        q[[v]] <- as.character(q[[v]])
      }
    }
    diff <- diffdf::diffdf(p, q, keys = keys[[d]], suppress_warnings = TRUE, tolerance = 1e-8)
    data.frame(
      dataset = d, rows = nrow(p), qc_rows = nrow(q), variables = ncol(q),
      clean = !diffdf::diffdf_has_issues(diff)
    )
  })
  do.call(rbind, out)
}
