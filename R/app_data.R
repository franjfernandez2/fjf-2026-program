# Data of the interactive explorer (app/): aggregate results of the studies, never patient rows.
# The explorer is one static HTML page with its data embedded, so it can be hosted without a
# server (the plan names Shiny or teal with shinylive; the environment has neither, and a page
# of this size does not need them). Every number is read from the objects the pipeline made.

#' Aggregate results of the studies for the explorer.
#'
#' @param a301 Analysis results of FJF-2026-301 (`tlf_301_analysis`).
#' @param a201 Analysis results of FJF-2026-201 (`tlf_201_analysis`).
#' @param pool Pooled datasets of the integrated safety analysis ([pool_iss_treatment()]).
#' @param sap_iss The plan of the integrated safety analysis.
#' @param unblinded Whether both studies use the real arms.
#' @return A list that [jsonlite::toJSON()] writes as the explorer's data.
explorer_data <- function(a301, a201, pool, sap_iss, unblinded) {
  num <- function(x) ifelse(is.finite(x), as.numeric(x), NA_real_)
  eff <- a301$efficacy
  labels <- c(
    A1 = "A1 Multiple imputation, jump to reference (primary)", A2 = "A2 MMRM (supplementary)",
    S1 = "S1 Missing at random", S2 = "S2 Copy reference"
  )
  est <- mmrm_estimates_201(a201$mcpmod$means)
  models <- list(a1 = "A1 Primary (hypothetical)", s1 = "S1 Model averaging", s2 = "S2 All observed values")
  list(
    unblinded = isTRUE(unblinded),
    phase3 = list(
      results = lapply(seq_len(nrow(eff$results)), function(i) {
        r <- eff$results[i, ]
        list(
          id = r$analysis, label = unname(labels[r$analysis]), estimate = num(r$estimate), lower = num(r$lower),
          upper = num(r$upper), p = num(r$p_value)
        )
      }),
      tipping = lapply(seq_len(nrow(eff$tipping)), function(i) {
        t <- eff$tipping[i, ]
        list(delta = num(t$delta), estimate = num(t$estimate), lower = num(t$lower), upper = num(t$upper))
      }),
      multiplicity = lapply(seq_len(nrow(a301$multiplicity)), function(i) {
        m <- a301$multiplicity[i, ]
        list(
          hypothesis = m$hypothesis, label = m$label, p_one_sided = num(m$p_one_sided),
          adjusted_p = num(m$adjusted_p), rejected = isTRUE(m$rejected)
        )
      })
    ),
    phase2 = list(
      mmrm = lapply(seq_len(nrow(est)), function(i) {
        list(
          dose = num(est$dose[i]), mean = num(est$mean[i]), diff = num(est$diff[i]),
          lower = num(est$lower[i]), upper = num(est$upper[i])
        )
      }),
      curves = lapply(names(models), function(k) {
        m <- a201$mcpmod[[k]]
        list(
          id = k, label = unname(models[[k]]), model = if (is.null(m$model)) "model average" else m$model,
          doses = as.numeric(a201$mcpmod$means$doses), effects = num(as.numeric(m$effects)),
          target_dose = num(m$target_dose)
        )
      }),
      mct = lapply(seq_len(nrow(a201$mcpmod$a1$mct)), function(i) as.list(a201$mcpmod$a1$mct[i, ])),
      cui = lapply(seq_len(nrow(a201$cui)), function(i) {
        r <- a201$cui[i, ]
        list(
          dose = num(r$dose), effect = num(r$effect), rate = num(r$rate), u_effect = num(r$u_effect),
          u_safety = num(r$u_safety), cui = num(r$cui)
        )
      })
    ),
    safety = explorer_safety(pool, sap_iss)
  )
}

#' Safety by treatment for the explorer: each column of the pool, and 20 mg against placebo.
#'
#' @inheritParams explorer_data
#' @return A list: `columns` (id, label, patients, years) and `rows` (label, per-column n, percent
#'   and exposure-adjusted incidence rate, and the risk difference of 20 mg against placebo).
explorer_safety <- function(pool, sap_iss) {
  s <- iss_safety_set(pool, sap_iss)
  years <- iss_years(pool, s)
  two <- iss_two_arms(s)
  num <- function(x) ifelse(is.finite(x), as.numeric(x), NA_real_)
  hypo <- pool$ADHYPO
  hypo <- hypo[hypo$SAFFL %in% "Y" & hypo$ONTRTFL %in% "Y" & as.integer(hypo$HYPOLVL) >= 2, ]
  groups <- c(
    lapply(iss_overview_rows(teae_301(pool)), function(r) list(label = r$label, records = r$records)),
    list(list(label = "Level 2 or 3 hypoglycaemia (episodes on treatment)", records = hypo))
  )
  rows <- lapply(groups, function(g) {
    cells <- lapply(seq_len(nrow(s$arms)), function(i) {
      n <- length(unique(g$records$USUBJID[g$records$ISSCOL == s$arms$id[i]]))
      list(
        id = s$arms$id[i], n = n, percent = num(100 * n / s$arms$n[i]),
        eair = num(if (years[[i]] > 0) 100 * n / years[[i]] else NA_real_)
      )
    })
    studies <- sort(unique(two$STUDYID))
    cell <- function(col, study) {
      who <- two$USUBJID[two$ISSCOL == col & two$STUDYID == study]
      c(x = sum(who %in% g$records$USUBJID), n = length(who))
    }
    a <- vapply(studies, function(st) cell("FJF20", st), c(x = 0, n = 0))
    p <- vapply(studies, function(st) cell("PBO", st), c(x = 0, n = 0))
    rd <- mh_risk_difference(a["x", ], a["n", ], p["x", ], p["n", ])
    list(
      label = g$label, columns = cells,
      rd = list(estimate = num(100 * rd$estimate), lower = num(100 * rd$lower), upper = num(100 * rd$upper))
    )
  })
  list(
    columns = lapply(seq_len(nrow(s$arms)), function(i) {
      list(id = s$arms$id[i], label = s$arms$label[i], patients = s$arms$n[i], years = num(years[[i]]))
    }),
    rows = rows
  )
}

#' Write the explorer: the template with the data embedded.
#'
#' @param data The result of [explorer_data()].
#' @param template Path to `app/template.html`.
#' @param path Where to write the page.
#' @return The path.
write_explorer <- function(data, template, path) {
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, na = "null", digits = 6, null = "null")
  html <- paste(readLines(template, warn = FALSE), collapse = "\n")
  stopifnot(grepl("__DATA__", html, fixed = TRUE))
  html <- sub("__DATA__", as.character(json), html, fixed = TRUE)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, path)
  path
}
