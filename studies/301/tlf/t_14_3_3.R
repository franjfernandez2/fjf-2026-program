# T-14.3.3 Hypoglycaemia by level. Source: ADHYPO (on treatment, ONTRTFL), ADEX (TRTDURY), safety set.

#' @inheritParams t_14_2_1_301
t_14_3_3_301 <- function(adam, analysis, outputs) {
  s <- safety_set_301(adam)
  hypo <- adam$ADHYPO[adam$ADHYPO$SAFFL == "Y" & adam$ADHYPO$ONTRTFL %in% "Y", ]
  ex <- adam$ADEX[adam$ADEX$SAFFL == "Y", ]
  years <- vapply(s$arms$label, function(l) sum(ex$TRTDURY[ex$TRTA == l], na.rm = TRUE), 0)
  block <- function(label, d) {
    patients <- patients_301(d, rep(TRUE, nrow(d)), s)
    cells <- list()
    for (i in seq_len(nrow(s$arms))) {
      k <- s$arms$key[i]
      n_ep <- sum(d$TRTA == s$arms$label[i])
      cells[[paste0(k, "_n")]] <- patients[[k]]
      cells[[paste0(k, "_e")]] <- as.character(n_ep)
      cells[[paste0(k, "_rate")]] <- fmt_num(100 * n_ep / years[[i]], 1)
    }
    table_rows_301(label, cells)
  }
  level <- as.integer(hypo$HYPOLVL)
  rows <- bind_rows_301(
    block("Any level", hypo),
    block("Level 1 (glucose <3.9 and >=3.0 mmol/L)", hypo[level == 1, ]),
    block("Level 2 (glucose <3.0 mmol/L)", hypo[level == 2, ]),
    block("Level 3 (severe, needing assistance)", hypo[level == 3, ]),
    block("Level 2 or 3", hypo[level >= 2, ])
  )
  columns <- unlist(lapply(seq_len(nrow(s$arms)), function(i) {
    stats::setNames(
      c(
        sprintf("%s|(N=%d)|n (%%)", s$arms$label[i], s$arms$n[i]), "||E",
        sprintf("|PY=%s|E/100 PY", fmt_num(years[[i]], 1))
      ),
      paste0(s$arms$key[i], c("_n", "_e", "_rate"))
    )
  }))
  tlf_table_301("T-14.3.3", outputs, columns, rows,
    footnotes = c(
      "Episodes on treatment (ADHYPO.ONTRTFL). n (%): patients with at least one episode. E: episodes.",
      "E/100 PY: episodes per 100 patient-years of exposure (ADEX.TRTDURY).",
      "Levels as classified by the American Diabetes Association."
    )
  )
}
