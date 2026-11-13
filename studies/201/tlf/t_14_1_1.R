# T-14.1.1 Disposition of patients.
# The program of FJF-2026-301 (studies/301/tlf/t_14_1_1.R) without its per-protocol row: the
# SAP of FJF-2026-201 has no per-protocol set.

#' @inheritParams t_14_2_1_201
t_14_1_1_201 <- function(adam, analysis, outputs) {
  table <- t_14_1_1_301(adam, analysis, outputs)
  table$rows <- table$rows[table$rows$label != "Per-protocol set", ]
  table
}
