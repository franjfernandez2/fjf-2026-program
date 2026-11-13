# T-14.1.2 Demographic and baseline characteristics. Source: ADSL, full analysis set.

#' @inheritParams t_14_1_1_301
t_14_1_2_301 <- function(adam, analysis, outputs) {
  adsl <- adam$ADSL[adam$ADSL$FASFL == "Y", ]
  arms <- arms_301(adsl, "TRT01P")
  col <- arm_key_301(adsl$TRT01P, arms)
  keys <- c(arms$key, "total")
  denom <- c(stats::setNames(arms$n, arms$key), total = nrow(adsl))
  # Every patient counts in the total column as well as in their arm's.
  both <- function(x) c(x, x)
  cols <- c(col, rep("total", nrow(adsl)))
  continuous <- function(label, var, digits) {
    bind_rows_301(heading_301(label), continuous_rows_301(both(adsl[[var]]), cols, keys, digits))
  }
  categorical <- function(label, var, levels = NULL) {
    bind_rows_301(heading_301(label), categorical_rows_301(both(adsl[[var]]), cols, keys, denom, levels))
  }
  race <- sort(unique(adsl$RACE))
  rows <- bind_rows_301(
    continuous("Age (years)", "AGE", 0),
    categorical("Age group, n (%)", "AGEGR1", c("<65 years" = "<65", ">=65 years" = ">=65")),
    categorical("Sex, n (%)", "SEX", c(Female = "F", Male = "M")),
    categorical("Race, n (%)", "RACE", stats::setNames(race, tools::toTitleCase(tolower(race)))),
    continuous("Body-mass index (kg/m2)", "BMIBL", 1),
    continuous("Body weight (kg)", "WEIGHTBL", 1),
    continuous("HbA1c at baseline (%)", "HBA1CBL", 1),
    categorical("Screening HbA1c stratum, n (%)", "STRATAR", c("<=8.5%" = "<=8.5", ">8.5%" = ">8.5"))
  )
  tlf_table_301("T-14.1.2", outputs, arm_columns_301(arms, total = TRUE), rows,
    footnotes = "SD: standard deviation. The stratum is the one used at randomisation."
  )
}
