# Table 14-2.01: Summary of Demographic and Baseline Characteristics (SAP section 9.2).
# Population: intent-to-treat (ITTFL = "Y"). Source: our ADSL.

# The pilot's race groups: race and ethnicity combined, as in the study report.
race_origin <- function(race, ethnic) {
  dplyr::case_when(
    ethnic == "HISPANIC OR LATINO" ~ "Hispanic",
    race == "WHITE" ~ "Caucasian",
    race == "BLACK OR AFRICAN AMERICAN" ~ "African Descent",
    .default = "Other"
  )
}

#' Build Table 14-2.01 from ADSL.
#'
#' Continuous variables: n, mean, SD, median, min and max, with a one-way ANOVA p-value
#' across treatments. Categorical variables: n (%) with a Pearson chi-square p-value.
#' @param adsl Our ADSL.
#' @return A tibble with `variable`, `statistic`, one column per treatment group and
#'   Total, and `p_value`, all as printed text.
make_t_14_2_01 <- function(adsl) {
  itt <- adsl |>
    dplyr::filter(ITTFL == "Y") |>
    dplyr::mutate(
      TRT = factor(TRT01P, levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")),
      RACEGR = race_origin(RACE, ETHNIC)
    )
  groups <- c(split(itt, itt$TRT), list(Total = itt))
  cols <- c("placebo", "low", "high", "total")

  continuous <- function(variable, var) {
    stat <- function(f, digits) vapply(groups, function(g) fmt_num(f(stats::na.omit(g[[var]])), digits), "")
    rows <- rbind(
      vapply(groups, function(g) as.character(sum(!is.na(g[[var]]))), ""),
      stat(mean, 1), stat(stats::sd, 2), stat(stats::median, 1), stat(min, 1), stat(max, 1)
    )
    p <- stats::anova(stats::lm(stats::reformulate("TRT", var), data = itt))[["Pr(>F)"]][1]
    out <- tibble::as_tibble(as.data.frame(rows, stringsAsFactors = FALSE), .name_repair = ~cols)
    tibble::tibble(variable, statistic = c("n", "Mean", "SD", "Median", "Min", "Max")) |>
      dplyr::bind_cols(out) |>
      dplyr::mutate(p_value = c(fmt_p(p), rep("", 5)))
  }

  categorical <- function(variable, var, levels, labels = levels, with_n = TRUE) {
    counts <- vapply(groups, function(g) table(factor(g[[var]], levels = levels)), numeric(length(levels)))
    counts <- matrix(counts, nrow = length(levels))
    denom <- vapply(groups, function(g) sum(!is.na(g[[var]])), numeric(1))
    cells <- vapply(seq_along(groups), function(j) fmt_n_pct(counts[, j], denom[j]), character(length(levels)))
    cells <- matrix(cells, nrow = length(levels))
    observed <- counts[, 1:3, drop = FALSE]
    p <- suppressWarnings(stats::chisq.test(observed[rowSums(observed) > 0, , drop = FALSE], correct = FALSE)$p.value)
    out <- tibble::as_tibble(as.data.frame(cells, stringsAsFactors = FALSE), .name_repair = ~cols)
    rows <- tibble::tibble(variable, statistic = labels) |>
      dplyr::bind_cols(out) |>
      dplyr::mutate(p_value = c(fmt_p(p), rep("", length(levels) - 1)))
    if (!with_n) {
      return(rows)
    }
    n_row <- tibble::tibble(variable, statistic = "n", !!!stats::setNames(as.list(as.character(denom)), cols))
    dplyr::bind_rows(dplyr::mutate(n_row, p_value = rows$p_value[1]), dplyr::mutate(rows, p_value = ""))
  }

  dplyr::bind_rows(
    continuous("Age (y)", "AGE"),
    categorical("Age (y)", "AGEGR1", c("<65", "65-80", ">80"), c("<65 yrs", "65-80 yrs", ">80 yrs"), with_n = FALSE),
    categorical("Sex", "SEX", c("M", "F"), c("Male", "Female")),
    categorical("Race (Origin)", "RACEGR", c("Caucasian", "African Descent", "Hispanic", "Other")),
    continuous("MMSE", "MMSETOT"),
    continuous("Duration of disease", "DURDIS"),
    categorical("Duration of disease", "DURDSGR1", c("<12", ">=12"), c("<12 months", ">=12 months"), with_n = FALSE),
    continuous("Years of education", "EDUCLVL"),
    continuous("Baseline weight(kg)", "WEIGHTBL"),
    continuous("Baseline height(cm)", "HEIGHTBL"),
    continuous("Baseline BMI", "BMIBL"),
    categorical("Baseline BMI", "BMIBLGR1", c("<25", "25-<30", ">=30"), with_n = FALSE)
  )
}
