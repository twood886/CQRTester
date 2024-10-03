#' @title Factor Quantile (S4 Object)
#' @description An S4 Class to represent Factor Quantiles
#' @slot score Factor Quantile
setClass(
  "factor_q_score",
  representation(score = "ordered")
)

#' @title Calculate Quantile Factor Value
#' @description Calculate Factor Z-Score
#' @param factor_values a numeric array of factor values
#' @param quantiles a numeric representing the number of quantiles.
#' @return Ordered quantiles
calc_factor_q <- function(factor_data, quantiles = 5) UseMethod("calc_factor_q")
#' @export
calc_factor_q.single_period_factor_data <- function(x, quantiles = 5, .desc = TRUE) { # nolint: line_length_linter.
  fq <- ctq(x@fvals, x@group, quantiles, .desc)
  new("factor_q_score", score = fq)
}

#' @title Calculate Factor Quantile
#' @param values a numeric vector of factor values.
#' @param group a character vector of groups.
#' @param quantiles integer representing the number of quantiles
#' to split data into.
#' @param desc Should values be ranked in secending order?
#' @return company quantile based on factor value
#' @import tidyverse
#' @import DescTools
#' @import forcats
#' @import data.table
ctq <- function(values, group = NA_real_, quantiles = 3, .desc = TRUE) {
  dt <- data.table::data.table(values = values, group = group)

  if (.desc) {
    dt[, values := -values]
  }

  dt[,
    quantile := custom_cut_interval(
      values,
      n = quantiles,
      labels = paste0("Q", quantiles:1),
      right = TRUE
    ),
    by = group
  ]

  dt[, quantile := forcats::fct_na_value_to_level(quantile, level = "NA")]
  dt[, quantile := factor(quantile, ordered = TRUE)]
  out <- dt$quantile
  names(out) <- names(values)
  return(out)
}

#' @title Discretise numeric data into categorical
#' @param x numeric vector.
#' @param n number of intervals to create.
#' @param labels lables for the levels of the resulting category.
#' @param right logical, indicating if the intervals should be closed
#'  on the right (and open on the left) or vice versa.
custom_cut_interval <- function(x, n, labels = NULL, right = TRUE) {
  finite_x <- x[is.finite(x)]

  if (length(finite_x) == 0) {
    warning("No finite values available for calculating intervals. Setting all to NA") # nolint
    return(factor(x, levels = labels))
  }

  if (length(finite_x) < length(labels)) {
    warning("Not enough finite values available for calculating intervals. Setting all to NA") # nolint
    return(factor(rep(NA, length(x)), levels = labels))
  }

  # Calculate range and breaks
  b <- quantile(finite_x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)

  # Use base R cut to assign intervals
  cut(x, breaks = b, labels = labels, right = TRUE, include.lowest = TRUE)
}
