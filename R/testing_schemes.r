#' @title Factor Z-Score (S4 Object)
#' @description An S4 Class to represent Factor Z-Scores
#' @slot fz Factor Z Scores
setClass(
  "factor_z_score",
  representation(factor_z = "numeric")
)

#' @title Factor Quantile (S4 Object)
#' @description An S4 Class to represent Factor Quantiles
#' @slot score Factor Quantile
setClass(
  "factor_q_score",
  representation(factor_q = "ordered")
)


#' @title Calcualte Z-Score Factor Value
#' @description Calculate Factor Z-Score
#' @param factor_values a numeric array of factor values
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#' @return A vector of the same length as the original data x containing the
#' winsorized and normalized data.
#' @include Utilities_Scoring.R
calc_factor_z <- function(factor_values, win_prob = c(0, 1)) {
  fz <- ctz(factor_values, win_prob)
  new("factor_z_score", factor_z = fz)
}

#' @title Calcualte Quantile Factor Value
#' @description Calculate Factor Z-Score
#' @param factor_values a numeric array of factor values
#' @param quantiles a numeric representing the number of quantiles.
#' @return Ordered quantiles
#' @include Utilities_Scoring.R
calc_factor_q <- function(factor_values, quantiles = 5) {
  fq <- ctq(factor_values, quantiles)
  new("factor_q_score", factor_q = fq)
}
