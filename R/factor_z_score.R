#' @title Factor Z-Score (S4 Object)
#' @description An S4 Class to represent Factor Z-Scores
#' @slot score Factor Z Scores
setClass(
  "factor_z_score",
  representation(score = "numeric")
)

setMethod("as.numeric", "factor_z_score", function(x) x@score)

#' @title Calculate Z-Score Factor Value
#' @description Calculate Factor Z-Score
#' @param factor_values Factor Values.
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#' @include utilities.R
#' @include orderedList.R
#' @export
calc_factor_z <- function(factor_data, win_prob = c(0, 1)) UseMethod("calc_factor_z") # nolint: line_length_linter.

#' @title Calculate Z-Score of Factor Values
#' @description Calculate the Z-Score of Factor Values including lagged
#'  Factor Values in a single_period_at_data S4 object
#' @param x A single_period_at_data S4 object.
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#'  defining the windsorization levels.
#' @include utilities.R
#' @include orderedList.R
#' @return orderedList S4 object of factor_z_score S4 objects
calc_factor_z.single_period_at_data <- function(x, win_prob = c(0, 1)) {
  z <- lapply(x@fvals@list, ctz, x@weights, x@group, win_prob)
  fz <- lapply(z, \(x) new("factor_z_score", score = x))
  new("orderedList", list = fz, n = x@fvals@n, order = x@fvals@order)
}




#' @export
calc_factor_z.single_period_factor_data <- function(x, win_prob = c(0, 1)) {
  fz <- ctz(x@fvals, x@weights, x@group, win_prob)
  new("factor_z_score", score = fz)
}
