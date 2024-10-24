#' @title Return Z-Score (S4 Object)
#' @description An S4 Class to represent Return Z-Scores
#' @slot score Return Z-Score
setClass(
  "return_z_score",
  representation(score = "numeric")
)

setMethod("as.numeric", "return_z_score", function(x) x@score)

#' @title Calculate Z-Score of Returns
#' @description Calculate Factor Z-Score
#' @param return_data Returns 
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#' @include utilities.R
#' @include orderedList.R
#' @export
calc_return_z <- function(return_data, win_prob = c(0, 1)) UseMethod("calc_return_z") # nolint: line_length_linter.

#' @title Calculate Z-Score of Return Values
#' @description Calculate the Z-Score of Returns Values including forward
#'  Return Values in a single_period_at_data S4 object
#' @param x A single_period_at_data S4 object.
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#'  defining the windsorization levels.
#' @include utilities.R
#' @include orderedList.R
#' @return orderedList S4 object of return_z_score S4 objects
calc_return_z.single_period_at_data <- function(x, win_prob = c(0, 1)) {
  z <- lapply(x@returns@list, ctz, x@weights, x@group, win_prob)
  rz <- lapply(z, \(x) new("return_z_score", score = x))
  new("orderedList", list = rz, n = x@returns@n, order = x@returns@order)
}
