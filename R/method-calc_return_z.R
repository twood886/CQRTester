#' @title Compute Z-Scores of Returns in Single Period Data
#' @description
#' Calculates Z-scores for return values in a single period alpha test dataset.
#'
#' @param at_data A `single_period_at_data` object containing returns data.
#' @param win_prob A numeric vector of probabilities used for winsorization.
#' @return An `orderedList` of `return_z_score` objects.
#' @include class-single_period_at_data.R
#' @include class-return_z_score.R
#' @include func-orderedList.R
#' @include gen-calc_return_z.R
#' @include utilities.R
#' @export
setMethod(
  "calc_return_z",
  signature(at_data = "single_period_at_data"),
  function(at_data, win_prob = c(0, 1)) {
    z <- lapply(
      at_data@returns@list,
      ctz,
      at_data@weights,
      at_data@group,
      win_prob
    )
    rz <- lapply(z, \(x) new("return_z_score", score = x))
    orderedList(rz, at_data@returns@n, at_data@returns@order)
  }
)