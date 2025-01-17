#' @title Calculate Z-Score-Based Factor Values
#' @description
#' Computes Z-score-based factor values, applying winsorization if specified.
#'
#' @param at_data A `single_period_at_data` object containing factor data.
#' @param win_prob A numeric vector specifying percentile cutoffs for
#'   winsorization.
#' @param .desc A logical value indicating whether values should be ranked in
#'   descending order.
#' @return An `orderedList` object containing `factor_z_score` objects.
#' @export
#' @include utilities.R
#' @include class-single_period_at_data.R
#' @include func-orderedList.R
#' @include class-factor_z_score.R
#' @include gen-calc_factor_z.R
setMethod(
  "calc_factor_z",
  signature(
    at_data = "single_period_at_data"
  ),
  function(at_data, win_prob = c(0, 1), .desc = TRUE) {
    z <- lapply(
      at_data@fvals@list,
      ctz,
      at_data@weights,
      at_data@group,
      win_prob
    )
    fz <- lapply(z, \(x) new("factor_z_score", score = x))
    orderedList(fz, at_data@fvals@n, at_data@fvals@order)
  }
)