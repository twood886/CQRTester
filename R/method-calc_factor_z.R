#' @title Calculate Z-Score of Factor Values
#' @description Calculate the Z-Score of Factor Values including lagged
#'  Factor Values in a single_period_at_data S4 object
#' @param at_data A single_period_at_data S4 object.
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#'  defining the windsorization levels.
#' @include utilities.R
#' @include class-single_period_at_data.R
#' @include func-orderedList.R
#' @include class-factor_z_score.R
#' @include gen-calc_factor_z.R
#' @return orderedList S4 object of factor_z_score S4 objects
setMethod(
  "calc_factor_z",
  signature(
    at_data = "single_period_at_data"
  ),
  function(at_data, win_prob = c(0, 1)) {
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
