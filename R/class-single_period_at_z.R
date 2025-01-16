#' @title Single Period Alpha Test (Factor Weighted) S4 Object
#' @slot IC Information Coefficient of Factor.
#' @slot factor_z_score factor_z_score object.
#' @slot return_z_score z-score of returns.
#' @include class-single_period_at.R
#' @include class-orderedList.R
setClass(
  "single_period_at_z",
  contains = "single_period_at",
  representation(
    IC_fwd_return = "orderedList",
    IC_lag_factor = "orderedList",
    factor_z_score = "orderedList",
    return_z_score = "orderedList"
  )
)