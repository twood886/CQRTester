#' @title Single Period Alpha Test (Quantiles) S4 Object
#' @slot factor_quantile todo
#' @slot q_returns todo
#' @slot q_stats todo
#' @include class-single_period_at.R
#' @include class-orderedList.R
setClass(
  "single_period_at_q",
  contains = "single_period_at",
  representation(
    factor_quantile = "orderedList",
    q_avg_fwd_return = "orderedList",
    q_lag_factor_avg_return = "orderedList"
  )
)