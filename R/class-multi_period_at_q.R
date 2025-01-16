#' @title Multi Period Alpha Test (Factor Quantile) S4 Object
#' @slot factor_quantile A list of factor quantiles.
#' @slot q_returns A list of factor quantile returns.
#' @slot q_stats A list of factor quantile stats.
#' @include class-multi_period_at.R
#' @include class-orderedList.R
setClass(
  "multi_period_at_q",
  contains = "multi_period_at",
  representation(
    factor_quantile = "orderedList",
    q_avg_fwd_returns = "orderedList",
    q_lag_factor_avg_return = "orderedList"
  )
)