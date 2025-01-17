#' @title Multi-Period Alpha Test Data (Quantile-Based) (S4 Object)
#' @description
#' Encapsulates multi-period alpha test results for quantile-based analysis.
#'
#' @slot factor_quantile An `orderedList` of factor quantiles for each period.
#' @slot q_avg_fwd_returns An `orderedList` of average forward returns by
#'   quantile.
#' @slot q_lag_factor_avg_return An `orderedList` of average returns for lagged
#'   factors by quantile.
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