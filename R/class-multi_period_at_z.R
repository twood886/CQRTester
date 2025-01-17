#' @title Multi-Period Alpha Test Data (Z-Score-Based) (S4 Object)
#' @description
#' Represents multi-period alpha test data for Z-score-based analysis.
#'
#' @slot IC_fwd_return An `orderedList` of information coefficients for
#'   forward returns.
#' @slot IC_lag_factor An `orderedList` of information coefficients for lagged
#'   factors.
#' @include class-orderedList.R
setClass(
  "multi_period_at_z",
  contains = "multi_period_at",
  representation(
    IC_fwd_return = "orderedList",
    IC_lag_factor = "orderedList"
  )
)
