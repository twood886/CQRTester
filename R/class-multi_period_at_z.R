#' @title Multi Period Alpha Test (Factor Z-Score) S4 Object
#' @slot IC_fwd_return ToDo
#' @slot IC_lag_factor ToDo
#' @include class-multi_period_at.R
#' @include class-orderedList.R
setClass(
  "multi_period_at_z",
  contains = "multi_period_at",
  representation(
    IC_fwd_return = "orderedList",
    IC_lag_factor = "orderedList"
  )
)