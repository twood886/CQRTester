#' @title Multi Period Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing Parent Calss
#'  for multiple periods.
#' @slot return_fwd_factor ToDo
#' @slot return_lag_factor ToDo
#' @slot return_bmark ToDo
#' @slot weights ToDo
#' @slot weights_bmark ToDo
#' @slot alpha_fwd_return ToDo
#' @slot alpha_lag_factor ToDo
#' @include class-orderedList.R
setClass(
  "multi_period_at",
  slots = c(
    return_fwd_factor = "orderedList",
    return_lag_factor = "orderedList",
    return_bmark = "orderedList",
    weights = "orderedList",
    weights_bmark = "orderedList",
    alpha_fwd_return = "orderedList",
    alpha_lag_factor = "orderedList"
  )
)