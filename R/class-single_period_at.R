#' @title Single Period Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing Parent Class
#'  for a single period
#' @slot date A date object representing the date of the data
#' @slot return portfolio return
#' @slot return_bmark benchmark return
#' @slot weights Portfolio Weights
#' @slot weights_bmark benchmark weights
#' @include class-orderedList.R
setClass(
  "single_period_at",
  slots = c(
    date = "Date",
    return_fwd_factor = "orderedList",
    return_lag_factor = "orderedList",
    return_bmark = "orderedList",
    alpha_fwd_return = "orderedList",
    alpha_lag_factor = "orderedList",
    weights = "orderedList",
    weights_bmark = "orderedList"
  )
)