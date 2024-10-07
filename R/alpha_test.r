# alpha_test (S4 Object) ---------------------------------------------------
#' @title Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing
#' @slot returns A numeric array of returns.
#' @slot return_bmark A numeric array of benchmark returns.
#' @slot weights A list of weights.
#' @slot weights_bmark A list of benchmark weights.
#' @include alpha_test_single_period.R
setClass(
  "alpha_test",
  slots = c(
    returns = "matrix",
    return_bmark = "matrix",
    weights = "list",
    weights_bmark = "list"
  )
)