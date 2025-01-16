#' @title Calculate Weighted Return from Weights & Returns
#' @description Function to calculate the portfolio return from Weights
#'  and Returns
#' @param weights An array of weights or orderedList of weights
#' @param returns An array of returns or orderedList of returns
#' used to calculate the portfolio weights.
#' @export
setGeneric("calc_return",
  function(weights, returns, ...) standardGeneric("calc_return")
)
