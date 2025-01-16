#' @title Calculate Quantile Factor Value
#' @description Calculate Factor Quantile
#' @param at_data Alpha Test data
#' @param quantiles a numeric representing the number of quantiles
#' @export
setGeneric("calc_factor_q",
  function(at_data, quantiles, ...) standardGeneric("calc_factor_q")
)
