#' @title Calculate Quantile Factor Value
#' @description Calculate Factor Quantile
#' @param at_data Alpha Test data
#' @param quantiles a numeric representing the number of quantiles
#' @param .desc Should values be ranked in secending order?
#' @export
setGeneric("calc_factor_q",
  function(at_data, quantiles, .desc, ...) standardGeneric("calc_factor_q")
)