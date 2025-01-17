#' @title Calculate Z-Score Factor Value
#' @description Calculate Factor Z-Score
#' @param at_data A single_period_at_data S4 object.
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#' @param .desc Should values be ranked in secending order?
#' @export
setGeneric("calc_factor_z",
  function(at_data, win_prob, .desc) {
    standardGeneric("calc_factor_z")
  }
)