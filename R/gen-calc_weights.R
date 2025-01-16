#' @title Calculate Weight from Factor Score
#' @description Function to calculate the portfolio weights for a given scoring
#' and weighting scheme
#' @param score A factor score object
#' @param weighting_scheme A string representing the weighting scheme to be
#' used to calculate the portfolio weights.
#' @export
setGeneric("calc_weights",
  function(score, weighting_scheme, ...) standardGeneric("calc_weights")
)
