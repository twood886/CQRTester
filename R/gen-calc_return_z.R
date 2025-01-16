#' @title Calculate Z-Score of Return Values
#' @description Calculate the Z-Score of Returns Values including forward
#'  Return Values in a single_period_at_data S4 object
#' @param at_data A single_period_at_data S4 object.
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#'  defining the windsorization levels.
#' @export
setGeneric(
  "calc_return_z",
  function(at_data, win_prob = c(0, 1)) {
    standardGeneric("calc_return_z")
  }
)