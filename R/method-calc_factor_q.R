#' @title Calculate Quantile of Factor Values
#' @description Calculate the Quantile of Factor Values including lagged
#'  Factor Values in a single_period_at_data S4 object
#' @param at_data A single_period_at_data S4 object.
#' @param quantiles a numeric representing the number of quantiles
#' @include utilities.R
#' @include class-single_period_at_data.R
#' @include func-ctq.R
#' @include class-factor_q_score.R
#' @include func-orderedList.R
#' @include gen-calc_factor_q.R
#' @return orderedList S4 object of factor_q_score S4 objects
setMethod(
  "calc_factor_q",
  signature(
    at_data = "single_period_at_data"
  ),
  function(at_data, quantiles = 5, .desc = TRUE) {
    q <- lapply(
      at_data@fvals@list,
      ctq,
      at_data@group,
      quantiles,
      .desc
    )
    fq <- lapply(q, \(x) new("factor_q_score", score = x))
    orderedList(fq, at_data@fvals@n, at_data@fvals@order)
  }
)
