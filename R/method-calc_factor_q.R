#' @title Calculate Quantile-Based Factor Values
#' @description
#' Computes quantile-based factor scores for a given period using specified
#' quantiles and settings.
#'
#' @param at_data A `single_period_at_data` object containing factor data.
#' @param quantiles A numeric value representing the number of quantiles.
#' @param .desc A logical value indicating whether values should be ranked in
#'   descending order.
#' @return An `orderedList` object containing `factor_q_score` objects.
#' @export
#' @include utilities.R
#' @include class-single_period_at_data.R
#' @include func-ctq.R
#' @include class-factor_q_score.R
#' @include func-orderedList.R
#' @include gen-calc_factor_q.R
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