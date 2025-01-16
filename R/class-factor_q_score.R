#' @title Factor Quantile (S4 Object)
#' @description An S4 Class to represent Factor Quantiles
#' @slot score Factor Quantile
setClass(
  "factor_q_score",
  representation(score = "ordered")
)

setMethod("as.numeric", "factor_q_score", function(x) x@score)