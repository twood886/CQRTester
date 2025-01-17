#' @title Factor Quantile Scores (S4 Object)
#' @description
#' Encapsulates quantile-based factor scores for a given period.
#'
#' @slot score A named vector of factor scores for the quantile-based analysis.
setClass(
  "factor_q_score",
  representation(score = "numeric")
)

setMethod("as.numeric", "factor_q_score", function(x) x@score)