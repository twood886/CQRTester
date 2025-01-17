#' @title Factor Z-Score (S4 Object)
#' @description
#' Represents Z-scores of factor values for a single period in alpha testing.
#'
#' @slot score A numeric vector containing Z-scores for factors.
setClass(
  "factor_z_score",
  representation(score = "numeric")
)

setMethod("as.numeric", "factor_z_score", function(x) x@score)