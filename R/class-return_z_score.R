#' @title Return Z-Score (S4 Object)
#' @description
#' Represents Z-scores of returns for a single period in alpha testing.
#'
#' @slot score A numeric vector containing Z-scores for returns.
setClass(
  "return_z_score",
  representation(score = "numeric")
)

setMethod("as.numeric", "return_z_score", function(x) x@score)