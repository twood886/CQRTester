#' @title Factor Z-Score (S4 Object)
#' @description An S4 Class to represent Factor Z-Scores
#' @slot score Factor Z Scores
setClass(
  "factor_z_score",
  representation(score = "numeric")
)

setMethod("as.numeric", "factor_z_score", function(x) x@score)