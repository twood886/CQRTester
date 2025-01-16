#' @title Return Z-Score (S4 Object)
#' @description An S4 Class to represent Return Z-Scores
#' @slot score Return Z-Score
setClass(
  "return_z_score",
  representation(score = "numeric")
)

setMethod("as.numeric", "return_z_score", function(x) x@score)