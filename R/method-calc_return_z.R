#' @title Calculate Z-Score of Returns in Single Period AT Data
#' @include utilities.R
#' @include class-single_period_at_data.R
#' @include func-orderedList.R
#' @include class-return_z_score.R
#' @include gen-calc_return_z.R
#' @return orderedList S4 object of return_z_score S4 objects
setMethod(
  "calc_return_z",
  signature(
    at_data = "single_period_at_data"
  ),
  function(at_data, win_prob = c(0, 1)) {
    z <- lapply(
      at_data@returns@list,
      ctz,
      at_data@weights,
      at_data@group,
      win_prob
    )
    rz <- lapply(z, \(x) new("return_z_score", score = x))
    orderedList(rz, at_data@returns@n, at_data@returns@order)
  }
)
