#' @title Factor Z-Score (S4 Object)
#' @description An S4 Class to represent Factor Z-Scores
#' @slot score Factor Z Scores
setClass(
  "factor_z_score",
  representation(score = "numeric")
)

#' @title Calculate Z-Score Factor Value
#' @description Calculate Factor Z-Score
#' @param factor_values a numeric array of factor values
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#' @return A vector of the same length as the original data x containing the
#' winsorized and normalized data.
#' @export
calc_factor_z <- function(factor_data, win_prob = c(0, 1)) UseMethod("calc_factor_z") # nolint: line_length_linter.
#' @export
calc_factor_z.single_period_factor_data <- function(x, win_prob = c(0, 1)) {
  fz <- ctz(x@fvals, x@weights, x@group, win_prob)
  new("factor_z_score", score = fz)
}

#' @title Calculate Factor Z-Score with Winsorization
#' @description Function to calculate normalized value with windsorization.
#' @details Used in Alpha Testing Functions
#' @param values a numeric vector of factor values
#' @param weights a numeric vector of weights.
#' @param group a character vector of grouping.
#' @param win.prob numeric vector of probabilities with values in [0,1]
#' as used in quantile.
#' @return ord
#' @import data.table
#' @export
ctz <- function(
  values, weights = NA_real_, group = NA_real_, win_prob = c(0, 1)
) {

  if (length(weights) == 1 && is.na(weights)) {
    weights <- rep(1, length(values))
  }

  if (length(group) == 1 && is.na(group)) {
    group <- rep("1", length(values))
  }

  dt <- data.table::data.table(
    values = values,
    weights = weights,
    group = group
  )
  # Replace NA weights with 0
  dt[is.na(values), weights := 0]
  dt[is.na(weights), weights := 0]
  # Perform winsorization
  dt[, c("minval", "maxval") := {
    q <- quantile(values, probs = win_prob, na.rm = TRUE)
    .(minval = q[1], maxval = q[2])
  }, by = group]
  dt[, win_values := pmin(pmax(values, minval), maxval)]
  # Calculate grouped weighted mean and standard deviation
  dt[, wmean := sum(values * weights, na.rm = TRUE) / sum(weights), by = group]
  dt[, wvar := {
    avg <- wmean[1]
    dof <- (.N - 1) / .N
    sum(weights * (values - avg)^2, na.rm = TRUE) / (dof * sum(weights))
  }, by = group]
  dt[, wsd := sqrt(wvar)]
  # Compute normalized values
  dt[, norm_x := (win_values - wmean) / wsd]
  # Return the normalized values in the original order
  out <- dt$norm_x
  names(out) <- names(values)
  return(out)
}
