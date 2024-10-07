#' @title Calculate Benchmark Weight for Alpha Testing
#' @description Function to calculate the weights for the benchmark returns.
#' @param bench_weighting_scheme Character representing the weighting scheme
#'  to be applied. Possible values include "zero", "weighted", "equal-weighted",
#'  "short-weighted", and "short-equal-weighted"
#' @param weights Numeric array of given weights.
calc_bench_weights <- function(bench_weighting_scheme = "zero", weights) {

  # Zero Weight
  # All weights will be set to zero
  if (bench_weighting_scheme == "zero") {
    bmark_weights <- rep(0, length(weights))
  }
  # Weighted
  # Provided weights will be returned
  if (bench_weighting_scheme == "weighted") {
    bmark_weights <- weights
  }
  # Equal-Weighted
  # All weights will be set to 1/n
  if (bench_weighting_scheme == "equal-weighted") {
    bmark_weights <- rep(1 / length(weights), length(weights))
  }
  # Short-Weighted
  # Provided weights will be multiplied by -1
  if (bench_weighting_scheme == "short-weighted") {
    bmark_weights <- -weights
  }
  # Short-Equal-Weighted
  # All weights will be set to -1/n
  if (bench_weighting_scheme == "short-equal-weighted") {
    bmark_weights <- rep(-1 / length(weights), length(weights))
  }

  names(bmark_weights) <- names(weights)
  return(bmark_weights)
}
