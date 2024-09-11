calc_bench_weights <- function(bench_weighting_scheme = "zero", weights) {

  if (bench_weighting_scheme == "zero") {
    bmark_weights <- rep(0, length(weights))
  }

  if (bench_weighting_scheme == "weighted") {
    bmark_weights <- weights
  }

  if (bench_weighting_scheme == "equal-weighted") {
    bmark_weights <- rep(1 / length(weights), length(weights))
  }

  if (bench_weighting_scheme == "short-weighted") {
    bmark_weights <- -weights
  }

  if (bench_weighting_scheme == "short-equal-weighted") {
    bmark_weights <- rep(-1 / length(weights), length(weights))
  }

  names(bmark_weights) <- names(weights)
  return(weights)
}