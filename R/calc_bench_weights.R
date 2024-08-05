calc_bench_weights <- function(bench_weighting_scheme = "zero", weights) {

  if (bench_weighting_scheme == "zero") {
    return(rep(NA_real_, length(weights)))
  }

  if (bench_weighting_scheme == "weighted") {
    return(weights)
  }

  if (bench_weighting_scheme == "equal-weighted") {
    return(rep(1 / length(weights), length(weights)))
  }

  if (bench_weighting_scheme == "short-weighted") {
    return(-weights)
  }

  if (bench_weighting_scheme == "short-equal-weighted") {
    return(rep(-1 / length(weights), length(weights)))
  }

  stop("unknown bechmark weighting scheme")
}