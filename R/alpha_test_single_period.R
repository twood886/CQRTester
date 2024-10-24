# single_period_at(S4 Object) --------------------------------------------------
#' @title Single Period Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing Parent Class
#'  for a single period
#' @slot date A date object representing the date of the data
#' @slot return portfolio return
#' @slot return_bmark benchmark return
#' @slot weights Portfolio Weights
#' @slot weights_bmark benchmark weights
#' @include factor_data_single_period.R
setClass(
  "single_period_at",
  slots = c(
    date = "Date",
    return_factor = "numeric",
    return_bmark = "numeric",
    weights = "numeric",
    weights_bmark = "numeric"
  )
)

# single_period_at_factor_z (S4 Object) ------------------------------------
#' @title Single Period Alpha Test (Factor Weighted) S4 Object
#' @slot IC Information Coefficient of Factor.
#' @slot factor_z_score factor_z_score object.
#' @slot return_z_score z-score of returns.
#' @include factor_data_single_period.R
setClass(
  "single_period_at_factor_z",
  contains = "single_period_at",
  representation(
    IC = "numeric",
    alpha = "numeric",
    factor_z_score = "factor_z_score",
    return_z_score = "list"
  )
)

# single_period_at_q_spread (S4 Object) ------------------------------------
#' @title Single Period Alpha Test (Quantiles) S4 Object
#' @slot factor_quantile todo
#' @slot q_returns todo
#' @slot q_stats todo
#' @include factor_data_single_period.R
setClass(
  "single_period_at_factor_q",
  contains = "single_period_at",
  representation(
    factor_quantile = "factor_q_score",
    q_returns = "list",
    q_stats = "list"
  )
)

# -------------------------------------------------------------------------
#' @include generic_methods.R
#' @include at_settings.R
#' @include factor_z_score.R
#' @include calc_weights.R
setMethod("alpha_test",
  signature(
    data = "single_period_factor_data",
    .setting = "at_settings_factor_z"
  ),
  function(data, .settings, ...) {
    # Extract Date
    d <- data@date
    # Calculate the Z-Score of Factors
    fz <- calc_factor_z(data, .settings@win_prob)
    # Calculate the Z-Score of Returns
    rz <- lapply(
      data@returns,
      ctz,
      weights = data@weights,
      group = data@group,
      win_prob = .settings@win_prob
    )
    # Calculate the IC
    ic <- sapply(rz, cor, x = fz@score, use = "pairwise.complete.obs")
    # Calculate the weights using the ZScores
    weights <- calc_weights(fz, .settings@weighting_scheme)

    weights_bmark <- calc_bench_weights(
      .settings@benchmark_weighting_scheme,
      data@weights
    )
    # Return
    r <- sapply(
      data@returns,
      \(x, y) as.numeric(x %*% y),
      y = weights,
      simplify = TRUE
    )
    r_bmark <- sapply(
      data@returns,
      \(x, y) as.numeric(x %*% y),
      y = weights_bmark,
      simplify = TRUE
    )

    alpha <- r - r_bmark

    new("single_period_at_factor_z",
      date = d,
      return_factor = r,
      return_bmark = r_bmark,
      weights = weights,
      weights_bmark = weights_bmark,
      IC = ic,
      alpha = alpha,
      factor_z_score = fz,
      return_z_score = rz
    )
  }
)

# -------------------------------------------------------------------------
#' @include generic_methods.R
#' @include at_settings.R
#' @include factor_q_score.R
#' @include calc_weights.R
setMethod("alpha_test",
  signature(
    data = "single_period_factor_data",
    .settings = "at_settings_factor_q"
  ),
  function(data, .settings, ...) {
    # Extract Date
    d <- data@date
    # Calculate the Quantile of Factors
    fq <- calc_factor_q(data, .settings@quantiles)
    # Calculate Weights of portfolio
    weights <- calc_weights(fq, .settings@weighting_scheme)
    # Calculate Weights of Benchmark
    weights_bmark <- calc_bench_weights(
      .settings@benchmark_weighting_scheme,
      data@weights
    )
    # Calculate Portfolio Weights
    r <- sapply(
      data@returns,
      \(x, y) as.numeric(x %*% y),
      y = weights,
      simplify = TRUE
    )
    # Calculate Benchmark Weights
    r_bmark <- sapply(
      data@returns,
      \(x, y) as.numeric(x %*% y),
      y = weights_bmark,
      simplify = TRUE
    )

    .calc_hr <- function(returns, benchmark = 0) {
      length(which(returns > benchmark)) / length(which(!is.na(returns)))
    }
    # Quintile Level Statistics
    q_stats <- list(
      "count" = sapply(
        data@returns,
        \(r, q) tapply(r, q, length),
        q = fq@score,
        simplify = FALSE
      ),
      "average" = sapply(
        data@returns,
        \(r, q) tapply(r, q, mean, na.rm = TRUE),
        q = fq@score,
        simplify = FALSE
      ),
      "median" = sapply(
        data@returns,
        \(r, q) tapply(r, q, median, na.rm = TRUE),
        q = fq@score,
        simplify = FALSE
      ),
      "hit_rate_zero" = sapply(
        data@returns,
        \(r, q) tapply(r, q, .calc_hr, benchmark = 0),
        q = fq@score,
        simplify = FALSE
      ),
      "hit_rate_bench" = mapply(
        \(r, q, b) tapply(r, q, .calc_hr, benchmark = b),
        r = data@returns,
        b = r_bmark,
        MoreArgs = list(q = fq@score),
        SIMPLIFY = FALSE
      )
    )

    new("single_period_at_factor_q",
      date = d,
      return_factor = r,
      return_bmark = r_bmark,
      weights = weights,
      weights_bmark = weights_bmark,
      factor_quantile = fq,
      q_returns = q_stats[["average"]],
      q_stats = q_stats
    )
  }
)