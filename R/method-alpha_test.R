#' @title Alpha Test Method for Single Period AT
#' @include class-single_period_at_data.R
#' @include class-at_settings_factor_z.R
#' @include gen-calc_factor_z.R
#' @include gen-calc_return_z.R
#' @include gen-calc_IC.R
#' @include gen-calc_weights.R
#' @include func-orderedList.R
#' @include func-calc_bench_weights.R
#' @include gen-calc_return.R
#' @include class-single_period_at_z.R
#' @include gen-alpha_test.R
#' @export
setMethod("alpha_test",
  signature(
    data = "single_period_at_data",
    .setting = "at_settings_factor_z"
  ),
  function(data, .settings, ...) {
    # Extract Date
    d <- data@date
    # Calculate the Z-Score of Factors
    fz <- calc_factor_z(data, .settings@win_prob)
    # Calculate the Z-Score of Returns
    rz <- calc_return_z(data, .settings@win_prob)
    # Calculate IC of current scores on Fwd Returns
    ic_fwd_return <- calc_ic(fz[[0]], rz)
    # Calculate IC of lagged scores on Next Period Return
    ic_lag_factor <- calc_ic(fz, rz[[1]])
    # Calculate the weights using the ZScores
    weights <- calc_weights(fz, .settings@weighting_scheme)
    # Calculate benchmark weights
    weights_bmark <- orderedList(
      list("bench_weight_0" = calc_bench_weights(
        .settings@benchmark_weighting_scheme,
        data@weights
      )),
      n = 1,
      order = c(0)
    )
    # Calculate Returns of Current Scores on Fwd Returns
    score_fwd_return <- calc_return(weights[[0]], data@returns)
    # Calculate Returns of Lagged Scores on t+1 Returns
    lag_score_return <- calc_return(weights, data@returns[[1]])
    # Calculate Returns of Benchmark on Fwd Returns
    bmark_fwd_return <- calc_return(weights_bmark[[0]], data@returns)
    # Calculate Alpha
    calc_alpha <- function(x, y) {
      orderedList(
        mapply(
          \(x, y, n) setNames(x - y, str_replace(n, "return", "alpha")),
          x@list, y, names(x@list),
          USE.NAMES = FALSE,
          SIMPLIFY = FALSE
        ),
        x@n,
        x@order
      )
    }

    alpha_fwd_return <- calc_alpha(score_fwd_return, bmark_fwd_return@list)
    alpha_lag_factor <- calc_alpha(lag_score_return, bmark_fwd_return@list[[1]])

    new("single_period_at_z",
      date = d,
      return_fwd_factor = score_fwd_return,
      return_lag_factor = lag_score_return,
      return_bmark = bmark_fwd_return,
      weights = weights,
      weights_bmark = weights_bmark,
      IC_fwd_return = ic_fwd_return,
      IC_lag_factor = ic_lag_factor,
      alpha_fwd_return = alpha_fwd_return,
      alpha_lag_factor = alpha_lag_factor,
      factor_z_score = fz,
      return_z_score = rz
    )
  }
)

#' @include class-single_period_at_data.R
#' @include class-at_settings_factor_q.R
#' @include gen-calc_factor_q.R
#' @include gen-calc_weights.R
#' @include func-orderedList.R
#' @include func-calc_bench_weights.R
#' @include gen-calc_return.R
#' @include gen-calc_q_metrics.R
#' @include class-single_period_at_q.R
#' @include gen-alpha_test.R
#' @export
setMethod("alpha_test",
  signature(
    data = "single_period_at_data",
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
    weights_bmark <- orderedList(
      list("bench_weight_0" = calc_bench_weights(
        .settings@benchmark_weighting_scheme,
        data@weights
      )),
      n = 1,
      order = c(0)
    )
    # Calculate Returns of Current Scores on Fwd Returns
    return_fwd_factor <- calc_return(weights[[0]], data@returns)
    # Calculate Returns of Lagged Scores on t+1 Returns
    return_lag_factor <- calc_return(weights, data@returns[[1]])
    # Calculate Returns of Benchmark on Fwd Returns
    bmark_fwd_return <- calc_return(weights_bmark[[0]], data@returns)
    # Calculate Alpha
    calc_alpha <- function(x, y) {
      orderedList(
        mapply(
          \(x, y, n) setNames(x - y, str_replace(n, "return", "alpha")),
          x@list, y, names(x@list),
          USE.NAMES = FALSE,
          SIMPLIFY = FALSE
        ),
        x@n,
        x@order
      )
    }
    alpha_fwd_return <- calc_alpha(
      return_fwd_factor,
      bmark_fwd_return@list
    )

    alpha_lag_factor <- calc_alpha(
      return_fwd_factor,
      bmark_fwd_return@list[[1]]
    )

    q_avg_fwd_return <- calc_q_returns(
      fq[[0]],
      data@returns,
      mean,
      na.rm = TRUE
    )

    q_lag_factor_avg_return <- calc_q_returns(
      fq,
      data@returns[[1]],
      mean,
      na.rm = TRUE
    )

    new("single_period_at_q",
      date = d,
      return_fwd_factor = return_fwd_factor,
      return_lag_factor = return_lag_factor,
      return_bmark = bmark_fwd_return,
      weights = weights,
      weights_bmark = weights_bmark,
      alpha_fwd_return = alpha_fwd_return,
      alpha_lag_factor = alpha_lag_factor,
      factor_quantile = fq,
      q_avg_fwd_return = q_avg_fwd_return,
      q_lag_factor_avg_return = q_lag_factor_avg_return
    )
  }
)

#' @include class-at_data.R
#' @include func-pivot_lospat_slot.R
#' @include class-multi_period_at.R
#' @include gen-alpha_test.R
setMethod("alpha_test",
  signature(data = "at_data"),
  function(data, .settings, ...) {
    spats <- lapply(data@alpha_data, alpha_test, .settings = .settings) #nolint
    dates <- as.Date(sapply(spats, \(x) x@date), origin = "1970-01-01")
    return_fwd_factor <- pivot_lospat_slot(spats, "return_fwd_factor", dates)
    return_lag_factor <- pivot_lospat_slot(spats, "return_lag_factor", dates)
    return_bmark <- pivot_lospat_slot(spats, "return_bmark", dates)
    weights <- pivot_lospat_slot(spats, "weights", dates, simplify = FALSE)
    weights_bmark <- pivot_lospat_slot(spats, "weights_bmark", dates, simplify = FALSE) # nolint
    alpha_fwd_return <- pivot_lospat_slot(spats, "alpha_fwd_return", dates)
    alpha_lag_factor <- pivot_lospat_slot(spats, "alpha_lag_factor", dates)

    list(
      "spats" = spats,
      "dates" = dates,
      "at" = new(
        "multi_period_at",
        return_fwd_factor = return_fwd_factor,
        return_lag_factor = return_lag_factor,
        return_bmark = return_bmark,
        weights = weights,
        weights_bmark = weights_bmark,
        alpha_fwd_return = alpha_fwd_return,
        alpha_lag_factor = alpha_lag_factor
      )
    )
  }
)


#' @include class-at_data.R
#' @include class-at_settings_factor_z.R
#' @include func-pivot_lospat_slot.R
#' @include class-multi_period_at_z.R
#' @include gen-alpha_test.R
setMethod("alpha_test",
  signature(
    data = "at_data",
    .settings = "at_settings_factor_z"
  ),
  function(data, .settings, ...) {
    at <- callNextMethod()

    IC_fwd_return <- pivot_lospat_slot(at$spats, "IC_fwd_return", at$dates)
    IC_lag_factor <- pivot_lospat_slot(at$spats, "IC_lag_factor", at$dates)

    new("multi_period_at_z",
      return_fwd_factor = at$at@return_fwd_factor,
      return_lag_factor = at$at@return_lag_factor,
      return_bmark = at$at@return_bmark,
      weights = at$at@weights,
      weights_bmark = at$at@weights_bmark,
      alpha_fwd_return = at$at@alpha_fwd_return,
      alpha_lag_factor = at$at@alpha_lag_factor,
      IC_fwd_return = IC_fwd_return,
      IC_lag_factor = IC_lag_factor
    )
  }
)

#' @include class-at_data.R
#' @include class-at_settings_factor_q.R
#' @include func-pivot_lospat_slot.R
#' @include class-multi_period_at_q.R
#' @include gen-alpha_test.R
setMethod("alpha_test",
  signature(
    data = "at_data",
    .settings = "at_settings_factor_q"
  ),
  function(data, .settings, ...) {
    at <- callNextMethod()

    factor_quantile <- pivot_lospat_slot(at$spats, "factor_quantile", at$dates)
    q_avg_fwd_returns <- pivot_lospat_slot(
      at$spat,
      "q_avg_fwd_return",
      at$dates
    )
    q_lag_factor_avg_return <- pivot_lospat_slot(
      at$spat,
      "q_lag_factor_avg_return",
      at$dates
    )

    new("multi_period_at_q",
      return_fwd_factor = at$at@return_fwd_factor,
      return_lag_factor = at$at@return_lag_factor,
      return_bmark = at$at@return_bmark,
      weights = at$at@weights,
      weights_bmark = at$at@weights_bmark,
      alpha_fwd_return = at$at@alpha_fwd_return,
      alpha_lag_factor = at$at@alpha_lag_factor,
      factor_quantile = factor_quantile,
      q_avg_fwd_returns = q_avg_fwd_returns,
      q_lag_factor_avg_return = q_lag_factor_avg_return
    )
  }
)