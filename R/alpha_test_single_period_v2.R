# single_period_at(S4 Object) --------------------------------------------------
#' @title Single Period Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing Parent Class
#'  for a single period
#' @slot date A date object representing the date of the data
#' @slot return portfolio return
#' @slot return_bmark benchmark return
#' @slot weights Portfolio Weights
#' @slot weights_bmark benchmark weights
#' @include at_data_single_period.R
setClass(
  "single_period_at",
  slots = c(
    date = "Date",
    return_fwd_factor = "orderedList",
    return_lag_factor = "orderedList",
    return_bmark = "orderedList",
    weights = "orderedList",
    weights_bmark = "orderedList"
  )
)

# single_period_at_factor_z (S4 Object) ------------------------------------
#' @title Single Period Alpha Test (Factor Weighted) S4 Object
#' @slot IC Information Coefficient of Factor.
#' @slot factor_z_score factor_z_score object.
#' @slot return_z_score z-score of returns.
#' @include at_data_single_period.R
setClass(
  "single_period_at_factor_z",
  contains = "single_period_at",
  representation(
    IC_fwd_return = "orderedList",
    IC_lag_factor = "orderedList",
    alpha = "orderedList",
    factor_z_score = "orderedList",
    return_z_score = "orderedList"
  )
)

# single_period_at_q_spread (S4 Object) ------------------------------------
#' @title Single Period Alpha Test (Quantiles) S4 Object
#' @slot factor_quantile todo
#' @slot q_returns todo
#' @slot q_stats todo
#' @include at_data_single_period.R
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
#' @include return_z_score.R
#' @include calc_IC.R
#' @include calc_weights.R
#' @include calc_bench_weights.R
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
    alpha <- orderedList(
      mapply(
        \(x, y, n) setNames(x - y, str_replace(n, "return", "alpha")),
        score_fwd_return@list,
        bmark_fwd_return@list,
        names(score_fwd_return@list),
        USE.NAMES = FALSE,
        SIMPLIFY = FALSE
      ),
      score_fwd_return@n,
      score_fwd_return@order
    )

    new("single_period_at_factor_z",
      date = d,
      return_fwd_factor = score_fwd_return,
      return_lag_factor = lag_score_return,
      return_bmark = bmark_fwd_return,
      weights = weights,
      weights_bmark = weights_bmark,
      IC_fwd_return = ic_fwd_return,
      IC_lag_factor = ic_lag_factor,
      alpha = alpha,
      factor_z_score = fz,
      return_z_score = rz
    )
  }
)
