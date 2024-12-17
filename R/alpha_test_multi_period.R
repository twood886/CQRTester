# multi_peirod_at(S4 Object) ---------------------------------------------------
#' @title Multi Period Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing Parent Calss
#'  for multiple periods.
#' @slot return_fwd_factor ToDo
#' @slot return_lag_factor ToDo
#' @slot return_bmark ToDo
#' @slot weights ToDo
#' @slot weights_bmark ToDo
#' @slot alpha_fwd_return ToDo
#' @slot alpha_lag_factor ToDo
#' @include orderedList.R
setClass(
  "multi_period_at",
  slots = c(
    return_fwd_factor = "orderedList",
    return_lag_factor = "orderedList",
    return_bmark = "orderedList",
    weights = "orderedList",
    weights_bmark = "orderedList",
    alpha_fwd_return = "orderedList",
    alpha_lag_factor = "orderedList"
  )
)

# multi_period_at_z (S4 Object) ------------------------------------------------
#' @title Multi Period Alpha Test (Factor Z-Score) S4 Object
#' @slot IC_fwd_return ToDo
#' @slot IC_lag_factor ToDo
#' @include orderedList.R
setClass(
  "multi_period_at_z",
  contains = "multi_period_at",
  representation(
    IC_fwd_return = "orderedList",
    IC_lag_factor = "orderedList"
  )
)

# multi_period_at_q (S4 Object) ------------------------------------------------
#' @title Multi Period Alpha Test (Factor Quantile) S4 Object
#' @slot factor_quantile A list of factor quantiles.
#' @slot q_returns A list of factor quantile returns.
#' @slot q_stats A list of factor quantile stats.
#' @include orderedList.R
setClass(
  "multi_period_at_q",
  contains = "multi_period_at",
  representation(
    factor_quantile = "orderedList",
    q_avg_fwd_returns = "orderedList",
    q_lag_factor_avg_return = "orderedList"
  )
)

#' @include orderedList.R
#' @include alpha_test.R
#' @include alpha_test_single_period.R
#' @include at_data.R
setMethod("alpha_test",
  signature(data = "at_data"),
  function(data, .settings, ...) {
    spats <- lapply(data@alpha_data, alpha_test, .settings = .settings)
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

#' @include alpha_test.R
#' @include alpha_test_single_period.R
#' @include orderedList.R
#' @include at_data.R
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

#' @include alpha_test.R
#' @include alpha_test_single_period.R
#' @include orderedList.R
#' @include at_data.R
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
