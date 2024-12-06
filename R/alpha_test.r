#' @export
#' @docType methods
setGeneric("alpha_test",
  function(data, .settings, ...) standardGeneric("alpha_test")
)

#' @title Alpha Test
#' @description The `alpha_test()` function is used to run a backtest on
#'  `at_data` or `single_period_at_data` objects according to the
#'  backtest parameters in set in an `at_data_params` object
#' @param data A `factor_data` or `single_period_factor_data` object.
#'  See Methods, below, for more details.
#' @param .settings A `factor_data_params` object.
#' @return An `alpha_test` or `single_period_at` object.
#'  See Methods, below, for more details.
#' @details The `alpha_test()` function is used to calculate backtest
#'  statistics regarding the effecacy of a factor to produce excess returns.
#'  The statistics returned depend on the object passed as `data` and the
#'  object passed as `.settings`. `alpha_test()` is designed to provide
#'  statistics about future cumulative performance for the current factor value
#'  as well as 1 period forward performance of the lagged factor value.
#'  These statistics are returned in `orderedList` objects.
#' @section Methods:
#' This function is a **generic** which means the function will return different
#'  statistics in different objects depending on the class of `data` and
#'  `.settings` provided.
#' @export
alpha_test <- function(data, .settings, ...) UseMethod("alpha_test")

# alpha_test (S4 Object) ---------------------------------------------------
#' @title Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing
#' @slot returns A numeric array of returns.
#' @slot return_bmark A numeric array of benchmark returns.
#' @slot weights A list of weights.
#' @slot weights_bmark A list of benchmark weights.
#' @include orderedList.R
setClass(
  "at",
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

# alpha_test_z (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Z-Score) S4 Object
#' @slot IC Information Coefficient
#' @slot factor_z_score A list of factor z-scores.
#' @include alpha_test.R
#' @include at_data.R
setClass(
  "alpha_test_z",
  contains = "at",
  representation(
    IC_fwd_return = "orderedList",
    IC_lag_factor = "orderedList",
  )
)

# alpha_test_q (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Quantile) S4 Object
#' @slot factor_quantile A list of factor quantiles.
#' @slot q_returns A list of factor quantile returns.
#' @slot q_stats A list of factor quantile stats.
#' @include alpha_test.R
#' @include at_data.R
setClass(
  "alpha_test_q",
  contains = "at",
  representation(
    factor_quantile = "orderedList",
    q_avg_fwd_returns = "list",
    q_lag_factor_avg_return = "list"
  )
)



#' @include alpha_test_single_period.R
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
        "at",
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




#' @include alpha_test_single_period.R
#' @include orderedList.R
setMethod("alpha_test",
  signature(
    data = "at_data",
    .settings = "at_settings_factor_z"
  ),
  function(data, .settings, ...) {
    at <- callNextMethod()

    IC_fwd_return <- pivot_lospat_slot(at$spats, "IC_fwd_return", at$dates)
    IC_lag_factor <- pivot_lospat_slot(at$spats, "IC_lag_factor", at$dates)

    new("alpha_test_z",
      return_fwd_factor = test$at@return_fwd_factor,
      return_lag_factor = test$at@return_lag_factor,
      return_bmark = test$at@return_bmark,
      weights = test$at@weights,
      weights_bmark = test$at@weights_bmark,
      alpha_fwd_return = test$at@alpha_fwd_return,
      alpha_lag_factor = test$at@alpha_lag_factor,
      IC_fwd_return = IC_fwd_return,
      IC_lag_factor = IC_lag_factor
    )
  }
)

#' @include alpha_test_single_period.R
#' @include orderedList.R
setMethod("alpha_test",
  signature(
    data = "at_data",
    .settings = "at_settings_factor_q"
  ),
  function(data, .settings, ...) {
    at <- callNextMethod()

    factor_quantile <- pivot_lospat_slot(at$spats, \(x) x@factor_quantile)

    IC_fwd_return <- pivot_lospat_slot(at$spats, "IC_fwd_return", at$dates)
    IC_lag_factor <- pivot_lospat_slot(at$spats, "IC_lag_factor", at$dates)

    new("alpha_test_z",
      return_fwd_factor = test$at@return_fwd_factor,
      return_lag_factor = test$at@return_lag_factor,
      return_bmark = test$at@return_bmark,
      weights = test$at@weights,
      weights_bmark = test$at@weights_bmark,
      alpha_fwd_return = test$at@alpha_fwd_return,
      alpha_lag_factor = test$at@alpha_lag_factor,
      IC_fwd_return = IC_fwd_return,
      IC_lag_factor = IC_lag_factor
    )
  }
)