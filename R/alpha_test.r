#' @export
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