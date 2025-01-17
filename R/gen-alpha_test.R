#' @title Alpha Test Function
#' @description
#' The `alpha_test` function performs a backtest on alpha test data using the
#' specified settings, generating performance metrics for factors.
#'
#' @param data A `factor_data` or `single_period_factor_data` object containing
#'   the input data for the alpha test.
#' @param .settings A `factor_data_params` object containing configuration
#'   settings for the alpha test.
#' @return A `multi_period_at` or `single_period_at` object, depending on the
#'   input data.
#' @details
#' The function computes backtest statistics, evaluating the efficacy of
#' factors in predicting excess returns. Metrics include forward cumulative
#' performance for the current factor value and one-period forward performance
#' for lagged factor values.
#' @export
setGeneric("alpha_test",
  function(data, .settings, ...) standardGeneric("alpha_test")
)