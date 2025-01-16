#' @title Create Alpha Testing Settings
#' @description
#' This function creates an "at_settings" class with arguments to be used when
#' testing factors
#' @param start_date A date object representing the earliest start date to be
#' used when performing alpha testing. Factor data is filtered to start at or
#' after provided Start.Date argument.
#' @param end_date A date object representing the last date to be
#' used when performing alpha testing. Factor data is filtered to end at or
#' before provided End.Date argument.
#' @param testing_scheme Weighting scheme to be used in testing factor.
#' Currently supports "Factor" and "Quantile"
#' @param weighting_scheme desctiption
#' @param benchmark_weighting_scheme description
#' @param ... Additional settings to be passed based on Weighting.Scheme
#' @include func-set_at_settings_factor_z.R
#' @include func-set_at_settings_factor_q.R
#' @export
set_at_settings <- function(
    testing_scheme = "factor-z",
    start_date = as.Date("1901-01-01"), end_date = Sys.Date(),
    weighting_scheme = NULL, benchmark_weighting_scheme = "zero", ...) {
  known_schemes <- c("factor-z", "factor-q")

  if (!testing_scheme %in% known_schemes) {
    stop("Testing Scheme must be one of \"factor-z\" or \"factor-q\".")
  }

  if (testing_scheme == "factor-z") {
    if (is.null(weighting_scheme)) {
      weighting_scheme <- "z-weighted"
    }
    at <- set_at_settings_factor_z(
      start_date, end_date, weighting_scheme, benchmark_weighting_scheme, ...
    )
  }

  if (testing_scheme == "factor-q") {
    if (is.null(weighting_scheme)) {
      weighting_scheme <- "equal-spread"
    }
    at <- set_at_settings_factor_q(
      start_date, end_date, weighting_scheme, benchmark_weighting_scheme, ...
    )
  }
  at
}