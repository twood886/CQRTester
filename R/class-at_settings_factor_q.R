#' @title Settings for Factor Quantile Weighted Alpha Testing (S4 Object)
#' @description
#' An S4 class for specifying settings used in quantile-based alpha testing.
#'
#' @slot quantiles A numeric value representing the number of quantiles to use.
#' @include class-at_settings.R
setClass(
  "at_settings_factor_q",
  contains = "at_settings",
  representation(quantiles = "numeric")
)