#' @title Settings for Factor Quantile Weighted Alpha Testing (S4 Object)
#' @slot quantiles Number of Quantiles to use.
#' @include class-at_settings.R
setClass(
  "at_settings_factor_q",
  contains = "at_settings",
  representation(quantiles = "numeric")
)