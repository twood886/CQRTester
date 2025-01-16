#' @title Settings for Factor Zscore Weighted Alpha Testing (S4 Object)
#' @slot win_prob A numeric vector of length 2 representing the percentile
#'cut-offs for windsorization.
#' @include class-at_settings.R
setClass(
  "at_settings_factor_z",
  contains = "at_settings",
  representation(win_prob = "numeric")
)