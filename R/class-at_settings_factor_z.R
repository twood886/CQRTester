#' @title Settings for Factor Z-Score Weighted Alpha Testing (S4 Object)
#' @description
#' An S4 class for specifying settings used in Z-score-based alpha testing.
#'
#' @slot win_prob A numeric vector of length 2 specifying the percentile
#'   cut-offs for winsorization.
#' @include class-at_settings.R
setClass(
  "at_settings_factor_z",
  contains = "at_settings",
  representation(win_prob = "numeric")
)
