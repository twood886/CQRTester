# ATSettings_FactorWeighted (S4 Object) ------------------------------------
#' @include ATSettings.R
#' @title Settings for Factor Weighted Alpha Testing
#' @slot weight.scheme description
#' @slot win.prob A numeric vector of length 2 representing the percentile
#'cut-offs for windsorization.
#' @slot longonly A logical representing if portfolio weights should be long only
#' @slot leverage description
setClass(
  "ATSettings_FactorWeighted",
  contains = "ATSettings",
  slots = c(
    weight.scheme = "character",
    win.prob = "numeric",
    longonly = "logical",
    leverage = "numeric"))

#' @title Factor Weighted Alpha Testing Settings
#' @description
#' A function to create Alpha Testing settings for a factor weighted Alpha Test.
#' 
#' @param win.prob A numeric vector of length 2 representing the percentile
#' cut-offs for windsorization
#' @param longonly A logical representing whether or not the portfolio weights
#' should be long only (True) or Long-Short (False)
#' @returns An ATSettings_FactorWeighted S4 object to be used in Alpha Testing.
#' @export
AT_FactorWeighted <- function(Start.Date = as.Date("1901-01-01"), 
  End.Date = Sys.Date(), weighting.scheme = "factorz", win.prob = c(0,1), 
  longonly = F, leverage = 1){
  
  new("ATSettings_FactorWeighted",
      Start.Date = Start.Date,
      End.Date = End.Date,
      Weighting.Scheme = "Factor",
      weight.scheme = weighting.scheme,
      win.prob = win.prob,
      longonly = longonly,
      leverage= leverage)
}
