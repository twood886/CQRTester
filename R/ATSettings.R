# ATSettings (S4 Object) --------------------------------------------------
#' @title Alpha Testing Settings
#' @slot WeightingScheme Character
setClass(
  "ATSettings",
  representation(
    WeightingScheme = "character"))



# ATSettings_FacorWeighted (S4 Object) ------------------------------------
#' @title Settings for Factor Weighted Alpha Testing
#' @slot win.prob A numeric vector of length 2 representing the percentile
#'cut-offs for windsorization.
#' @slot longonly A logical representing if portfolio weights should be 
#' long only
setClass(
  "ATSettings_FactorWeighted",
  contains = "ATSettings",
  slots = c(
    win.prob = "numeric",
    longonly = "logical"))

# ATSettings_FactorWeighted -----------------------------------------------
#' @export
ATSettings_FactorWeighted <- function(win.prob = c(0,1), longonly = F ,...){
  new("ATSettings_FactorWeighted",
    WeightingScheme = "FactorWeighted",
    win.prob = win.prob,
    longonly = longonly)
  }



# ATSettings_Quantile (S4 Object) -----------------------------------------
#' @title Settings for Quantile Alpha Testing
#' @slot quantiles Number of Quantiles to use.
#' @slot longonly A logical representing if portfolio weights should be 
#' long only
setClass(
  "ATSettings_Quantile",
  contains = "ATSettings",
  slots = c(
    quantiles = "integer",
    longonly = "logical"))


# ATSettings_Quantile -----------------------------------------------------
ATSettings_Quantile <- function(quantiles = 5, longonly = F, ...){
  new("ATSettings_Quantile",
    WeightingScheme = "Quantile",
    quantiles = quantiles,
    longonly = longonly)
  }