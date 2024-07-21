# ATSettings (S4 Object) --------------------------------------------------
#' @title Alpha Testing Settings
#' @description
#' An S4 Object containing the settings to be used in Alpha Testing. Arguments
#' passed through ATSettings are used in designing the alpha testing procedure.
#' 
#' @slot start.date Date representing the earliest date for Alpha Testing.
#' @slot end.date Date representing the latest date for Alpha Testing.
#' @slot testing.scheme Character representing alpha testing procedure.
setClass(
  "ATSettings",
  representation(
    start.date = "Date",
    end.date = "Date",
    testing.scheme = "character"))


# ATSettings_FactorWeighted (S4 Object) ------------------------------------
#' @title Settings for Factor Weighted Alpha Testing
#' @slot win.prob A numeric vector of length 2 representing the percentile
#'cut-offs for windsorization.
#' @slot longonly A logical representing if portfolio weights should be long only.
#' @slot leverage description
setClass(
  "ATSettings_FactorWeighted",
  contains = "ATSettings",
  slots = c(win.prob = "numeric"))

#' @title Factor Weighted Alpha Testing Settings
#' @description
#' A function to create Alpha Testing settings for a factor weighted Alpha Test.
#' 
#' @param start.date description
#' @param end.date description
#' @param win.prob A numeric vector of length 2 representing the percentile
#' cut-offs for windsorization.
#' @param longonly A logical representing whether or not the portfolio weights
#' should be long only (True) or Long-Short (False)
#' @param leverage description
#' @returns An ATSettings_FactorWeighted S4 object to be used in Alpha Testing.
AT_SettingWeighted <- function(
    start.date = as.Date("1901-01-01"), end.date = Sys.Date(), 
    win.prob = c(0,1))
{
  new("ATSettings_FactorWeighted",
      start.date = start.date,
      end.date = end.date,
      testing.scheme = "Factor",
      win.prob = win.prob)
}


# ATSettings_QSpread (S4 Object) -----------------------------------------
#' @title Settings for Quantile Spread Alpha Testing
#' @slot quantiles Number of Quantiles to use.
#' @slot longonly A logical representing if portfolio weights should be long only.
#' @slot leverage A numeric representing the leverage.
setClass(
  "ATSettings_QSpread",
  contains = "ATSettings",
  slots = c(quantiles = "numeric"))

#' @title Quantile Spread Alpha Testing Settings
#' @description
#' A function to create Alpha Testing settings for a Quantile Spread Alpha Test.
#' 
#' @param start.date description
#' @param end.date description
#' @param quantiles description
#' @param longonly A logical representing whether or not the portfolio weights
#' should be long only (True) or Long-Short (False)
#' @param leverage description
#' @returns An ATSettings_FactorWeighted S4 object to be used in Alpha Testing.
AT_SettingQSpread <- function(
    start.date = as.Date("1901-01-01"), end.date = Sys.Date(), quantiles = 5)
{
  new("ATSettings_QSpread",
      start.date = start.date,
      end.date = end.date,
      testing.scheme = "QSpread",
      quantiles = quantiles)
}


# ATSettings Function -----------------------------------------------------
#' @title Create Alpha Testing Settings
#' @description
#' This function creates an "ATSettings" class with arguments to be used when
#' testing factors
#' @param start.date A date object representing the earliest start date to be
#' used when performing alpha testing. Factor data is filtered to start at or
#' after provided Start.Date argument.
#' @param end.date A date object representing the last date to be
#' used when performing alpha testing. Factor data is filtered to end at or
#' before provided End.Date argument.
#' @param testing.scheme Weighting scheme to be used in testing factor. 
#' Currently supports "Factor" and "Quantile"
#' @param ... Additional settings to be passed based on Weighting.Scheme
#' @export
ATSettings <- function(start.date = as.Date("1901-01-01"),
  end.date = Sys.Date(), testing.scheme = "Factor", ...)
{
  
  known.schemes <- c("Factor","QSpread")
  
  if(!testing.scheme %in% known.schemes){
    stop("Testing Scheme must be one of \"Factor\" or \"QSpread\".")
  }
  
  # Use if then statement to create a Factor Weighted AT Settings or Quantile 
  if(testing.scheme == "Factor"){
    object <- AT_SettingWeighted(start.date, end.date, ...)
  }
  
  if(testing.scheme == "QSpread"){
    object <- AT_SettingQSpread(start.date, end.date, ...)
  }
  
  object
}



# Show Method -------------------------------------------------------------
setMethod(f = 'show',
  signature(object = "ATSettings_FactorWeighted"),
  function(object){
    out <- paste(
      paste("----- Alpha Testing Settings -----"),
      paste("Testing Scheme:", object@testing.scheme, sep = "\t\t"),
      paste("Start Date:", format(object@start.date,"%b %d,%Y"), sep = "\t\t"),
      paste("End Date:", format(object@end.date, "%b %d,%Y"), sep = "\t\t"),
      paste(
        "Windsorization:",
        paste(
            paste0(format(object@win.prob[1] * 100, digits = 2),"%"),
            paste0(format(object@win.prob[2] * 100, digits = 2),"%"),
            sep = " - "),
        sep = "\t\t"),
      sep = "\n")
    cat(out)
  })

setMethod(f = 'show',
  signature(object = "ATSettings_QSpread"),
  function(object){
    out <- paste(
      paste("----- Alpha Testing Settings -----"),
      paste("Testing Scheme:", object@testing.scheme, sep = "\t\t"),
      paste("Start Date:", format(object@start.date,"%b %d,%Y"), sep = "\t\t"),
      paste("End Date:", format(object@end.date, "%b %d,%Y"), sep = "\t\t"),
      paste("Factor Quantiles:", object@quantiles, sep = "\t"),
      sep = "\n")
    cat(out)
  })