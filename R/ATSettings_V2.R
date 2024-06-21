# AlphaTestSettings (S4 Object) -------------------------------------------
#' @title Alpha Testing Settings (S4 Class)
#' @description
#' An S4 Object containing settings to be used in Alpha Testing.
#' @slot start.date Date object representing the 
#' @slot win.prob Windsorizing probability range
#' @slot quantiles Number of quantiles for quantile analysis
#' @slot q_w_scheme Weighting scheme for quantiles
#' @slot w_scheme Weighting scheme for testing
setClass(
  "ATSettings",
  representation(
    start.date = "Date",
    end.date = "Date",
    w_scheme = "character",
    win.prob = "numeric",
    quantiles = "numeric",
    q_w_scheme = "character",
    long_only = "logical",
    leverage = "numeric"))


# ATSettings Function -----------------------------------------------------
#' @title Create Alpha Testing Settings
#' @description
#' This function creates a "ATSettings" class with arguements to be used when
#' testing alpha factors.
#' @param start.date A date object representing the start date to be used when 
#' alpha testing. Factor data is filtered to start at or after provided 
#' start.date arguement.
#' @param end.date A date object representing the end date to be used when 
#' alpha testing. Factor data is filtered to end before or at the provided
#' end.date arguement.
#' @param win.prob A numeric vector of length 2 representing the percentile 
#' cut-offs for windsorization.
#' @param quantiles A numeric value representing the number of quantiles to group
#' the factor data by.
#' @param q_w_scheme A string representing the quantile weighting scheme to
#' employ. Default value is "EW" (equal weighted).
#' @param w_scheme A string representing the weighting scheme for building 
#' portfolio based on factor scores. Default value is "valuez".
#' @param long_only A boolean representing if the portfolio weights should be
#' long only. Default value is FALSE
#' @param leverage A numeric representing the leverage of the portfolio.
#' Default is set to 1, representing sum 
ATSettings <- function(start.date = as.Date("1901-01-01"), 
  end.date = Sys.Date(), win.prob = c(0,1), quantiles = 5,
  q_w_scheme = "EW", w_scheme = "valuez", long_only = F, leverage = 1)
{
  new("ATSettings",
    start.date = start.date,
    end.date = end.date,
    win.prob = win.prob,
    quantiles = quantiles,
    q_w_scheme = q_w_scheme,
    w_scheme = w_scheme,
    long_only = long_only,
    leverage = leverage)
  }



# ATSettings Methods ------------------------------------------------------
## show -------------------------------------------------------------------
setMethod(f = 'show',
  signature(object = "ATSettings"),
  function(object){
    out <- paste(
      paste("----- Alpha Testing Settings -----"),
      paste("Start Date:", format(object@start.date,"%b %d,%Y"), sep = "\t\t"),
      paste("End Date:", format(object@end.date, "%b %d,%Y"), sep = "\t\t"),
      paste(
        "Windsorization:", 
        paste(
            paste0(format(object@win.prob[1] * 100, digits = 2),"%"),
            paste0(format(object@win.prob[2] * 100, digits = 2),"%"),
            sep = " - "),
        sep = "\t\t"),
      paste("Factor Quantiles:", object@quantiles, sep = "\t"),
      paste("Quantile Weighting:", object@q_w_scheme, sep = "\t"),
      paste("Weighting Scheme:", object@w_scheme, sep = "\t"),
      paste("Long Only:", object@long_only, sep = "\t\t"),
      paste("Leverage Factor:", object@leverage, sep = "\t"),
      sep = "\n")
    cat(out)
  })


setGeneric("setAT", function(x, ...) standardGeneric("setAT"))
setMethod("setAT",
  signature(x = "ATSettings"),
  function(x, ...){
    dargs <- list(...)
    for(iarg in 1:length(dargs)){
      s <- names(dargs)[[iarg]]
      v <- dargs[[iarg]]
      slot(x, s) <- v
    }
    x
  })
setATsettings <- function(x, ...) UseMethod("setAT")




