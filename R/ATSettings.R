# ATSettings (S4 Object) --------------------------------------------------
#' @title Alpha Testing Settings
#' @description
#' An S4 Object containing the settings to be used in Alpha Testing
#' 
#' @slot Start.Date Date representing the earliest date for Alpha Testing
#' @slot End.Date Date representing the latest date for Alpha Testing
#' @slot Weighting.Scheme Character
setClass(
  "ATSettings",
  representation(
    Start.Date = "Date",
    End.Date = "Date",
    Weighting.Scheme = "character"))







# ATSettings Function -----------------------------------------------------
#' @include ATSettings_FactorWeighted.R
#' @title Create Alpha Testing Settings
#' @description
#' This function creates an "ATSettings" class with arguments to be used when
#' testing factors
#' @param Start.Date A date object representing the earliest start date to be
#' used when performing alpha testing. Factor data is filtered to start at or
#' after provided Start.Date argument.
#' @param End.Date A date object representing the last date to be
#' used when performing alpha testing. Factor data is filtered to end at or
#' before provided End.Date argument.
#' @param Weighting.Scheme Weighting scheme to be used in testing factor. 
#' Currently supports "Factor" and "Quantile"
#' @param ... Additional settings to be passed based on Weighting.Scheme
ATSettings <- function(Start.Date = as.Date("1901-01-01"),
  End.Date = Sys.Date(), Weighting.Scheme = "Factor", ...)
{
  
  # Use if then statement to create a Factor Weighted AT Settings or Quantile 
  if(Weighting.Scheme == "Factor"){
    return(AT_FactorWeighted(Start.Date, End.Date, ...))
    
  }
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
    quantiles = "integer"))


# ATSettings_Quantile -----------------------------------------------------
#' @export
AT_FactorQSpread <- function(quantiles = 5, ...){
  new("ATSettings_Quantile",
    WeightingScheme = "Quantile",
    quantiles = quantiles)}



#' setMethod(f = 'show',
#'   signature(object = "ATSettings"),
#'   function(object){
#'     out <- paste(
#'       paste("----- Alpha Testing Settings -----"),
#'       paste("Start Date:", format(object@start.date,"%b %d,%Y"), sep = "\t\t"),
#'       paste("End Date:", format(object@end.date, "%b %d,%Y"), sep = "\t\t"),
#'       paste(
#'         "Windsorization:", 
#'         paste(
#'             paste0(format(object@win.prob[1] * 100, digits = 2),"%"),
#'             paste0(format(object@win.prob[2] * 100, digits = 2),"%"),
#'             sep = " - "),
#'         sep = "\t\t"),
#'       paste("Factor Quantiles:", object@quantiles, sep = "\t"),
#'       paste("Quantile Weighting:", object@q_w_scheme, sep = "\t"),
#'       paste("Weighting Scheme:", object@w_scheme, sep = "\t"),
#'       paste("Long Only:", object@long_only, sep = "\t\t"),
#'       paste("Leverage Factor:", object@leverage, sep = "\t"),
#'       sep = "\n")
#'     cat(out)
#'   })