# SinglePeriodAT (S4 Object) ----------------------------------------------
#'  An S4 Class to represent Factor Alpha Testing for a single period
#'
#' @slot .factordata A SinglePeriodFactorData object.
#' @slot date A date object representing the date of the data
#' @slot factorZscore A named numeric vector representing the factor Z-Score
#' @slot factorQuantile A named numeric vector representing the factor Quantile
#' @slot returnZscore A named numeric vector representing the forward
#'  return Z-Score
#' @slot IC A numeric value representing the Information Coefficient
#' @slot uStats Universe Return Statistics
#' @slot qStats Quantile Return Statistics
#' @slot .settings Alpha Testing settings
#' @include SinglePeriodFactorData.R
setClass(
  "SinglePeriodAT",
  representation(
    .factordata = "SinglePeriodFactorData",
    date = "Date",
    factorZscore = "numeric",
    factorQuantile = "ordered",
    returnZscore = "numeric",
    IC = "numeric",
    uStats = "list",
    qStats = "list",
    .settings = "list"))


# SinglePeriodAT ----------------------------------------------------------
#'@title Single Period Factor Alpha Testing
#'@description Function to create a Single Period Alpha Testing Object
#'@param .data A SinglePeriodFactorData
#'@param fftile A numeric value representing the number of quantiles to group
#'the factor data by.
#'@param win.prob A numeric vector of length 2 representing the percentile
#'cut-offs for windsorization.
#'@returns A SinglePeriodFactorData object.
#'@export
AlphaTest <- function(.Object, fftile = 5, win.prob = c(0,1), ...) UseMethod("AlphaTest")

# -------------------------------------------------------------------------
#' @include Utilities.R
#' @include Utilities_Scoring.R
setMethod('AlphaTest',
  signature(.Object = 'SinglePeriodFactorData'),
  function(.Object, fftile, win.prob, ...){
    
    # Extract Date
    d <- .Object@date
    
    # Alpha Testing Settings
    .settings <- list(
      "fftile" = fftile,
      "win.prob" = win.prob)
    
    # Calculate the Z-Score of Factors
    fz <- ctz(.Object@fvals, win.prob)
    
    # Calculate the Z-Score of Returns
    rz <- ctz(.Object@returns, win.prob)
    
    # Calculate the Quantile of Factors
    fq<-ctq(.Object@fvals, fftile)
    
    # Calculate the IC
    IC<-cor(fz, rz, use = "pairwise.complete.obs")
    
    # Universe Level Statistics
    uStats <- UniverseReturnStats(.Object)
    
    # Quintile Level Statistics
    qStats <- QuantileReturnStats(.Object, fftile=fftile)
    
    return(
      new("SinglePeriodAT",
        .factordata = .Object,
        date = d,
        factorZscore = fz,
        factorQuantile = fq,
        returnZscore = rz,
        IC = IC,
        uStats = uStats,
        qStats = qStats,
        .settings = .settings
    ))
  })