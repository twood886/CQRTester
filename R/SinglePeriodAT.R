# SinglePeriodAT (S4 Object) ----------------------------------------------
#'  An S4 Class to represent Factor Alpha Testing for a single period
#'
#'  @slot .factordata A SinglePeriodFactorData object.
#'  @slot date A date object representing the date of the data
#'  @slot factorZscore A named numeric vector representing the factor Z-Score
#'  @slot factorQuantile A named numeric vector representing the factor Quantile
#'  @slot returnZscore A named numeric vector representing the forward
#'  return Z-Score
#'  @slot IC A numeric value representing the Information Coefficient
#'  @slot .settings
setClass(
  "SinglePeriodAT",
  representation(
    .factordata = "SinglePeriodFactorData",
    date = "Date",
    factorZscore = "numeric",
    factorQuantile = "ordered",
    returnZscore = "numeric",
    IC = "numeric",
    .settings = "list"))


# SinglePeriodAT ----------------------------------------------------------
#'  @title Single Period Factor Alpha Testing
#'  @description Function to create a Single Period Alpha Testing Object
#'  @param .data A SinglePeriodFactorData
#'  @param fftile A numeric value representing the number of quantiles to group
#'  the factor data by.
#'  @param win.prob A numeric vector of length 2 representing the percentile
#'  cut-offs for windsorization.
#'  @returns A SinglePeriodFactorData object.
#'  @keywords internal
#'  @export
AlphaTest <- function(.Object) UseMethod("AlphaTest")

# -------------------------------------------------------------------------
is.SinglePeriodAT <- function(x) is(x, "SPAT")

# settings  ---------------------------------------------------------------
# setGeneric("settings", function(.Object) standardGeneric("settings"))
# setGeneric("settings<-", function(.Object, ...) standardGeneric("settings<-"))
# setMethod("settings",
#   signature(.Object = "SinglePeriodAT"),
#   function(.Object) .Object@.settings)
# setMethod("settings<-",
#   signature(.Object = "SinglePeriodAT"),
#   function(.Object,...){
#     dargs <- list(...)
#
#     if("ffile" %in% names(dargs))
#       fftile = dargs$fftile
#
#     if("win.prob" %in% names(dargs))
#       win.prob = dargs$win.prob
#
#     .settings <- list(
#       "ffitle" = fftile,
#       "win.prob" = win.prob)})


