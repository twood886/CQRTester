# -------------------------------------------------------------------------
#'  An S4 Class to represent Factor Alpha Testing for a single period
#'
#'  @slot .factordata A SinglePeriodFactorData object.
#'  @slot date A date object representing the date of the data
#'  @slot factorZscore A named numeric vector representing the factor Z-Score
#'  @slot factorQuantile A named numeric vector representing the factor Quantile
#'  @slot returnZscore A named numeric vector representing the forward return Z-Score
#'  @slot IC A numeric value representing the Information Coefficient
setClass("SinglePeriodAT",
  representation(
    .factordata = "SinglePeriodFactorData",
    date = "Date",
    factorZscore = "numeric",
    factorQuantile = "ordered",
    returnZscore = "numeric",
    IC = "numeric"))


# -------------------------------------------------------------------------
#'  @title Single Period Factor Alpha Testing
#'  @description Function to create a Single Period Alpha Testing Object
#'  @param .data A SinglePeriodFactorData
#'  @param fftile A numeric value representing the number of quantiles to group the factor data by.
#'  @param win.prob A numeric vector of length 2 representing the percentile cut-offs for windsorization.
#'  @returns A SinglePeriodFactorData object.
#'  @keywords internal
#'  @export
SinglePeriodAT <- function(.data, fftile = 5, win.prob = c(0,1), ...){

  # If provided data is not a SinglePeriodFactorData object, convert
  if(!is(.data, "SinglePeriodFactorData"))
    .data <- SinglePeriodFactorData(data = .data, ...)

  d<-.data@date

  # Calculate the Z-Score of Factors
  fz<-ctz(.data@fvals, win.prob)

  # Calculate the Z-Score of Returns
  rz<-ctz(.data@returns, win.prob)

  # Calculate the Quantile of Factors
  fq<-ctq(.data@fvals, fftile)

  # Calculate the IC
  IC<-cor(fz, rz, use = "pairwise.complete.obs")

  return(new("SPAT",
    .factordata = .data,
    date = d,
    factorZscore = fz,
    factorQuantile = fq,
    returnZscore = rz,
    IC = IC
  ))
}

# -------------------------------------------------------------------------
is.SinglePeriodAT <- function(x) is(x, "SPAT")
