# SinglePeriodFactorData (S4 Object) --------------------------------------
#'  An S4 Class to represent Factor Data for a single period
#'
#'@slot factor A character representing the name of the factor
#'@slot date A date object representing the date of the data
#'@slot ids A character vector representing the company ids
#'@slot fvales A named numeric vector representing the factor values
#'@slot returns A names numeric vector representing the forward returns
#'@export
setClass(
  "SinglePeriodFactorData",
  representation(
    factor = "character",
    date = "Date",
    ids = "character",
    fvals = "numeric",
    returns = "numeric"))

# SinglePeriodFactorData --------------------------------------------------
#'@title Single Period Factor Data
#'@description Function to create SinglePeriodFactorData object
#'@details This function converts data into SinglePeriodFactorData object.
#'@param data A data frame containing id columns, return column and factor
#' value column.
#'@param date A date object representing the date of the data.
#'@param iname A character representing the column name of identifiers.
#'@param fname A character representing the column name of the factor.
#'@param rname A character representing the column name of the returns.
#'@returns A SinglePeriodFactorData object.
#'@import methods
#'@keywords internal
#'@export
SinglePeriodFactorData <- function(data, date, iname, fname, rname, ...){

  dargs <- list(...)

  if("date" %in% names(dargs))
    date = dargs$date
  if("iname" %in% names(dargs))
    iname = dargs$iname
  if("rname" %in% names(dargs))
    rname = dargs$rname
  if("fname" %in% names(dargs))
    fname = dargs$fname

  # Check if necessary inputs are included
  if(missing(data))
    stop("There is no data provided")

  if(missing(date))
    stop("No date arguement provided")

  if(missing(iname))
    stop("No identifier column name provided")

  if(missing(fname))
    stop("No factor column name provided")

  if(missing(rname)){
    returns <- rep(NA_real_, nrow(data))
  }else{
    returns = data[[rname]]
  }


  ids = data[[iname]]
  fvals = data[[fname]]
  names(fvals) = ids
  names(returns) = ids

  return( new("SinglePeriodFactorData",
              factor = fname,
              date = date,
              ids = ids,
              fvals = fvals,
              returns = returns
  ))
}


# -------------------------------------------------------------------------
is.SinglePeriodFactorData <- function(x) is(x, "SinglePeriodFactorData")


# -------------------------------------------------------------------------
as.SinglePeriodFactorData <- function(x, ...) UseMethod("as.SinglePeriodFactorData")

as.SinglePeriodFactorData.SinglePeriodsFactorData <- function(x,...){
  ## true SinglePeriodFactorData
  if(class(x)=="SinglePeriodFactorData")
    x
  else
    as(x, "SinglePeriodFactorData")
}


# UniverseReturnStats -----------------------------------------------------
setMethod("UniverseReturnStats",
  signature(.data = "SinglePeriodFactorData"),
  function(.data){

    n = length(.data@ids)
    n_avail = length(which(!is.na(.data@returns)))
    avg_ret = mean(.data@returns, na.rm = T)
    med_ret = median(.data@returns, na.rm = T)
    hit_rate_zero = length(which(.data@returns > 0)) / n_avail
    hit_rate_uavg = length(which(.data@returns>avg_ret)) /n_avail

    u_stats = list(
      "n" = n,
      "n_avail" = n_avail,
      "avg_return" = avg_ret,
      "med_return" = med_ret,
      "hit_rate_zero" = hit_rate_zero,
      "hit_rate_uavg" = hit_rate_uavg
    )
    return(u_stats)
  })


# -------------------------------------------------------------------------

QuantileReturnStats <- function(.Object, fftile = 5, ...) UseMethod("QuantileReturnStats")
setMethod("QuantileReturnStats",
  signature(.Object = "SinglePeriodFactorData"),
  function(.Object, fftile){
    
    # Calculate the Quantile of Factors
    fq<-ctq(.Object@fvals, fftile)
    
    .SingleGroupReturnStats <- function(.Object, fq, ftile){
      group_loc <- which(fq==ftile)
      group_ids <-.Object@ids[group_loc]
      group_returns <- .Object@returns[group_loc]
      
      group_n <- length(group_ids)
      group_n_avail <- length(which(!is.na(group_returns)))
      group_weights <- rep(1/group_n, group_n)
      names(group_weights) <- group_ids
      
      out <- list(
        "n" = group_n,
        "n_avail" = group_n_avail,
        "avg_return" = mean(group_returns, na.rm = T),
        "med_return" = median(group_returns, na.rm = T),
        "hit_rate_zero" = length(which(group_returns > 0)) / group_n_avail,
        "weights" = group_weights)
      return(out)
    }
    
    quantileStats <- lapply(
      levels(fq), 
      .SingleGroupReturnStats, 
      .Object = .Object, 
      fq = fq)
    names(quantileStats) <- levels(fq)
    
    qn <- length(levels(fq)[which(levels(fq) != "NA")])
    qspread <- quantileStats[[1]]$avg_return - quantileStats[[qn]]$avg_return
    qspreadweights <- c(quantileStats[[1]]$weights, quantileStats[[qn]]$weights)
    
    list(
      "q_spread" = qspread,
      "q_spread.weights" = qspreadweights,
      "q_stats" = quantileStats)
})

