# FactorData (S4 Object) --------------------------------------------------
#'  An S4 Class to represent Factor Data over time
#'
#'  @slot data A list of SinglePeriodFactorData objects.
#'  @include SinglePeriodFactorData.R
setClass(
  "FactorData",
  representation(
    data = "list"))



# FactorData --------------------------------------------------------------
#' @title Create Factor Data
#' @description
#' Function to create factor data from data frame.
#' @details
#' Details
#' @param data A data frame containing, id column, data column, factor column, return column.
#' @param dname A character representing the column name of the dates.
#' @param iname A character representing the column name of the identifiers.
#' @param fname A character representing the column name of the factor.
#' @param rname A character representing the column name of the returns.
#' @returns A FactorData object.
#' @import dplyr
#' @import methods
#' @include Utilities.R
#' @export
FactorData <- function(data, dname, iname, fname, rname){

  # Split Data by Data Column
  ldata <- data %>% named_group_split(!!sym(dname))

  # Convert split data into SinglePeriodFactorData
  fdata <- lapply(
    ldata,
    SinglePeriodFactorData,
    date = as.Date(names(ldata)),
    iname = iname,
    fname = fname,
    rname = rname)

  return(new("FactorData", data = fdata))
}

#' @export
FactorAvail <- function(FactorData, ...) UseMethod("FactorAvail")
setMethod("FactorAvail",
  signature(FactorData = "FactorData"),
  function(FactorData, ...){
    
    .avail <- function(.data){
      fvals = .data@fvals
      length(which(!is.na(fvals)))/length(fvals)}
    
    unlist(lapply(FactorData@data, .avail))
    })