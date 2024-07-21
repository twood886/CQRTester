# factor_data (S4 Object) --------------------------------------------------
#'  An S4 Class to represent Factor Data over time
#'
#' @slot data A list of SinglePeriodFactorData objects.
#' @include SinglePeriodFactorData.R
setClass(
  "factor_data",
  representation(
    data = "list"
  )
)


# FactorData --------------------------------------------------------------
#' @title Create Factor Data
#' @description
#' Function to create factor data from data frame.
#' @details
#' Details
#' @param data A data frame containing, id column, data column, factor column,
#' return column.
#' @param date_col_name A character representing the column name of the dates.
#' @param id_col_name A character representing the column name of the
#'  identifiers.
#' @param factor_col_name A character representing the column name of the
#'  factor.
#' @param return_col_name A character representing the column name of the
#'  returns.
#' @returns A FactorData object.
#' @import dplyr
#' @import methods
#' @include Utilities.R
#' @export
create_factor_data <- function(
  data,
  date_col_name,
  id_col_name,
  factor_col_name,
  return_col_name
) {

  # Split Data by Data Column
  ldata <- data %>% named_group_split(!!sym(date_col_name))

  # Convert split data into SinglePeriodFactorData
  fdata <- lapply(
    ldata,
    SinglePeriodFactorData,
    date = as.Date(names(ldata)),
    iname = id_col_name,
    fname = factor_col_name,
    rname = return_col_name
  )

  return(new("FactorData", data = fdata))
}

#' @export
calc_factor_avail <- function(factor_data, ...) UseMethod("calc_factor_avail")
setMethod("factor_avail",
  signature(factor_data = "factor_data"),
  function(factor_data, ...) {
    .avail <- function(.data) {
      fvals <- .data@fvals
      length(which(!is.na(fvals))) / length(fvals)
    }
    unlist(lapply(FactorData@data, .avail))
  }
)