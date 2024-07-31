# factor_data (S4 Object) --------------------------------------------------
#' @title Factor Data (S4 Object)
#' @description An S4 Class to represent Factor Data over time
#' @slot factor_data A list of SinglePeriodFactorData objects.
#' @slot data A data.frame containing raw data.
#' @slot params A factor_data_params object
#' @include factor_data_single_period.R
setClass(
  "factor_data",
  representation(
    factor_data = "list",
    data = "data.frame",
    params = "factor_data_params"
  )
)

#' @title Create Factor Data
#' @description Function to create factor data from data frame.
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
#' @include utilities.R
#' @include factor_data_params.R
#' @include factor_data_single_period.R
#' @export
create_factor_data <- function(
  data, date_col_name = NA_character_, id_col_name = NA_character_,
  factor_col_name = NA_character_, return_col_name = NA_character_,
  group_col_name = NA_character_, weight_col_name = NA_character_
) {
  factor_data_params <- create_factor_data_params(
    date_col_name = date_col_name,
    id_col_name = id_col_name,
    factor_col_name = factor_col_name,
    return_col_name = return_col_name,
    group_col_name = group_col_name,
    weight_col_name = weight_col_name
  )
  x <- new("factor_data",
    factor_data = as.list(NULL),
    data = data,
    params = factor_data_params
  )
  create_fdata(x)
}



create_fdata <- function(factor_data) UseMethod("create_fdata")
#' @include utilities.R
#' @include factor_data_params.R
#' @include factor_data_single_period.R
create_fdata.factor_data <- function(factor_data) {
  if(check_factor_data_params(factor_data@params, factor_data@data)) { # nolint
    ldata <- data %>%
      named_group_split(!!sym(factor_data@params@date_col_name))
    fdata <- lapply(
      ldata,
      create_single_period_factor_data,
      date = as.Date(names(ldata)),
      id_col_name = factor_data@params@id_col_name,
      factor_col_name = factor_data@params@factor_col_name,
      return_col_name = factor_data@params@return_col_name
    )
    factor_data@factor_data <- fdata
  }
  factor_data
}


set_date_col <- function(x, date_col_name) UseMethod("set_date_col")
set_date_col.factor_data <- function(x, date_col_name) {
  x@params@date_col_name <- date_col_name
  create_fdata(x)
}

set_id_col <- function(x, date_col_name) UseMethod("set_id_col")
set_id_col.factor_data <- function(x, id_col_name) {
  x@params@id_col_name <- id_col_name
  create_fdata(x)
}

set_factor_col <- function(x, factor_col_name) UseMethod("set_factor_col")
set_factor_col.factor_data <- function(x, factor_col_name) {
  x@params@factor_col_name <- factor_col_name
  create_fdata(x)
}

set_return_col <- function(x, return_col_name) UseMethod("set_return_col")
set_return_col.factor_data <- function(x, return_col_name) {
  x@params@return_col_name <- return_col_name
  create_fdata(x)
}

set_group_col <- function(x, group_col_name) UseMethod("set_group_col")
set_group_col.factor_data <- function(x, group_col_name) {
  x@params@group_col_name <- group_col_name
  create_fdata(x)
}

set_weight_col <- function(x, weight_col_name) UseMethod("set_weight_col")
set_weight_col.factor_data <- function(x, weight_col_name) {
  x@params@weight_col_name <- weight_col_name
  create_fdata(x)
}