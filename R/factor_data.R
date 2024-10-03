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

#' @title Create Factor Data
#' @description create factor data
#' @param factor_data factor_data_object
#' @export
#' @keywords internal
create_fdata <- function(factor_data) UseMethod("create_fdata")

#' @import dplyr
#' @include utilities.R
#' @include factor_data_params.R
#' @include factor_data_single_period.R
#' @export
create_fdata.factor_data <- function(factor_data) {
  if(check_factor_data_params(factor_data@params, factor_data@data)) { # nolint
    ldata <- factor_data@data %>% # nolint: object_usage_linter.
      named_group_split(!!rlang::sym(factor_data@params@date_col_name))
    fdata <- lapply(
      ldata,
      create_single_period_factor_data,
      date_col_name = factor_data@params@date_col_name,
      id_col_name = factor_data@params@id_col_name,
      factor_col_name = factor_data@params@factor_col_name,
      return_col_name = factor_data@params@return_col_name,
      group_col_name = factor_data@params@group_col_name,
      weight_col_name = factor_data@params@weight_col_name
    )
    factor_data@factor_data <- fdata
  }
  factor_data
}

#' @title Set Date Column Name
#' @description Set the date column for factor_data_params
#' @param x factor_data or factor_data_params object
#' @param date_col_name a string representing the date column name
#' @returns updated factor_data or factor_data_params object
#' @export
set_date_col <- function(x, date_col_name) UseMethod("set_date_col")
#' @export
set_date_col.factor_data <- function(x, date_col_name) {
  x@params@date_col_name <- date_col_name
  create_fdata(x)
}
#' @export
set_date_col.factor_data_params <- function(x, date_col_name) {
  x@date_col_name <- date_col_name
  x
}

#' @title Set ID Column Name
#' @description Set the ID column for factor_data_params
#' @param x factor_data or factor_data_params object
#' @param id_col_name a string representing the id column name
#' @returns updated factor_data or factor_data_params object
#' @export
set_id_col <- function(x, date_col_name) UseMethod("set_id_col")
#' @export
set_id_col.factor_data <- function(x, id_col_name) {
  x@params@id_col_name <- id_col_name
  create_fdata(x)
}
#' @export
set_id_col.factor_data_params <- function(x, id_col_name) {
  x@id_col_name <- id_col_name
  x
}

#' @title Set Factor Column Name
#' @description Set the Factor column for factor_data_params
#' @param x factor_data or factor_data_params object
#' @param factor_col_name a string representing the factor column name
#' @returns updated factor_data or factor_data_params object
#' @export
set_factor_col <- function(x, factor_col_name) UseMethod("set_factor_col")
#' @export
set_factor_col.factor_data <- function(x, factor_col_name) {
  x@params@factor_col_name <- factor_col_name
  create_fdata(x)
}
#' @export
set_factor_col.factor_data_params <- function(x, factor_col_name) {
  x@factor_col_name <- factor_col_name
  x
}

#' @title Set Return Column Name
#' @description Set the Return column for factor_data_params
#' @param x factor_data or factor_data_params object
#' @param return_col_name a string representing the return column name
#' @returns updated factor_data or factor_data_params object
#' @export
set_return_col <- function(x, return_col_name) UseMethod("set_return_col")
#' @export
set_return_col.factor_data <- function(x, return_col_name) {
  x@params@return_col_name <- return_col_name
  create_fdata(x)
}
#' @export
set_return_col.factor_data_params <- function(x, return_col_name) {
  x@return_col_name <- return_col_name
  x
}

#' @title Set Group Column Name
#' @description Set the Group column for factor_data_params
#' @param x factor_data or factor_data_params object
#' @param group_col_name a string representing the group column name
#' @returns updated factor_data or factor_data_params object
#' @export
set_group_col <- function(x, group_col_name) UseMethod("set_group_col")
#' @export
set_group_col.factor_data <- function(x, group_col_name) {
  x@params@group_col_name <- group_col_name
  create_fdata(x)
}
#' @export
set_group_col.factor_data_params <- function(x, group_col_name) {
  x@group_col_name <- group_col_name
  x
}

#' @title Set Weight Column Name
#' @description Set the Weight column for factor_data_params
#' @param x factor_data or factor_data_params object
#' @param weight_col_name a string representing the weight column name
#' @returns updated factor_data or factor_data_params object
#' @export
set_weight_col <- function(x, weight_col_name) UseMethod("set_weight_col")
#' @export
set_weight_col.factor_data <- function(x, weight_col_name) {
  x@params@weight_col_name <- weight_col_name
  create_fdata(x)
}
#' @export
set_weight_col.factor_data_params <- function(x, weight_col_name) {
  x@weight_col_name <- weight_col_name
  x
}