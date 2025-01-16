#' @title Alpha Test Data Parameters
#' @description
#' An S4 class containing the parameters for creating a at_data object
#' @slot data_col_name A string representing the column name of date values.
#' @slot id_col_name A string representing the column name of id values.
#' @slot factor_col_name A string representing the column name of factor values.
#' @slot return_col_name A string representing the column name of return values.
#' @slot include_col_name A string representing the column name of include T/F.
#' @slot group_col_name A string representing the column name of grouping value.
#' @slot weight_col_name A string representing the column name of weights.
#' @slot horizon return and factor horizon
setClass(
  "at_data_params",
  representation(
    date_col_name = "character",
    id_col_name = "character",
    factor_col_name = "character",
    return_col_name = "character",
    include_col_name = "character",
    weight_col_name = "character",
    group_col_name = "character",
    horizon = "numeric"
  )
)