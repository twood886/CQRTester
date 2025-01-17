#' @title Alpha Test Data Parameters
#' @description
#' An S4 class containing the parameters for configuring an `at_data`
#' object.
#'
#' @slot date_col_name A character string representing the column name for
#'   date values.
#' @slot id_col_name A character string representing the column name for ID
#'   values.
#' @slot factor_col_name A character string representing the column name for
#'   factor values.
#' @slot return_col_name A character string representing the column name for
#'   return values.
#' @slot include_col_name A character string representing the column name for
#'   inclusion flags (TRUE/FALSE).
#' @slot group_col_name A character string representing the column name for
#'   group values.
#' @slot weight_col_name A character string representing the column name for
#'   weights.
#' @slot horizon A numeric value representing the return and factor horizon.
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