#' @title Create Alpha Test Data Parameters Object
#' @description
#' Function to create and configure an `at_data_params` object, used to set
#' parameters for factor data in alpha testing.
#'
#' @param date_col_name A string representing the column name of date values.
#' @param id_col_name A string representing the column name of ID values.
#' @param factor_col_name A string representing the column name of factor
#'   values.
#' @param return_col_name A string representing the column name of return
#'   values.
#' @param include_col_name A string representing the column name for include
#'   flags (TRUE/FALSE).
#' @param group_col_name A string representing the column name for grouping
#'   values.
#' @param weight_col_name A string representing the column name for weights.
#' @param horizon A numeric value representing the return and factor horizon.
#'   Defaults to 12.
#' @return An `at_data_params` S4 object.
#' @include class-at_data_params.R
#' @export
create_at_data_params <- function(
  date_col_name = NA_character_, id_col_name = NA_character_,
  factor_col_name = NA_character_, return_col_name = NA_character_,
  include_col_name = NA_character_, group_col_name = NA_character_,
  weight_col_name = NA_character_, horizon = 12
) {
  new("at_data_params",
    date_col_name = date_col_name,
    id_col_name = id_col_name,
    factor_col_name = factor_col_name,
    return_col_name = return_col_name,
    include_col_name = include_col_name,
    group_col_name = group_col_name,
    weight_col_name = weight_col_name,
    horizon = horizon
  )
}