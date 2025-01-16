#' @title Create Alpha Test Data Parameters Object
#' @description
#' Function to set parameters for factor data, creating a at_data_params object.
#' @param date_col_name A string representing the column name of date values.
#' @param id_col_name A string representing the column name of id values.
#' @param factor_col_name A string representing the column name of factor values. # nolint: line_length_linter.
#' @param return_col_name A string representing the column name of return values. # nolint: line_length_linter.
#' @param include_col_name A string representing the column name of include T/F.
#' @param group_col_name A string representing the column name of grouping value. # nolint: line_length_linter.
#' @param weight_col_name A string representing the column name of weights.
#' @param horizon A numeric representing the return and factor horizon.
#' @return at_data_params object
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