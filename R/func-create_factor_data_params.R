#' @title Create Factor Data Parameters Object
#' @description
#' Function to set parameters for factor data, creating a factor_data_params
#' object.
#' @param date_col_name A string representing the column name of date values.
#' @param id_col_name A string representing the column name of id values.
#' @param factor_col_name
#' A string representing the column name of factor values.
#' @param return_col_name
#' A string representing the column name of return values.
#' @param group_col_name
#' A string representing the column name of grouping value.
#' @param weight_col_name A string representing the column name of weights.
#' @include class-factor_data_params.R
#' @return factor_data_params object
#' @export
create_factor_data_params <- function(
  date_col_name = NA_character_, id_col_name = NA_character_,
  factor_col_name = NA_character_, return_col_name = NA_character_,
  group_col_name = NA_character_, weight_col_name = NA_character_
) {
  new("factor_data_params",
    date_col_name = date_col_name,
    id_col_name = id_col_name,
    factor_col_name = factor_col_name,
    return_col_name = return_col_name,
    group_col_name = group_col_name,
    weight_col_name = weight_col_name
  )
}