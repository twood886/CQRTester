#' @title Create Factor Data Parameters Object
#' @description
#' Function to create and configure a `factor_data_params` object, used for
#' specifying factor-related data parameters.
#'
#' @param date_col_name A string representing the column name of date values.
#' @param id_col_name A string representing the column name of ID values.
#' @param factor_col_name A string representing the column name of factor
#'   values.
#' @param return_col_name A string representing the column name of return
#'   values.
#' @param group_col_name A string representing the column name for grouping
#'   values.
#' @param weight_col_name A string representing the column name for weights.
#' @return A `factor_data_params` S4 object.
#' @include class-factor_data_params.R
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