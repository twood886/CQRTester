# factor_data_params (S4 Object) -------------------------------------------
#' @title Factor Data Parameters
#' @description
#' An S4 class containing the parameters for creating a factor_data object
#' @slot data_col_name
#' @slot id_col_name
#' @slot factor_col_name
#' @slot return_col_name
#' @slot group_col_name
#' @slot weight_col_name
setClass(
  "factor_data_params",
  representation(
    date_col_name = "character",
    id_col_name = "character",
    factor_col_name = "character",
    return_col_name = "character",
    weight_col_name = "character",
    group_col_name = "character"
  )
)

#' @title Create Factor Data Parameters Object
#' @description
#' Function to set parameters for factor data, creating a factor_data_params
#' object
#' @param date_col_name A string representing the column name of date values
#' @param id_col_name A string representing the column name of id values
#' @param factor_col_name A string representing the column name of factor values
#' @param return_col_name A string representing the column name of return values
#' @param group_col_name A string representing the column name of grouping value
#' @param weight_col_name A string representing the column name of weights
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







#' @include generic_methods.R
setMethod("check_factor_data_params",
  signature(params = "factor_data_params", data = "data.frame"),
  function(params, data) {
    date_col_name <- params@date_col_name
    id_col_name <- params@id_col_name
    factor_col_name <- params@factor_col_name
    return_col_name <- params@return_col_name
    group_col_name <- params@group_col_name
    weight_col_name <- params@weight_col_name
    data_col_names <- colnames(data)

    entered_names <-
      all(!is.na(c(
        date_col_name, id_col_name, factor_col_name, return_col_name
      )))

    names_in_data <-
      all(c(
        date_col_name, id_col_name, factor_col_name, return_col_name,
        group_col_name, weight_col_name
      ) %in% c(data_col_names, NA_character_),
      na.rm = TRUE)

    all(c(entered_names, names_in_data))
  }
)
