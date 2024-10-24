# at_data_params (S4 Object) -----------------------------------------------
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

setGeneric("check_at_data_params",
  function(params, data) standardGeneric("check_at_data_params")
)
setMethod("check_at_data_params",
  signature(params = "at_data_params", data = "data.frame"),
  function(params, data) {
    date_col_name <- params@date_col_name
    id_col_name <- params@id_col_name
    factor_col_name <- params@factor_col_name
    return_col_name <- params@return_col_name
    include_col_name <- params@include_col_name
    group_col_name <- params@group_col_name
    weight_col_name <- params@weight_col_name
    data_col_names <- colnames(data)

    entered_names <-
      all(!is.na(c(
        date_col_name, id_col_name,
        factor_col_name, return_col_name,
        include_col_name
      )))

    names_in_data <-
      all(c(
        date_col_name, id_col_name,
        factor_col_name, return_col_name, include_col_name,
        group_col_name, weight_col_name
      ) %in% c(data_col_names, NA_character_),
      na.rm = TRUE)

    all(c(entered_names, names_in_data))
  }
)
