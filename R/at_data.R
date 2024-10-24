# at_data (S4 Object) ----------------------------------------------------------
#' @title Alpha Test Data (S4 Object)
#' @description An S4 Class to represent alpha test data for multi-periods
#' @slot alpha_data A list of single_period_at_data objects.
#' @slot data A data.frame containing raw data.
#' @slot params An at_data_params object.
#' @include at_data_single_period.R
setClass(
  "at_data",
  representation(
    alpha_data = "list",
    data = "data.frame",
    params = "at_data_params"
  )
)

#' @title Create Alpha Data
#' @description Function to create alpha data from data frame.
#' @param data A data frame containing, id column, data column, factor column,
#' return column.
#' @param date_col_name A string representing the column name of date values.
#' @param id_col_name A string representing the column name of id values.
#' @param factor_col_name A string representing the column name of factor values. # nolint: line_length_linter.
#' @param return_col_name A string representing the column name of return values. # nolint: line_length_linter.
#' @param include_col_name A string representing the column name of include T/F.
#' @param group_col_name A string representing the column name of grouping value. # nolint: line_length_linter.
#' @param weight_col_name A string representing the column name of weights.
#' @param horizon A numeric representing the return and factor horizon.
#' @returns An at_data object.
#' @import dplyr
#' @import methods
#' @include utilities.R
#' @include at_data_params.R
#' @include at_data_single_period.R
#' @export
create_at_data <- function(
  data, date_col_name = NA_character_, id_col_name = NA_character_,
  factor_col_name = NA_character_, return_col_name = NA_character_,
  include_col_name = NA_character_, group_col_name = NA_character_,
  weight_col_name = NA_character_, horizon = 12
) {
  params <- create_at_data_params(
    date_col_name = date_col_name,
    id_col_name = id_col_name,
    factor_col_name = factor_col_name,
    return_col_name = return_col_name,
    include_col_name = include_col_name,
    group_col_name = group_col_name,
    weight_col_name = weight_col_name,
    horizon = horizon
  )
  x <- new("at_data",
    alpha_data = as.list(NULL),
    data = data,
    params = params
  )
  create_single_period_at_data_list(x)
}

#' @title Create Single Period Alpha Test Data List
#' @param at_data factor_data_object
#' @include at_data_params.R
#' @include at_data_single_period.R
#' @keywords internal
create_single_period_at_data_list <- function(at_data) { # nolint
  UseMethod("create_single_period_at_data_list")
}

#' @include at_data_params.R
#' @include at_data_single_period.R
#' @importFrom pbapply pblapply
#' @keywords internal
create_single_period_at_data_list.at_data <- function(at_data) {
  if (check_at_data_params(at_data@params, at_data@data)) {
    dates <- sort(unique(at_data@data[[at_data@params@date_col_name]]))
    horizon <- at_data@params@horizon
    print("Creating AT Data for Each Period")
    alpha_data <- pbapply::pblapply(
      dates[horizon : (length(dates) - horizon + 1)],
      create_single_period_at_data,
      data = at_data@data,
      date_col_name = at_data@params@date_col_name,
      id_col_name = at_data@params@id_col_name,
      factor_col_name = at_data@params@factor_col_name,
      return_col_name = at_data@params@return_col_name,
      include_col_name = at_data@params@include_col_name,
      group_col_name = at_data@params@group_col_name,
      weight_col_name = at_data@params@weight_col_name,
      horizon = at_data@params@horizon
    )
    at_data@alpha_data <- alpha_data
  }
  at_data
}