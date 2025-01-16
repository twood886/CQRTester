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
#' @include func-create_at_data_params.R
#' @include class-at_data.R
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
#' @keywords internal
create_single_period_at_data_list <- function(at_data) { # nolint
  UseMethod("create_single_period_at_data_list")
}

#' @include method-check_at_data_params.R
#' @include func-create_single_period_at_data.R
#' @importFrom pbapply pblapply
#' @keywords internal
#' @include class-at_data.R
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


# Show Method ------------------------------------------------------------------
#' @include class-at_data.R
#' @include func-pivot_lospat_slot.R
setMethod(f = "show",
  signature(object = "at_data"),
  function(object) {
    dates <- as.Date(sapply(object@alpha_data, \(x) as.character(x@date)))

    pivot_lospat_slot(object@alpha_data, "date")
    out <- paste(
      paste("------------ AT Data -------------"),
      paste("Date Column:", object@params@date_col_name, sep = "\t"),
      paste("Id Column:", object@params@id_col_name, sep = "\t"),
      paste("Factor Column:", object@params@factor_col_name, sep = "\t"),
      paste("Include Column:", object@params@include_col_name, sep = "\t"),
      paste("Group Column:", object@params@group_col_name, sep = "\t"),
      paste("Weight Column:", object@params@weight_col_name, sep = "\t"),
      paste("Horizon:", object@params@horizon, sep = "\t"),
      paste("Date Range:",
        paste(min(dates), ":", max(dates)),
        sep = "\t"
      ),
      paste("Periods:", length(dates), sep = "\t"),
      "----------------------------------",
      sep = "\n"
    )
    cat(paste0(out, "\n"))
  }
)