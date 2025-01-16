#' @title Alpha Test Data (S4 Object)
#' @description An S4 Class to represent alpha test data for multi-periods
#' @slot alpha_data A list of single_period_at_data objects.
#' @slot data A data.frame containing raw data.
#' @slot params An at_data_params object.
#' @include class-at_data_params.R
setClass(
  "at_data",
  representation(
    alpha_data = "list",
    data = "data.frame",
    params = "at_data_params"
  )
)