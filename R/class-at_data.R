#' @title Alpha Test Data (S4 Object)
#' @description
#' An S4 class to represent alpha test data for multiple periods.
#'
#' @slot alpha_data A list of `single_period_at_data` objects representing
#'   alpha test results for individual periods.
#' @slot data A `data.frame` containing the raw input data used in the
#'   alpha test.
#' @slot params An `at_data_params` object containing the parameters used
#'   to configure the alpha test.
#' @include class-at_data_params.R
setClass(
  "at_data",
  representation(
    alpha_data = "list",
    data = "data.frame",
    params = "at_data_params"
  )
)