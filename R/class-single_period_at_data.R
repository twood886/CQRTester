# single_period_at_data (S4 Object) --------------------------------------------
#' @title Single Period Alpha Test Data (S4 Object)
#' @description An S4 Class to represent Alpha Test Data for a single period.
#' @slot factor A character representing the name of the factor.
#' @slot date A date object representing the date of the data.
#' @slot ids A character vector representing the company ids.
#' @slot fvales An orderedList of factor values.
#' @slot returns An orderedList of returns.
#' @slot group A named character vector representing the company grouping.
#' @slot weights A named numeric vector of weights from factor values.
#' @include class-orderedList.R
setClass(
  "single_period_at_data",
  representation(
    factor = "character",
    date = "Date",
    ids = "character",
    fvals = "orderedList",
    returns = "orderedList",
    group = "character",
    weights = "numeric"
  )
)