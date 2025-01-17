#' @title Single Period Alpha Test Data (S4 Object)
#' @description
#' Represents alpha test data for a single period, encapsulating results and
#' configurations for individual factor and return evaluations.
#'
#' @slot factor A character string representing the name of the factor.
#' @slot date A `Date` object representing the specific period date.
#' @slot ids A character vector containing company IDs for the period.
#' @slot fvals An `orderedList` containing factor values for the period.
#' @slot returns An `orderedList` containing returns data for the period.
#' @slot group A named character vector indicating company groupings.
#' @slot weights A named numeric vector of weights derived from factor values.
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