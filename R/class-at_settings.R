#' @title Alpha Testing Settings (S4 Object)
#' @description
#' An S4 object that encapsulates settings for alpha testing.
#'
#' @slot start_date A `Date` object representing the earliest date for alpha
#'   testing.
#' @slot end_date A `Date` object representing the latest date for alpha
#'   testing.
#' @slot testing_scheme A character string representing the alpha testing
#'   scheme.
#' @slot weighting_scheme A character string representing the weighting scheme.
#' @slot benchmark_weighting_scheme A character string representing the
#'   benchmark weighting scheme.
#' @slot .desc A logical value indicating whether the factor scores should
#'   be ranked in descending order.
setClass(
  "at_settings",
  representation(
    start_date = "Date",
    end_date = "Date",
    testing_scheme = "character",
    weighting_scheme = "character",
    benchmark_weighting_scheme = "character",
    .desc = "logical"
  )
)