# at_settings (S4 Object) --------------------------------------------------
#' @title Alpha Testing Settings (S4 Object)
#' @description
#' An S4 Object containing the settings to be used in Alpha Testing. Arguments
#' passed through ATSettings are used in designing the alpha testing procedure.
#' @slot start_date Date representing the earliest date for Alpha Testing.
#' @slot end_date Date representing the latest date for Alpha Testing.
#' @slot testing_scheme Character representing alpha testing procedure.
#' @slot weighting_scheme Character representing weighting scheme.
#' @slot benchmark_weighting_scheme Character represenitng the weighting scheme
#' for the benchmark.
setClass(
  "at_settings",
  representation(
    start_date = "Date",
    end_date = "Date",
    testing_scheme = "character",
    weighting_scheme = "character",
    benchmark_weighting_scheme = "character"
  )
)