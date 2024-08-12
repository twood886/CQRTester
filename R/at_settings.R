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

# at_settings_factor_z (S4 Object) -----------------------------------------
#' @title Settings for Factor Zscore Weighted Alpha Testing (S4 Object)
#' @slot win_prob A numeric vector of length 2 representing the percentile
#'cut-offs for windsorization.
setClass(
  "at_settings_factor_z",
  contains = "at_settings",
  representation(win_prob = "numeric")
)

# at_settings_factor_q (S4 Object) -----------------------------------------
#' @title Settings for Factor Quantile Weighted Alpha Testing (S4 Object)
#' @slot quantiles Number of Quantiles to use.
setClass(
  "at_settings_factor_q",
  contains = "at_settings",
  representation(quantiles = "numeric")
)

#' @title Set Factor Weighted Alpha Testing Settings
#' @description
#' A function to create Alpha Testing settings for a factor weighted Alpha Test.
#' @param start_date description
#' @param end_date description
#' @param weighting_scheme description
#' @param benchmark_weighting_scheme description
#' @param win_prob A numeric vector of length 2 representing the percentile
#' @param ... Additional arguements to be passed to function.
#' @returns An at_settings_factor_w S4 object to be used in Alpha Testing.
set_at_settings_factor_z <- function(
  start_date = as.Date("1901-01-01"), end_date = Sys.Date(),
  weighting_scheme = "z-weighted", benchmark_weighting_scheme = "zero",
  win_prob = c(0, 1), ...
) {
  dargs <- list(...)

  if ("win_prob" %in% names(dargs))
    win_prob <- dargs$win_prob

  new("at_settings_factor_z",
    start_date = start_date,
    end_date = end_date,
    testing_scheme = "factor_z",
    weighting_scheme = weighting_scheme,
    benchmark_weighting_scheme = benchmark_weighting_scheme,
    win_prob = win_prob
  )
}

#' @title Set Quantile Spread Alpha Testing Settings
#' @description
#' A function to create Alpha Testing settings for a Quantile Spread Alpha Test.
#' @param start_date description
#' @param end_date description
#' @param weighting_scheme description
#' @param benchmark_weighting_scheme description
#' @param quantiles description
#' @param ... Addtional arguements to be passesd to function.
#' @returns An at_settings_q_spread S4 object to be used in Alpha Testing.
set_at_settings_factor_q <- function(
  start_date = as.Date("1901-01-01"), end_date = Sys.Date(),
  weighting_scheme = "equal",  benchmark_weighting_scheme = "zero",
  quantiles = 5, ...
) {
  dargs <- list(...)

  if ("quantiles" %in% names(dargs)) {
    quantiles <- dargs$quantiles
  }

  new("at_settings_factor_q",
    start_date = start_date,
    end_date = end_date,
    testing_scheme = "factor-q",
    weighting_scheme = weighting_scheme,
    benchmark_weighting_scheme = benchmark_weighting_scheme,
    quantiles = quantiles
  )
}

# set_at_settings Function -----------------------------------------------------
#' @title Create Alpha Testing Settings
#' @description
#' This function creates an "at_settings" class with arguments to be used when
#' testing factors
#' @param start_date A date object representing the earliest start date to be
#' used when performing alpha testing. Factor data is filtered to start at or
#' after provided Start.Date argument.
#' @param end_date A date object representing the last date to be
#' used when performing alpha testing. Factor data is filtered to end at or
#' before provided End.Date argument.
#' @param testing_scheme Weighting scheme to be used in testing factor.
#' Currently supports "Factor" and "Quantile"
#' @param weighting_scheme desctiption
#' @param benchmark_weighting_scheme description
#' @param ... Additional settings to be passed based on Weighting.Scheme
#' @export
set_at_settings <- function(
    testing_scheme = "factor-z",
    start_date = as.Date("1901-01-01"), end_date = Sys.Date(),
    weighting_scheme = NULL, benchmark_weighting_scheme = "zero", ...) {
  known_schemes <- c("factor-z", "factor-q")

  if (!testing_scheme %in% known_schemes) {
    stop("Testing Scheme must be one of \"factor-z\" or \"factor-q\".")
  }

  if (testing_scheme == "factor-z") {
    if (is.null(weighting_scheme)) {
      weighting_scheme <- "z-weighted"
    }
    at <- set_at_settings_factor_z(
      start_date, end_date, weighting_scheme, benchmark_weighting_scheme, ...
    )
  }

  if (testing_scheme == "factor-q") {
    if (is.null(weighting_scheme)) {
      weighting_scheme <- "equal-spread"
    }
    at <- set_at_settings_factor_q(
      start_date, end_date, weighting_scheme, benchmark_weighting_scheme, ...
    )
  }
  at
}





# Show Method -------------------------------------------------------------
setMethod(f = "show",
  signature(object = "at_settings_factor_z"),
  function(object) {
    out <- paste(
      paste("----- Alpha Testing Settings -----"),
      paste("Testing Scheme:", object@testing_scheme, sep = "\t\t"),
      paste("Weighting Scheme:", object@weighting_scheme, sep = "\t"),
      paste("Benchmark Weighting:", object@benchmark_weighting_scheme, sep = "\t"), # nolint: line_length_linter.
      paste("Start Date:", format(object@start_date, "%b %d,%Y"), sep = "\t\t"),
      paste("End Date:", format(object@end_date, "%b %d,%Y"), sep = "\t\t"),
      paste("Windsorization:",
        paste(
          paste0(format(object@win_prob[1] * 100, digits = 2), "%"),
          paste0(format(object@win_prob[2] * 100, digits = 2), "%"),
          sep = " - "
        ),
        sep = "\t\t"
      ),
      "----------------------------------",
      sep = "\n"
    )
    cat(out)
  }
)

setMethod(f = "show",
  signature(object = "at_settings_factor_q"),
  function(object) {
    out <- paste(
      paste("----- Alpsha Testing Settings -----"),
      paste("Testing Scheme:", object@testing_scheme, sep = "\t\t"),
      paste("Weighting Scheme:", object@weighting_scheme, sep = "\t"),
      paste("Benchmark Weighting:", object@benchmark_weighting_scheme, sep = "\t"), # nolint: line_length_linter.
      paste("Start Date:", format(object@start_date, "%b %d,%Y"), sep = "\t\t"),
      paste("End Date:", format(object@end_date, "%b %d,%Y"), sep = "\t\t"),
      paste("Factor Quantiles:", object@quantiles, sep = "\t"),
      "----------------------------------",
      sep = "\n"
    )
    cat(out)
  }
)