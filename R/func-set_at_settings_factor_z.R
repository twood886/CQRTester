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
#' @include class-at_settings_factor_z.R
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
    cat(paste0(out, "\n"))
  }
)