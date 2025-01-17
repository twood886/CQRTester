#' @title Set Quantile Spread Alpha Testing Settings
#' @description
#' A function to create Alpha Testing settings for a Quantile Spread Alpha Test.
#' @param start_date description
#' @param end_date description
#' @param weighting_scheme description
#' @param benchmark_weighting_scheme description
#' @param .desc Should factor score be ranked in descending order?
#' @param quantiles description
#' @param ... Addtional arguements to be passesd to function.
#' @returns An at_settings_q_spread S4 object to be used in Alpha Testing.
#' @include class-at_settings_factor_q.R
set_at_settings_factor_q <- function(
  start_date = as.Date("1901-01-01"), end_date = Sys.Date(),
  weighting_scheme = "equal",  benchmark_weighting_scheme = "zero",
  .desc = TRUE, quantiles = 5, ...
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
    .desc = .desc,
    quantiles = quantiles
  )
}

# Show Method ------------------------------------------------------------------
setMethod(f = "show",
  signature(object = "at_settings_factor_q"),
  function(object) {
    out <- paste(
      paste("----- Alpha Testing Settings -----"),
      paste("Testing Scheme:", object@testing_scheme, sep = "\t\t"),
      paste("Weighting Scheme:", object@weighting_scheme, sep = "\t"),
      paste("Benchmark Weighting:", object@benchmark_weighting_scheme, sep = "\t"), # nolint: line_length_linter.
      paste("Start Date:", format(object@start_date, "%b %d,%Y"), sep = "\t\t"),
      paste("End Date:", format(object@end_date, "%b %d,%Y"), sep = "\t\t"),
      paste("Factor Quantiles:", object@quantiles, sep = "\t"),
      "----------------------------------",
      sep = "\n"
    )
    cat(paste0(out, "\n"))
  }
)