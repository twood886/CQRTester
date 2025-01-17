#' @title Create Alpha Testing Settings
#'
#' @description
#' Creates an object of class `at_settings` with the specified arguments to
#' configure alpha testing settings. Supports multiple testing schemes and
#' weighting options for factor analysis.
#'
#' @param testing_scheme A character string specifying the testing scheme to
#'   use. Must be one of \code{"factor-z"} or \code{"factor-q"}. Defaults to
#'   \code{"factor-z"}.
#' @param start_date A \code{Date} object representing the earliest start date
#'   for alpha testing. Factor data will be filtered to include only dates on
#'   or after this value. Defaults to \code{"1901-01-01"}.
#' @param end_date A \code{Date} object representing the latest end date for
#'   alpha testing. Factor data will be filtered to include only dates on or
#'   before this value. Defaults to the current system date (\code{Sys.Date()}).
#' @param weighting_scheme A character string specifying the weighting scheme
#'   to use. Available options depend on the \code{testing_scheme}:
#'   \itemize{
#'     \item For \code{"factor-z"}: \code{"z-weighted"}, \code{"long-only"},
#'     \code{"short-only"}.
#'     \item For \code{"factor-q"}: \code{"equal-spread"}, \code{"equal-long-only"},
#'     \code{"equal-short-only"}.
#'   }
#'   If not provided, defaults to \code{"z-weighted"} for \code{"factor-z"} and
#'   \code{"equal-spread"} for \code{"factor-q"}.
#' @param benchmark_weighting_scheme A character string specifying the benchmark
#'   weighting scheme. Defaults to \code{"zero"}.
#' @param .desc A logical value indicating whether factor scores should be
#'   ranked in descending order. Defaults to \code{TRUE}.
#' @param ... Additional settings to be passed based on the specified
#'   \code{testing_scheme}.
#'
#' @details
#' This function supports two testing schemes:
#' \itemize{
#'   \item \code{"factor-z"}: Uses Z-weighted factor scores for testing. Available
#'   \code{weighting_scheme} options are \code{"z-weighted"}, \code{"long-only"},
#'   and \code{"short-only"}.
#'   \item \code{"factor-q"}: Uses quantile-based equal-spread factor scores for
#'   testing. Available \code{weighting_scheme} options are \code{"equal-spread"},
#'   \code{"equal-long-only"}, and \code{"equal-short-only"}.
#' }
#'
#' Depending on the scheme, this function delegates the creation of the settings
#' object to the appropriate internal function (\code{set_at_settings_factor_z} or
#' \code{set_at_settings_factor_q}).
#'
#' @return
#' An object of class \code{at_settings} configured with the specified parameters.
#'
#' @seealso
#' \code{\link[=set_at_settings_factor_z]{set_at_settings_factor_z}} for the
#' factor-Z scheme, and \code{\link[=set_at_settings_factor_q]{set_at_settings_factor_q}}
#' for the factor-Q scheme.
#'
#' @examples
#' # Example 1: Create settings with default parameters
#' settings <- set_at_settings()
#'
#' # Example 2: Specify custom parameters for factor-z
#' settings <- set_at_settings(
#'   testing_scheme = "factor-z",
#'   start_date = as.Date("2020-01-01"),
#'   end_date = as.Date("2023-01-01"),
#'   weighting_scheme = "long-only",
#'   benchmark_weighting_scheme = "custom-benchmark",
#'   .desc = TRUE
#' )
#'
#' # Example 3: Specify custom parameters for factor-q
#' settings <- set_at_settings(
#'   testing_scheme = "factor-q",
#'   weighting_scheme = "equal-short-only",
#'   .desc = FALSE
#' )
#'
#' @include func-set_at_settings_factor_z.R
#' @include func-set_at_settings_factor_q.R
#' @export
set_at_settings <- function(
    testing_scheme = "factor-z",
    start_date = as.Date("1901-01-01"), end_date = Sys.Date(),
    weighting_scheme = NULL, benchmark_weighting_scheme = "zero",
    .desc = TRUE, ...) {
  known_schemes <- c("factor-z", "factor-q")

  if (!testing_scheme %in% known_schemes) {
    stop("Testing Scheme must be one of \"factor-z\" or \"factor-q\".")
  }

  if (testing_scheme == "factor-z") {
    if (is.null(weighting_scheme)) {
      weighting_scheme <- "z-weighted"
    }
    at <- set_at_settings_factor_z(
      start_date, end_date, weighting_scheme,
      benchmark_weighting_scheme, .desc, ...
    )
  }

  if (testing_scheme == "factor-q") {
    if (is.null(weighting_scheme)) {
      weighting_scheme <- "equal-spread"
    }
    at <- set_at_settings_factor_q(
      start_date, end_date, weighting_scheme,
      benchmark_weighting_scheme, .desc, ...
    )
  }
  at
}