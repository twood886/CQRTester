# alpha_test (S4 Object) ---------------------------------------------------
#' @title Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing
#' @slot returns A numeric array of returns.
#' @slot return_bmark A numeric array of benchmark returns.
#' @slot weights A list of weights.
#' @slot weights_bmark A list of benchmark weights.
#' @include alpha_test_single_period.R
setClass(
  "alpha_test",
  slots = c(
    returns = "numeric",
    return_bmark = "numeric",
    weights = "list",
    weights_bmark = "list"
  )
)

# alpha_test_factor_z (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Z-Score) S4 Object
#' @slot IC Information Coefficient
#' @slot factor_z_score A list of factor z-scores.
#' @keywords internal
setClass(
  "alpha_test_factor_z",
  contains = "alpha_test",
  representation(
    IC = "numeric",
    factor_z_score = "list"
  )
)

# alpha_test_factor_q (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Quantile) S4 Object
#' @slot factor_quantile A list of factor quantiles.
#' @slot q_returns A list of factor quantile returns.
#' @slot q_stats A list of factor quantile stats.
setClass(
  "alpha_test_factor_q",
  contains = "alpha_test",
  representation(
    factor_quantile = "list",
    q_returns = "list",
    q_stats = "list"
  )
)

#' @include generic_methods.R
#' @include alpha_test_single_period.R
#' @include factor_data.R
setMethod("alpha_test",
  signature(
    data = "factor_data",
    .settings = "at_settings_factor_z"
  ),
  function(data, .settings, ...) {
    spats <- lapply(
      data@factor_data,
      alpha_test,
      .setting = .settings
    )

    returns <- sapply(spats, \(x) x@return_factor)
    returns_bmark <- sapply(spats, \(x) x@return_bmark)
    weights <- lapply(spats, \(x) x@weights)
    weights_bmark <- lapply(spats, \(x) x@weights)

    ic <- sapply(spats, \(x) x@IC)
    factor_z_score <- lapply(spats, \(x) x@factor_z_score)

    new("alpha_test_factor_z",
      returns = returns,
      return_bmark = returns_bmark,
      weights = weights,
      weights_bmark = weights_bmark,
      IC = ic,
      factor_z_score = factor_z_score
    )
  }
)

#' @include generic_methods.R
#' @include alpha_test_single_period.R
#' @include factor_data.R
setMethod("alpha_test",
  signature(
    data = "factor_data",
    .settings = "at_settings_factor_q"
  ),
  function(data, .settings, ...) {
    spats <- lapply(
      data@factor_data,
      alpha_test,
      .setting = .settings
    )

    returns <- sapply(spats, \(x) x@return_factor)
    returns_bmark <- sapply(spats, \(x) x@return_bmark, simplify = TRUE)
    weights <- lapply(spats, \(x) x@weights)
    weights_bmark <- lapply(spats, \(x) x@weights)

    factor_quantile <- lapply(spats, \(x) x@factor_quantile)
    q_returns <- lapply(spats, \(x) x@q_returns)
    q_stats <- lapply(spats, \(x) x@q_stats)

    new("alpha_test_factor_q",
      returns = returns,
      return_bmark = returns_bmark,
      weights = weights,
      weights_bmark = weights_bmark,
      factor_quantile = factor_quantile,
      q_returns = q_returns,
      q_stats = q_stats
    )
  }
)

##' @title Get Factor Score Data
##' @description Get Factor Score Data as new Factor Data from Alpha Test
##' @param alpha_test alpha_test object
#get_score_data <- function(alpha_test) UseMethod("get_score_data")

