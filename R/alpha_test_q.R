# alpha_test_factor_q (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Quantile) S4 Object
#' @slot factor_quantile A list of factor quantiles.
#' @slot q_returns A list of factor quantile returns.
#' @slot q_stats A list of factor quantile stats.
#' @include alpha_test.R
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
    .settings = "at_settings_factor_q"
  ),
  function(data, .settings, ...) {
    # Calculate Single Period Alpha Tests
    spats <- lapply(
      data@factor_data,
      alpha_test,
      .setting = .settings
    )

    returns <- t(sapply(spats, \(x) x@return_factor))
    returns_bmark <- t(sapply(spats, \(x) x@return_bmark, simplify = TRUE))
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