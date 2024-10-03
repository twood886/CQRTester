# alpha_test_factor_z (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Z-Score) S4 Object
#' @slot IC Information Coefficient
#' @slot factor_z_score A list of factor z-scores.
#' @keywords internal
#' @include alpha_test.r
setClass(
  "alpha_test_factor_z",
  contains = "alpha_test",
  representation(
    IC = "matrix",
    factor_z_score = "list"
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

    returns <- t(sapply(spats, \(x) x@return_factor))
    returns_bmark <- t(sapply(spats, \(x) x@return_bmark))
    weights <- lapply(spats, \(x) x@weights)
    weights_bmark <- lapply(spats, \(x) x@weights)

    ic <- t(sapply(spats, \(x) x@IC))
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