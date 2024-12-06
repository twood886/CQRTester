# alpha_test_factor_q (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Quantile) S4 Object
#' @slot factor_quantile A list of factor quantiles.
#' @slot q_returns A list of factor quantile returns.
#' @slot q_stats A list of factor quantile stats.
#' @include alpha_test.R
#' @include at_data.R
setClass(
  "alpha_test_q",
  contains = "at",
  representation(
    factor_quantile = "orderedList",
    q_avg_fwd_returns = "list",
    q_lag_factor_avg_return = "list"
  )
)

#' @include alpha_test.R
#' @include alpha_test_single_period.R
setMethod("alpha_test",
  signature(
    data = "at_data",
    .settings = "at_settings_factor_q"
  ),
  function(data, .settings, ...) {

    .pivSlot <- function(x, slotname, dates = NULL, simplify = TRUE) {
      lol <- lapply(x, \(x) slot(x, slotname))
      stackorderedLists(lol, dates)
    }

    spats <- lapply(
      data@alpha_data,
      alpha_test,
      .setting = .settings
    )

    dates <- as.Date(sapply(spats, \(x) x@date), origin = "1970-01-01")

    return_fwd_factor <- .pivSlot(spats, "return_fwd_factor", dates)
    return_lag_factor <- .pivSlot(spats, "return_lag_factor", dates)
    return_bmark <- .pivSlot(spats, "return_bmark", dates)
    
    weights <- .pivSlot(spats, "weights", dates, simplify = FALSE)
    weights_bmark <- .pivSlot(spats, "weights_bmark", dates, simplify = FALSE)
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