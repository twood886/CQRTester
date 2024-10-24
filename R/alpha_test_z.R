# alpha_test_factor_z (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Z-Score) S4 Object
#' @slot IC Information Coefficient
#' @slot factor_z_score A list of factor z-scores.
#' @keywords internal
#' @include alpha_test.R
#' @include at_data.R
setClass(
  "alpha_test_factor_z",
  contains = "alpha_test",
  representation(
    IC_fwd_return = "orderedList",
    IC_lag_factor = "orderedList",
    alpha = "orderedList"
  )
)

#' @include generic_methods.R
#' @include alpha_test_single_period.R
#' @include factor_data.R
#' @include orderedList.R
setMethod("alpha_test",
  signature(
    data = "at_data",
    .settings = "at_settings_factor_z"
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

    IC_fwd_return <- .pivSlot(spats, "IC_fwd_return", dates)
    IC_lag_factor <- .pivSlot(spats, "IC_lag_factor", dates)
    alpha <- .pivSlot(spats, "alpha", dates)

    new("alpha_test_factor_z",
      return_fwd_factor = return_fwd_factor,
      return_lag_factor = return_lag_factor,
      return_bmark = return_bmark,
      weights = weights,
      weights_bmark = weights_bmark,
      IC_fwd_return = IC_fwd_return,
      IC_lag_factor = IC_lag_factor,
      alpha = alpha
    )
  }
)

# Show Method -------------------------------------------------------------
setMethod(f = show,
  signature(object = "alpha_test_factor_z"),
  function(object) {
  }
)