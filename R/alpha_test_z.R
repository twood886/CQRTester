# alpha_test_factor_z (S4 Object) ------------------------------------------
#' @title Alpha Test (Factor Z-Score) S4 Object
#' @slot IC Information Coefficient
#' @slot factor_z_score A list of factor z-scores.
#' @include alpha_test.R
#' @include at_data.R
setClass(
  "alpha_test_z",
  contains = "at",
  representation(
    IC_fwd_return = "orderedList",
    IC_lag_factor = "orderedList",
    alpha_fwd_return = "orderedList",
    alpha_lag_factor = "orderedList"
  )
)

#' @include alpha_test.R
#' @include alpha_test_single_period.R
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
    alpha_fwd_return <- .pivSlot(spats, "alpha_fwd_return", dates)
    alpha_lag_factor <- .pivSlot(spats, "alpha_lag_factor", dates)

    new("alpha_test_z",
      return_fwd_factor = return_fwd_factor,
      return_lag_factor = return_lag_factor,
      return_bmark = return_bmark,
      weights = weights,
      weights_bmark = weights_bmark,
      IC_fwd_return = IC_fwd_return,
      IC_lag_factor = IC_lag_factor,
      alpha_fwd_return = alpha_fwd_return,
      alpha_lag_factor = alpha_lag_factor
    )
  }
)



# Summary Method --------------------------------------------------------------
#setMethod(f = summary,
#  signature(object = "alpha_test_factor_z"),
#  function(object, perscale = 1) {
#
#    alpha_avg_ann_return <- (
#      prod(1 + object@alpha_fwd_return[[1]]) ^
#        (perscale / length(object@alpha_fwd_return[[1]])) - 1
#    )
#
#    alpha_t_test <- orderedList(
#      lapply(object@alpha@list, t.test),
#      object@alpha@n,
#      object@alpha@order
#    )
#
#    alpha_avg <- orderedList(
#      lapply(object@alpha@list, \(x) mean(x, na.rm = TRUE)),
#      object@alpha@n,
#      object@alpha@order
#    )
#
#    alpha_sd <- orderedList(
#      lapply(object@alpha@list, \(x) sd(x, na.rm = TRUE)),
#      object@alpha@n,
#      object@alpha@order
#    )
#
#    alpha_n <- orderedList(
#      lapply(object@alpha@list, \(x) length(x)),
#      object@alpha@n,
#      object@alpha@order
#    )
#
#    alpha_t <- mapply(
#      \(m, s, n) m / (s / sqrt(n)),
#      alpha_avg,
#      alpha_sd,
#      alpha_n
#    )
#  }
#)

# Show Method -----------------------------------------------------------------
#setMethod(f = show,
#  signature(object = "alpha_test_factor_z"),
#  function(object) {
#  }
#)