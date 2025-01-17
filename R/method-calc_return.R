#' @title Calculate Returns Based on Weights and Returns Data
#' @description
#' Computes the weighted return for portfolios using the given weights and
#' returns data.
#'
#' @param weights A numeric vector or `orderedList` of portfolio weights.
#' @param returns A numeric vector or `orderedList` of asset returns.
#' @return A numeric value or `orderedList` representing portfolio returns.
#' @importFrom tidyr replace_na
#' @include class-orderedList.R
#' @include func-orderedList.R
#' @export
setGeneric("calc_return",
  function(weights, returns, ...) standardGeneric("calc_return")
)

setMethod("calc_return",
  signature(weights = "numeric", returns = "numeric"),
  function(weights, returns) {
    w <- tidyr::replace_na(weights, 0)
    r <- tidyr::replace_na(returns, 0)
    as.numeric(w %*% r)
  }
)

setMethod("calc_return",
  signature(weights = "numeric", returns = "orderedList"),
  function(weights, returns) {
    orderedList(
      lapply(returns@list, calc_return, weights = weights),
      returns@n,
      returns@order
    )
  }
)

setMethod("calc_return",
  signature(weights = "orderedList", returns = "numeric"),
  function(weights, returns) {
    r <- lapply(weights@list, calc_return, returns = returns)

    names(r) <- lapply(
      names(r),
      \(x) {
        paste0(
          "factor_",
          tidyr::replace_na(stringr::str_extract(x, "lag[0-9]+"), "lag0"),
          "_return"
        )
      }
    )
    orderedList(r, weights@n, weights@order)
  }
)