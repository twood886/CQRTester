#' @title Calculate Weighted Return from Weights & Returns
#' @description Function to calculate the portfolio return from Weights
#'  and Returns
#' @param weights An array of weights or orderedList of weights
#' @param returns An array of returns or orderedList of returns
#' used to calculate the portfolio weights.
calc_return <- function(weights, returns, ...) UseMethod("calc_weights")

#' @importFrom tidyr replace_na
setMethod("calc_return",
  signature(
    weights = "numeric",
    returns = "numeric"
  ),
  function(weights, returns) {
    w <- tidyr::replace_na(weights, 0)
    r <- tidyr::replace_na(returns, 0)
    as.numeric(w %*% r)
  }
)

#' @include orderedList.R
setMethod("calc_return",
  signature(
    weights = "numeric",
    returns = "orderedList"
  ),
  function(weights, returns) {
    orderedList(
      lapply(returns@list, calc_return, weights = weights),
      returns@n,
      returns@order
    )
  }
)

#' @include orderedList.R
setMethod("calc_return",
  signature(
    weights = "orderedList",
    returns = "numeric"
  ),
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
