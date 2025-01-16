#' @importFrom tidyr replace_na
#' @include gen-calc_return.R
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

#' @include class-orderedList.R
#' @include func-orderedList.R
#' @include gen-calc_return.R
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

#' @include class-orderedList.R
#' @include func-orderedList.R
#' @include gen-calc_return.R
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