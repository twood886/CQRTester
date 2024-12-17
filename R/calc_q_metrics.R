#' @include factor_q_score.R
calc_q_returns <- function(factor_q, returns, f, ...) UseMethod("calc_q_returns") #nolint

#' @include orderedList.R
setMethod("calc_q_returns",
  signature(
    factor_q = "factor_q_score",
    returns = "orderedList"
  ),
  function(factor_q, returns, f, ...) {
    fwd_returns_q <- lapply(
      returns[[]],
      \(x, fq) tapply(x, fq, f, ...),
      fq = factor_q@score
    )
    orderedList(fwd_returns_q, returns@n, returns@order)
  }
)

#' @include orderedList.R
setMethod("calc_q_returns",
  signature(
    factor_q = "orderedList",
    returns = "numeric"
  ),
  function(factor_q, returns, f,  ...) {
    fwd_returns_q <- lapply(
      factor_q[[]],
      \(q, x) tapply(x, q@score, f, ...),
      x = returns
    )
    orderedList(fwd_returns_q, factor_q@n, factor_q@order)
  }
)
