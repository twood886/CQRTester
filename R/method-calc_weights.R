#' @include gen-calc_weights.R
#' @include class-orderedList.R
#' @include func-orderedList.R
setMethod("calc_weights",
  signature(score = "orderedList"),
  function(score, weighting_scheme = "z-weighted", ...) {
    weights <- lapply(
      score@list,
      calc_weights,
      weighting_scheme = weighting_scheme,
    )
    orderedList(weights, score@n, score@order)
  }
)

#' @title Calculate Portfolio Weights
#' @description
#' Computes portfolio weights based on factor scores and a specified weighting
#' scheme.
#'
#' @param score A factor score object (either `factor_z_score` or
#'   `factor_q_score`).
#' @param weighting_scheme A string representing the weighting scheme to use.
#' @return A numeric vector of weights.
#' @importFrom tidyr replace_na
#' @include class-factor_z_score.R
#' @include gen-calc_weights.R
#' @export
setMethod(
  "calc_weights",
  signature(
    score = "factor_z_score"
  ),
  function(score, weighting_scheme = "z-weighted", ...) {
    fz <- score@score
    total_z <- sum(abs(fz), na.rm = TRUE)
    raw_weights <- tidyr::replace_na(fz / (total_z / 2), 0)

    weights <- switch(
      weighting_scheme,
      "z-weighted" = raw_weights,
      "long-only" = ifelse(raw_weights > 0, raw_weights, 0),
      "short-only" = ifelse(raw_weights < 0, raw_weights, 0),
      stop("Unknown weighting scheme for Factor Z Scores")
    )

    names(weights) <- names(fz)
    weights
  }
)

#' @include class-factor_q_score.R
#' @include gen-calc_weights.R
#' @importFrom dplyr full_join
#' @importFrom dplyr summarise
#' @importFrom dplyr case_when
#' @export
setMethod("calc_weights",
  signature(score = "factor_q_score"),
  function(score, weighting_scheme = "equal-spread", ...) {
    fq <- score@score
    if (length(which(fq == "NA")) == length(fq)) {
      weights <- rep(0, length(fq))
      names(weights) <- names(fq)
      return(weights)
    }

    if (weighting_scheme == "equal-spread") {
      weights <- dplyr::case_when(
        fq == "Q1" ~ 1 / length(which(fq == "Q1")),
        fq == min(fq) ~ -1 / length(which(fq == min(fq))),
        .default = 0
      )
    }else if (weighting_scheme == "equal-long-only") {
      weights <- dplyr::case_when(
        fq == "Q1" ~ 1 / length(which(fq == "Q1")),
        .default = 0
      )
    }else if (weighting_scheme == "equal-short-only") {
      weights <- dplyr::case_when(
        fq == min(fq) ~ -1 / length(which(fq == min(fq))),
        .default = 0
      )
    }else {
      stop("Unknown Weighting Scheme for Factor Quantiles")
    }

    names(weights) <- names(fq)
    weights
  }
)