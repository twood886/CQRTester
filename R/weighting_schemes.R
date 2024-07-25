setGeneric("calc_weights",
  function(score, weighting_scheme, ...) standardGeneric("calc_weights")
)

#'@export
calc_weights <- function(score, weighting_scheme, ...) UseMethod("calc_weights")

#' @importFrom tidyr replace_na
setMethod("calc_weights",
  signature(score = "factor_z_score"),
  function(score, weighting_scheme = "z-weighted", ...) {
    fz <- score@factor_z
    total_z <- sum(abs(fz), na.rm = TRUE)
    raw_weights <- tidyr::replace_na(fz / (total_z / 2), 0)

    if (weighting_scheme == "z-weighted") {
      weights <- raw_weights
    }else if (weighting_scheme == "long-only") {
      weights <- ifelse(raw_weights > 0, raw_weights, 0)
    }else if (weighting_scheme == "short-only") {
      weights <- ifelse(raw_weights < 0, raw_weights, 0)
    }else {
      stop("Unknown Weighting Scheme for Factor Z Score")
    }

    names(weights) <- names(fz)
    weights
  }
)

#' @importFrom dplyr full_join
#' @importFrom dplyr summarise
#' @importFrom dplyr case_when
setMethod("calc_weights",
  signature(score = "factor_q_score"),
  function(score, weighting_scheme = "equal-spread", ...) {
    fq <- score@factor_q
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
