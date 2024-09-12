#' @title Factor Quantile (S4 Object)
#' @description An S4 Class to represent Factor Quantiles
#' @slot score Factor Quantile
setClass(
  "factor_q_score",
  representation(score = "ordered")
)

#' @title Calculate Quantile Factor Value
#' @description Calculate Factor Z-Score
#' @param factor_values a numeric array of factor values
#' @param quantiles a numeric representing the number of quantiles.
#' @return Ordered quantiles
calc_factor_q <- function(factor_data, quantiles = 5) UseMethod("calc_factor_q")
#' @export
calc_factor_q.single_period_factor_data <- function(x, quantiles = 5, .desc = TRUE) { # nolint: line_length_linter.
  fq <- ctq(x@fvals, x@group, quantiles, .desc)
  new("factor_q_score", score = fq)
}

#' @title Calculate Factor Quantile
#' @param values a numeric vector of factor values.
#' @param group a character vector of groups.
#' @param quantiles integer representing the number of quantiles
#' to split data into.
#' @param desc Should values be ranked in secending order?
#' @return company quantile based on factor value
#' @import tidyverse
#' @import DescTools
#' @importFrom ggplot2 cut_interval
#' @import forcats
ctq <- function(values, group, quantiles = 3, .desc = TRUE) {
  qs <- mapply(
    quantile_ind,
    x = values,
    x_name = names(values),
    MoreArgs = list(
      v = values,
      gr = group,
      q = quantiles,
      .desc = .desc
    ),
    USE.NAMES = FALSE
  )

  labels <- gettextf("Q%s", quantiles:1)
  ftile <- factor(
    ggplot2::cut_interval(qs, n = quantiles, labels = labels),
    ordered = TRUE
  )
  ftile <- forcats::fct_na_value_to_level(ftile, level = "NA")
  names(ftile) <- names(values)
  ftile
}

quantile_ind <- function(x, x_name, v, gr, q, .desc) {
  if (is.na(x)) {
    out <- NA
    names(out) <- x_name
    return(out)
  }
  g <- gr[which(names(v) == x_name)]
  v <- v[which(gr == g)]
  if (.desc == FALSE) {
    v <- v * -1
  }
  b <- sum(!is.na(unique(v)))
  if (b >= q) {
    qs <- round(
      rank(v, na.last = "keep") / sum(!is.na(v)) / (1 / q) + .4999
    )
    qs <- ifelse(qs < 1, 1, qs)
    out <- qs[which(names(qs) == x_name)]
  }else {
    out <- NA
    names(out) <- x_name
  }
  out
}
