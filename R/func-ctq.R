#' @title Calculate Factor Quantile
#' @param values a numeric vector of factor values.
#' @param group a character vector of groups.
#' @param quantiles integer representing the number of quantiles
#' to split data into.
#' @param desc Should values be ranked in secending order?
#' @return company quantile based on factor value
#' @import tidyverse
#' @import DescTools
#' @import forcats
#' @import data.table
#' @include func-custom_cut_interval.R
ctq <- function(values, group = NA_real_, quantiles = 3, .desc = TRUE) {
  dt <- data.table::data.table(values = values, group = group)

  if (.desc) {
    dt[, values := -values]
  }

  dt[,
    quantile := custom_cut_interval(
      values,
      n = quantiles,
      labels = paste0("Q", quantiles:1),
      right = TRUE
    ),
    by = group
  ]

  dt[, quantile := forcats::fct_na_value_to_level(quantile, level = "NA")]
  dt[, quantile := factor(quantile, ordered = TRUE)]
  out <- dt$quantile
  names(out) <- names(values)
  return(out)
}