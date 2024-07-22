#' @export
#' @importFrom tidyr replace_na
zscore_weighting <- function(factor_zscore, longonly = FALSE) {
  weights <- tidyr::replace_na(
    factor_zscore / (sum(abs(factor_zscore), na.rm = TRUE) / 2),
    0
  )

  if (longonly == TRUE) {
    weights <- ifelse(weights > 0, weights, 0)
  }
  weights
}

#' @importFrom dplyr full_join
#' @importFrom dplyr summarise
quantile_return_stats <- function(quantiles, returns) {

  quantile_levels <- levels(quantiles)

  quantile_df <- data.frame(
    "id" = names(quantiles),
    "q" = quantiles
  )

  return_df <- data.frame(
    "id" = names(returns),
    "return" = returns
  )

  data_df <-
    dplyr::full_join(quantile_df, return_df, by = "id") %>%
    dplyr::mutate(`q` = factor(`q`, levels = quantile_levels, ordered = TRUE))

  q_count <- data_df %>%
    dplyr::summarise(n = n(), .by = `q`) %>%
    dfcol2array(
      .col = "n",
      .names = "q",
      .levels = quantile_levels,
      .ordered = TRUE
    )

  q_average <- data_df %>%
    dplyr::summarise(`avg` = mean(`return`, na.rm = TRUE), .by = `q`) %>%
    dfcol2array(
      .col = "avg",
      .names = "q",
      .levels = quantile_levels,
      .ordered = TRUE
    )

  list(
    "return_avg" = q_average
  )
}


dfcol2array <- function(data, .col,
  .names = NULL, .levels = NULL, .ordered = F
) {
  x <- data[[.col]]

  if (!is.null(.names)) {
    names(x) <- data[[.names]]
  }

  if (!is.null(.names) & !is.null(.levels)) {
    y <- factor(.levels, levels = .levels, ordered = .ordered)
  }

  x[order(match(names(x), y))]
}