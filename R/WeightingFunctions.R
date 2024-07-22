dfcol2array <- function(data, .col,
  .names = NULL, .levels = NULL, .ordered = FALSE
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
