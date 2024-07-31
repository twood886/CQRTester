#' @title Convert Data Array to Quantiles
#' @description Function to calculate quantiles.
#' @details Used in Alpha Testing Functions
#' @param x a numeric array to be quantiled
#' @param fftile integer number of fractiles to use in splitting data
#' @return ftile
#' @import tidyverse
#' @import DescTools
#' @import ggplot2
#' @import forcats
#' @export
ctq <- function(x, fftile){
  b <- sum(!is.na(unique(x)))
  labels <- gettextf("Q%s", fftile:1)
  if (b >= fftile) {
    xnames <- names(x)
    qs <- round(
      rank(x, na.last = "keep") / sum(!is.na(x)) / (1 / fftile) + .4999
    )
    qs <- ifelse(qs < 1, 1, qs)
    ftile <- factor(
      ggplot2::cut_interval(qs, n = fftile, labels = labels),
      ordered = TRUE
    )
    ftile <- forcats::fct_na_value_to_level(ftile, level = "NA")
    names(ftile) <- xnames
  }else {
    ftile <- factor(
      rep(NA, times = length(x)),
      levels = labels,
      ordered = TRUE
    )
    ftile <- forcats::fct_na_value_to_level(ftile, level = "NA")
  }
  return(ftile)
}

#' @title Calculate Factor Z-Score with Winsorization
#' @description Function to calculate normalized value with windsorization.
#' @details Used in Alpha Testing Functions
#' @param values a numeric vector to be winsorized and normalized.
#' @param weights a numeric vector of weights.
#' @param group a character vector of grouping.
#' @param win.prob numeric vector of probabilities with values in [0,1]
#' as used in quantile.
#' @return ord
#' @import tidyverse
#' @import DescTools
#' @export
ctz <- function(values, weights, group, win_prob = c(0, 1)) {
  win_values <- grouped_windsorization(values, group, win_prob)
  means <- grouped_weighted_mean(values, weights, group)
  stds <- grouped_weighted_sd(values, weights, group)
  norm_x <- (win_values - means) / stds
  return(norm_x)
}



grouped_weighted_mean <- function(values, weights, group) {
  ind_grouped_weighted_mean <- function(group_x, values, weights, group) {
    avail_values <- values[which(!is.na(values))]
    weights <- ifelse(is.na(weights), 0, weights)
    avail_weights <- weights[which(!is.na(values))]
    avail_group <- group[which(!is.na(values))]
    g_values <- avail_values[which(avail_group == group_x)]
    g_weights <- avail_weights[which(avail_group == group_x)]
    sum(g_values * g_weights) / sum(g_weights)
  }
  sapply(
    group,
    ind_grouped_weighted_mean,
    values = values, weights = weights, group = group
  )
}

grouped_weighted_sd <- function(values, weights, group) {
  ind_grouped_weighted_sd <- function(group_x, values, weights, group) {
    avail_values <- values[which(!is.na(values))]
    weights <- ifelse(is.na(weights), 0, weights)
    avail_weights <- weights[which(!is.na(values))]
    avail_group <- group[which(!is.na(values))]
    g_values <- avail_values[which(avail_group == group_x)]
    g_weights <- avail_weights[which(avail_group == group_x)]

    avg <- sum(g_values * g_weights) / sum(g_weights)
    dof <- (length(g_weights) - 1) / length(g_weights)
    var <- sum(g_weights * (g_values - avg)^2) / (dof * sum(g_weights))
    sqrt(var)
  }
  sapply(
    group,
    ind_grouped_weighted_sd,
    values = values, weights = weights, group = group
  )
}

grouped_windsorization <- function(
  values, group, probs = c(0.05, 0.95), na.rm = TRUE, type = 7
) {

  ind_grouped_windsorization <- function(x, g, v, gr,
    probs = c(0.05, 0.95), na.rm = TRUE, type = 7
  ) {
    g_values <- v[which(gr == g)]
    xq <- quantile(x = g_values, probs = probs, na.rm = na.rm, type = type)
    minval <- xq[[1L]]
    maxval <- xq[[2L]]

    if (is.na(x)) {
      x <- NA
    }else if (x < minval) {
      x <- minval
    } else if (x > maxval) {
      x <- maxval
    }
    return(x)
  }

  mapply(
    ind_grouped_windsorization,
    x = values,
    g = group,
    MoreArgs = list(
      v = values,
      gr = group,
      probs = probs,
      na.rm = na.rm,
      type = type
    )
  )
}
