#' @title Factor Z-Score (S4 Object)
#' @description An S4 Class to represent Factor Z-Scores
#' @slot score Factor Z Scores
setClass(
  "factor_z_score",
  representation(score = "numeric")
)

#' @title Calculate Z-Score Factor Value
#' @description Calculate Factor Z-Score
#' @param factor_values a numeric array of factor values
#' @param win_prob a numeric vector of probabilities with values in [0,1]
#' @return A vector of the same length as the original data x containing the
#' winsorized and normalized data.
calc_factor_z <- function(factor_data, win_prob = c(0, 1)) UseMethod("calc_factor_z") # nolint: line_length_linter.
#' @export
calc_factor_z.single_period_factor_data <- function(x, win_prob = c(0, 1)) {
  fz <- ctz(x@fvals, x@weights, x@group, win_prob)
  new("factor_z_score", score = fz)
}

#' @title Calculate Factor Z-Score with Winsorization
#' @description Function to calculate normalized value with windsorization.
#' @details Used in Alpha Testing Functions
#' @param values a numeric vector of factor values
#' @param weights a numeric vector of weights.
#' @param group a character vector of grouping.
#' @param win.prob numeric vector of probabilities with values in [0,1]
#' as used in quantile.
#' @return ord
ctz <- function(values, weights, group, win_prob = c(0, 1)) {
  win_values <- windsorize(values, group, win_prob, na.rm = TRUE)
  means <- grouped_weighted_mean(values, weights, group)
  stds <- grouped_weighted_sd(values, weights, group)
  norm_x <- (win_values - means) / stds
  norm_x
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

windsorize <- function(values, group, probs = c(0.05, 0.95), ...) {
  windsorize_ind <- function(x, g, v, gr, probs = c(0.05, 0.95), ...) {
    if (is.na(x)) {
      return(NA)
    }
    v <- v[which(gr == g)]
    xq <- quantile(x = v, probs = probs, ...)
    minval <- xq[[1L]]
    maxval <- xq[[2L]]

    if (x < minval) {
      x <- minval
    } else if (x > maxval) {
      x <- maxval
    }
    x
  }

  mapply(
    windsorize_ind,
    x = values,
    g = group,
    MoreArgs = list(
      v = values,
      gr = group,
      probs = probs,
      ... = ...
    )
  )
}
