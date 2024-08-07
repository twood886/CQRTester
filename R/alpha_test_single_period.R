# single_period_at(S4 Object) ----------------------------------------------
#' @title Single Period Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing Parent Class
#'  for a single period
#' @slot date A date object representing the date of the data
#' @slot return portfolio return
#' @slot return_bmark benchmark return
#' @slot weights Portfolio Weights
#' @slot weights_bmark benchmark weights
#' @include factor_data_single_period.R
setClass(
  "single_period_at",
  slots = c(
    date = "Date",
    return_factor = "numeric",
    return_bmark = "numeric",
    weights = "numeric",
    weights_bmark = "numeric"
  )
)

# single_period_at_factor_w (S4 Object) ------------------------------------
#' @title Single Period Alpha Test (Factor Weighted) S4 Object
#' @slot IC todo
#' @slot factor_z_score todo
#' @slot return_z_score todo
#' @include factor_data_single_period.R
setClass(
  "single_period_at_factor_z",
  contains = "single_period_at",
  representation(
    IC = "numeric",
    factor_z_score = "factor_z_score",
    return_z_score = "numeric"
  )
)


# single_period_at_q_spread (S4 Object) ------------------------------------
#' @title Single Period Alpha Test (Quantiles) S4 Object
#' @slot factor_quantile todo
#' @slot q_returns todo
#' @slot q_stats todo
#' @include factor_data_single_period.R
setClass(
  "single_period_at_factor_q",
  contains = "single_period_at",
  representation(
    factor_quantile = "factor_q_score",
    q_returns = "numeric",
    q_stats = "list"
  )
)


# -------------------------------------------------------------------------
#' @include generic_methods.R
#' @include at_settings.R
#' @include factor_z_score.R
#' @include calc_weights.R
setMethod("alpha_test",
  signature(
    data = "single_period_factor_data",
    .setting = "at_settings_factor_z"
  ),
  function(data, .settings, ...) {
    # Extract Date
    d <- data@date
    # Calculate the Z-Score of Factors
    fz <- calc_factor_z(data, .settings@win_prob)
    # Calculate the Z-Score of Returns
    rz <- ctz( # nolint
      values = data@returns,
      weights = data@weights,
      group = data@group,
      win_prob = .settings@win_prob
    )
    # Calculate the IC
    ic <- cor(fz@score, rz, use = "pairwise.complete.obs")
    # Calculate the weights using the ZScores
    weights <- calc_weights(fz, .settings@weighting_scheme)
    # Return
    r <- as.numeric(weights %*% data@returns)
    weights_bmark <- data@weights / sum(data@weights, na.rm = TRUE)
    r_bmark <- as.numeric(weights_bmark %*% data@returns)

    new("single_period_at_factor_z",
      date = d,
      return_factor = r,
      return_bmark = r_bmark,
      weights = weights,
      weights_bmark = weights_bmark,
      IC = ic,
      factor_z_score = fz,
      return_z_score = rz
    )
  }
)

# -------------------------------------------------------------------------
#' @include generic_methods.R
#' @include at_settings.R
#' @include factor_q_score.R
#' @include calc_weights.R
setMethod("alpha_test",
  signature(
    data = "single_period_factor_data",
    .settings = "at_settings_factor_q"
  ),
  function(data, .settings, ...) {
    # Extract Date
    d <- data@date
    # Calculate the Quantile of Factors
    fq <- calc_factor_q(data, .settings@quantiles)
    # Quintile Level Statistics
    # Should Change to be defined for alt weighting, TODO
    #quantile_return_stats(fq@factor_q, returns = data@returns) # nolint
    q_stats <- list(NULL)
    weights <- calc_weights(fq, .settings@weighting_scheme)
    # returns
    r <- as.numeric(weights %*% data@returns)
    new("single_period_at_factor_q",
      date = d,
      return_factor = r,
      weights = weights,
      factor_quantile = fq,
      q_returns = as.numeric(q_stats$avg_return),
      q_stats = q_stats
    )
  }
)
