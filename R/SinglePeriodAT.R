# single_period_at(S4 Object) ----------------------------------------------
#' @title Single Period Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing Parent Class
#'  for a single period
#' @slot date A date object representing the date of the data
#' @slot return
#' @slot weights Portfolio Weights
#' @slot .factordata A SinglePeriodFactorData object.
#' @slot .settings Alpha Testing Settings#'
#' @include SinglePeriodFactorData.R
setClass(
  "single_period_at",
  slots = c(
    date = "Date",
    return = "numeric",
    weights = "numeric",
    .factor_data = "single_period_factor_data",
    .settings = "at_settings"
  )
)

# single_period_at_factor_w (S4 Object) ------------------------------------
#' @title Single Period Alpha Test (Factor Weighted) S4 Object
#' @slot IC todo
#' @slot factor_z_score todo
#' @slot return_z_score todo
#' @include SinglePeriodFactorData.R
setClass(
  "single_period_at_factor_w",
  contains = "single_period_at",
  representation(
    IC = "numeric",
    factor_z_score = "numeric",
    return_z_score = "numeric",
    .settings = "at_settings"
  )
)


# single_period_at_q_spread (S4 Object) ------------------------------------
#' @title Single Period Alpha Test (Quantiles) S4 Object
#' @slot factor_quantile todo
#' @slot q_returns todo
#' @slot q_stats todo
setClass(
  "single_period_at_q_spread",
  contains = "SinglePeriodAT",
  representation(
    factor_quantile = "ordered",
    q_returns = "numeric",
    q_stats = "list"
  )
)

# -------------------------------------------------------------------------
setGeneric("alpha_test",
  function(data, .settings, ...) standardGeneric("alpha_test")
)
# -------------------------------------------------------------------------

#' @include WeightingFunctions.R
setMethod("alpha_test",
  signature(
    data = "single_period_factor_data",
    .setting = "at_settings"
  ),
  function(data, .settings, ...) {
    # Extract Date
    d <- data@date
    # Calculate the Z-Score of Factors
    fz <- ctz(data@fvals, .settings@win.prob)
    # Calculate the Z-Score of Returns
    rz <- ctz(data@returns, .settings@win.prob)
    # Calculate the IC
    ic <- cor(fz, rz, use = "pairwise.complete.obs")
    
    # Calculate the weights using the ZScores
    weights <- ZscoreWeighting(fz)
    
    # Return
    r <- as.numeric(weights %*% data@returns)
    
    return(
      new("SinglePeriodAT_FactorWeighted",
          date = d,
          return = r,
          weights = weights,
          .factordata = data,
          .settings = .Settings,
          IC = IC,
          factorZscore = fz,
          returnZscore = rz))
  }
)



# -------------------------------------------------------------------------
#' @include WeightingFunctions.R
setMethod('AlphaTest',
  signature(
    data = "SinglePeriodFactorData",
    .Settings = "ATSettings_QSpread"),
  function(data, .Settings, ...){
    
    # Extract Date
    d <- data@date
    
    # Calculate the Quantile of Factors
    fq <- ctq(data@fvals, .Settings@quantiles)
    
    # Quintile Level Statistics
    # Should Change to be defined for alt weighting, TODO
    qStats <- QuantileReturnStats(.Object, fftile = .Settings@quantiles)
    
    # This needs to be fixed
    weights <- qStats@q_spread_weights
    
    r <- as.numeric(weights %*% data@returns)
    
    return(
      new("SinglePeriodAT_Quantile",
        date = d,
        sp_return = qStats$qspread,
        alpha = as.numeric(NA),
        weights = weights,
        .factordata = data,
        .settings = .Settings,
        factorQuintile = fq,
        qreturns = qStats$q_stats$avg_return,
        qstats = qStats$q_stats))
  })




#' @export
AlphaTest <- function(data, .Settings, ...) UseMethods("AlphaTest")