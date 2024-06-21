# SinglePeriodAT (S4 Object) ----------------------------------------------
#' @title Single Period Alpha Testing S4 Object
#' @description An S4 Class to represent Factor Alpha Testing Parent Class
#'  for a single period
#' @slot date A date object representing the date of the data
#' @slot return 
#' @slot alpha Relative Return of Factor over Benchmark
#' @slot weights Portfolio Weights
#' @slot .factordata A SinglePeriodFactorData object.#' 
#' @slot .settings Alpha Testing Settings#' 
#' @include SinglePeriodFactorData.R
setClass(
  "SinglePeriodAT",
  slots = c(
    date = "Date",
    sp_return = "numeric",
    alpha = "numeric",
    weights = "numeric",
    .factordata = "SinglePeriodFactorData",
    .settings = "ATSettings"))

# SinglePeriodAT_FactorWeighted (S4 Object) -------------------------------
#' @title Single Period Alpha Test (Factor Weighted) S4 Object
#' @slot IC todo
#' @slot factorZscore todo
#' @slot returnZscore todo
#' @include SinglePeriodFactorData.R
setClass(
  "SinglePeriodAT_FactorWeighted",
  contains = "SinglePeriodAT",
  representation(
    IC = "numeric",
    factorZscore = "numeric",
    returnZscore = "numeric",
    .settings = "ATSettings"))


# SinglePeriodAT_Qunatile (S4 Object) -------------------------------------
#' @title Single Period Alpha Test (Quantiles) S4 Object
#' @slot factorQuantile todo
#' @slot qreturns todo
#' @slot qstats todo
#' @include SinglePeriodFactorData.R
setClass(
  "SinglePeriodAT_Quantile",
  contains = "SinglePeriodAT",
  slots = c(
    factorQuantile = "ordered",
    qreturns = "numeric",
    qstats = "list"))

setMethod(f = 'show',
  signature = "SinglePeriodAT",
  definition = function(object){
    print("test")
  })

# -------------------------------------------------------------------------
setGeneric("AlphaTest", function(data, .Settings, ...) standardGeneric("AlphaTest"))
# -------------------------------------------------------------------------
#' @export
AlphaTest <- function(data, .Settings, ...) UseMethods("AlphaTest")


# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
#' @include WeightingFunctions.R
setMethod('AlphaTest',
  signature(
    data = "SinglePeriodFactorData", 
    .Setting = "ATSettings_FactorWeighted"),
  function(data, .Settings, ...){
    
    # Extract Date
    d <- data@date
    
    # Calculate the Z-Score of Factors
    fz <- ctz(data@fvals, .Settings@win.prob)
    
    # Calculate the Z-Score of Returns
    rz <- ctz(data@returns, .Settings@win.prob)
    
    # Calculate the IC
    IC <- cor(fz, rz, use = "pairwise.complete.obs")
    
    # Weights
    weights <- ZscoreWeighting(fz)
    
    # Return
    r <- as.numeric(weights %*% data@returns)
    
    return(
      new("SinglePeriodAT_FactorWeighted",
        date = d,
        sp_return = r,
        alpha = as.numeric(NA),
        weights = weights,
        .factordata = data,
        .settings = .Settings,
        IC = IC,
        factorZscore = fz,
        returnZscore = rz))
  })

# -------------------------------------------------------------------------
setMethod('AlphaTest',
  signature(
    data = "SinglePeriodFactorData",
    .Settings = "ATSettings_Quantile"),
  function(data, .Settings, ...){
    
    # Extract Date
    d <- data@date
    
    # Calculate the Quantile of Factors
    fq<-ctq(data@fvals, .Settings@quantiles)
    
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