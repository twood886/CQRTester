
# ATSettings (S4 Object) --------------------------------------------------
#' @title Alpha Testing Settings
#' @slot WeightingScheme Character
#' @slot Leverage leverage
#' @slot 
setClass(
  "ATSettings",
  representation(
    WeightingScheme = "character",
    Leverage = "numeric",
    
  )
)

