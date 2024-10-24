setGeneric("alpha_test",
  function(data, .settings, ...) standardGeneric("alpha_test")
)


#' @title Alpha Test
#' @description Run Alpha Test on factor data with at_settings
#' @param data factor_data or single_period_factor_data object
#' @param .settings factor_data_params object
#' @return alpha_test or single_period_at object
#' @include generic_methods.R
#' @export
alpha_test <- function(data, .settings, ...) UseMethod("alpha_test")