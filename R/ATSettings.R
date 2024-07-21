# at_settings (S4 Object) --------------------------------------------------
#' @title Alpha Testing Settings
#' @description
#' An S4 Object containing the settings to be used in Alpha Testing. Arguments
#' passed through ATSettings are used in designing the alpha testing procedure.
#' @slot start_date Date representing the earliest date for Alpha Testing.
#' @slot end_date Date representing the latest date for Alpha Testing.
#' @slot testing_scheme Character representing alpha testing procedure.
#' @slot long_only A logical representing if portfolio should be long only.
setClass(
  "at_settings",
  representation(
    start_date = "Date",
    end_date = "Date",
    testing_scheme = "character",
    long_only = "logical",
  )
)

# at_settings_factor_w (S4 Object) -----------------------------------------
#' @title Settings for Factor Weighted Alpha Testing
#' @slot win_prob A numeric vector of length 2 representing the percentile
#'cut-offs for windsorization.
setClass(
  "at_settings_factor_w",
  contains = "at_settings",
  slots = c(win_prob = "numeric")
)

#' @title Factor Weighted Alpha Testing Settings
#' @description
#' A function to create Alpha Testing settings for a factor weighted Alpha Test.
#' @param start_date description
#' @param end_date description
#' @param win_prob A numeric vector of length 2 representing the percentile
#' @param long_only A logical if only long positions should be used.
#' cut-offs for windsorization.
#' @returns An at_settings_factor_w S4 object to be used in Alpha Testing.
set_at_settings_factor_w <- function(
    start_date = as.Date("1901-01-01"), end_date = Sys.Date(),
    win_prob = c(0, 1), long_only = FALSE) {
  new("at_settings_factor_w",
    start_date = start_date,
    end_date = end_date,
    testing_scheme = "factorw",
    win_prob = win_prob,
    long_only = long_only
  )
}

# at_settings_q_spread (S4 Object) -----------------------------------------
#' @title Settings for Quantile Spread Alpha Testing
#' @slot quantiles Number of Quantiles to use.
setClass(
  "at_settings_q_spread",
  contains = "at_settings",
  slots = c(quantiles = "numeric")
)

#' @title Quantile Spread Alpha Testing Settings
#' @description
#' A function to create Alpha Testing settings for a Quantile Spread Alpha Test.
#' @param start_date description
#' @param end_date description
#' @param quantiles description
#' @param long_only A logical representing whether or not the portfolio weights
#' should be long only (True) or Long-Short (False)
#' @returns An at_settings_q_spread S4 object to be used in Alpha Testing.
set_at_settings_q_spread <- function(
    start_date = as.Date("1901-01-01"), end_date = Sys.Date(),
    quantiles = 5, long_only = FALSE) {
  new("at_settings_q_spread",
    start_date = start_date,
    end_date = end_date,
    testing_scheme = "qspread",
    quantiles = quantiles,
    long_only = long_only
  )
}

# set_at_settings Function -----------------------------------------------------
#' @title Create Alpha Testing Settings
#' @description
#' This function creates an "at_settings" class with arguments to be used when
#' testing factors
#' @param start_date A date object representing the earliest start date to be
#' used when performing alpha testing. Factor data is filtered to start at or
#' after provided Start.Date argument.
#' @param end_date A date object representing the last date to be
#' used when performing alpha testing. Factor data is filtered to end at or
#' before provided End.Date argument.
#' @param testing_scheme Weighting scheme to be used in testing factor. 
#' Currently supports "Factor" and "Quantile"
#' @param long_only add
#' @param ... Additional settings to be passed based on Weighting.Scheme
#' @export
set_at_settings <- function(
    start_date = as.Date("1901-01-01"), end_date = Sys.Date(), 
    testing_scheme = "factorw", long_only = FALSE, ...) {

  dargs <- list(...)
  known_schemes <- c("Factor", "Quantile")

  if (!testing_scheme %in% known_schemes) {
    stop("Testing Scheme must be one of \"Factor\" or \"QSpread\".")
  }

  if (testing_scheme == "Factor") {
    if ("win_prob" %in% names(dargs)) {
      win_prob <- dargs$win_prob
    }else {
      win_prob <- c(0, 1)
    }
    set_at_settings_factor_w(start_date, end_date, win_prob, long_only)
  }

  if (testing_scheme == "Quantile") {
    if ("quantiles" %in% names(dargs)) {
      quantiles <- dargs$quantiles
    }else {
      quantiles <- 5
    }
    set_at_settings_q_spread(start_date, end_date, quantiles, long_only)
  }
}



# Show Method -------------------------------------------------------------
setMethod(f = "show",
  signature(object = "at_settings_factor_w"),
  function(object) {
    out <- paste(
      paste("----- Alpha Testeing Settings -----"),
      paste("Testing Scheme:", object@testing_scheme, sep = "\t\t"),
      paste("Start Date:", format(object@start_date, "%b %d,%Y"), sep = "\t\t"),
      paste("End Date:", format(object@end_date, "%b %d,%Y"), sep = "\t\t"),
      paste("Windsorization:",
        paste(
          paste0(format(object@win_prob[1] * 100, digits = 2), "%"),
          paste0(format(object@win_prob[2] * 100, digits = 2), "%"),
          sep = " - "
        ),
        sep = "\t\t"
      ),
      sep = "\n"
    )
    cat(out)
  }
)

setMethod(f = "show",
  signature(object = "at_settings_q_spread"),
  function(object) {
    out <- paste(
      paste("----- Alpha Testing Settings -----"),
      paste("Testing Scheme:", object@testing.scheme, sep = "\t\t"),
      paste("Start Date:", format(object@start.date,"%b %d,%Y"), sep = "\t\t"),
      paste("End Date:", format(object@end.date, "%b %d,%Y"), sep = "\t\t"),
      paste("Factor Quantiles:", object@quantiles, sep = "\t"),
      sep = "\n"
    )
    cat(out)
  }
)