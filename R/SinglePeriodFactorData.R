# SinglePeriodFactorData (S4 Object) --------------------------------------
#'  An S4 Class to represent Factor Data for a single period
#'
#' @slot factor A character representing the name of the factor
#' @slot date A date object representing the date of the data
#' @slot ids A character vector representing the company ids
#' @slot fvales A named numeric vector representing the factor values
#' @slot returns A names numeric vector representing the forward returns
#' @export
setClass(
  "single_period_factor_data",
  representation(
    factor = "character",
    date = "Date",
    ids = "character",
    fvals = "numeric",
    returns = "numeric"
  )
)

# SinglePeriodFactorData --------------------------------------------------
#' @title Create single_period_factor_data S4 Object
#' @description Function to create SinglePeriodFactorData object
#' @details This function converts data into SinglePeriodFactorData object.
#' @param data A data frame containing id columns, return column and factor
#' value column.
#' @param date A date object representing the date of the data.
#' @param iname A character representing the column name of identifiers.
#' @param fname A character representing the column name of the factor.
#' @param rname A character representing the column name of the returns.
#' @returns A SinglePeriodFactorData object.
#' @import methods
#' @keywords internal
#' @export
create_single_period_factor_data <- function(
  data, date, id_col_name, factor_col_name, return_col_name, ...
) {
  dargs <- list(...)

  if ("date" %in% names(dargs))
    date <- dargs$date
  if ("id_col_name" %in% names(dargs))
    id_col_name <- dargs$id_col_name
  if ("return_col_name" %in% names(dargs))
    return_col_name <- dargs$return_col_name
  if ("factor_col_name" %in% names(dargs))
    fname <- dargs$factor_col_name

  # Check if necessary inputs are included
  if (missing(data))
    stop("There is no data provided")

  if (missing(date))
    stop("No date arguement provided")

  if (missing(id_col_name))
    stop("No identifier column name provided")

  if (missing(factor_col_name))
    stop("No factor column name provided")

  if (missing(return_col_name)){
    returns <- rep(NA_real_, nrow(data))
  } else {
    returns <- data[[return_col_name]]
  }

  ids <- data[[id_col_name]]
  fvals <- data[[factor_col_name]]
  names(fvals) <- ids
  names(returns) <- ids
  new("single_period_factor_data",
    factor = fname,
    date = date,
    ids = ids,
    fvals = fvals,
    returns = returns
  )
}



# UniverseReturnStats -----------------------------------------------------
setGeneric("calc_universe_return_stats",
  function(.data) standardGeneric("calc_universe_return_stats")
)

setMethod("calc_universe_return_stats",
  signature(.data = "single_period_factor_data"),
  function(.data){
    n <- length(.data@ids)
    n_avail <- length(which(!is.na(.data@returns)))
    avg_ret <- mean(.data@returns, na.rm = TRUE)
    med_ret <- median(.data@returns, na.rm = TRUE)
    hit_rate_zero <- length(which(.data@returns > 0)) / n_avail
    hit_rate_uavg <- length(which(.data@returns > avg_ret)) / n_avail

    list(
      "n" = n,
      "n_avail" = n_avail,
      "avg_return" = avg_ret,
      "med_return" = med_ret,
      "hit_rate_zero" = hit_rate_zero,
      "hit_rate_uavg" = hit_rate_uavg
    )
  }
)


# -------------------------------------------------------------------------
calc_qtile_return_stats <- function(.object, fftile = 5, ...) UseMethod("calc_qtile_return_stats")

setMethod("calc_qtile_return_stats",
  signature(.object = "SinglePeriodFactorData"),
  function(.object, fftile)
  {
    # Calculate the Quantile of Factors
    fq <- ctq(.object@fvals, fftile)

    .SingleGroupReturnStats <- function(.object, fq, ftile){
      group_loc <- which(fq==ftile)
      group_ids <- .object@ids[group_loc]
      group_returns <- .object@returns[group_loc]
      group_n <- length(group_ids)
      group_n_avail <- length(which(!is.na(group_returns)))
      group_weights <- rep(1/group_n, group_n)
      names(group_weights) <- group_ids
      list(
        "n" = group_n,
        "n_avail" = group_n_avail,
        "avg_return" = mean(group_returns, na.rm = TRUE),
        "med_return" = median(group_returns, na.rm = TRUE),
        "hit_rate_zero" = length(which(group_returns > 0)) / group_n_avail,
        "weights" = group_weights
      )
    }

    quantile_stats <- lapply(
      levels(fq),
      .SingleGroupReturnStats,
      .Object = .object,
      fq = fq
    )
    names(quantile_stats) <- levels(fq)
    qn <- length(levels(fq)[which(levels(fq) != "NA")])
    qspread <- quantile_stats[[1]]$avg_return - quantile_stats[[qn]]$avg_return
    qspreadweights <- c(quantile_stats[[1]]$weights, quantile_stats[[qn]]$weights)
    list(
      "q_spread" = qspread,
      "q_spread.weights" = qspreadweights,
      "q_stats" = quantile_stats
    )
  }
)