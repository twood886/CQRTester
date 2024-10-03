# single_period_factor_data (S4 Object) ------------------------------------
#' @title Single Period Factor Data (S4 Object)
#' @description An S4 Class to represent Factor Data for a single period.
#' @slot factor A character representing the name of the factor.
#' @slot date A date object representing the date of the data.
#' @slot ids A character vector representing the company ids.
#' @slot fvales A named numeric vector representing the factor values.
#' @slot returns A list of named numeric vector representing the forward returns.
#' @slot group A named character vector representing the company grouping.
#' @slot weights A named numeric vector of weights from factor values.
setClass(
  "single_period_factor_data",
  representation(
    factor = "character",
    date = "Date",
    ids = "character",
    fvals = "numeric",
    returns = "list",
    group = "character",
    weights = "numeric"
  )
)

# SinglePeriodFactorData --------------------------------------------------
#' @title Create single_period_factor_data S4 Object
#' @description Function to create SinglePeriodFactorData object
#' @details This function converts data into SinglePeriodFactorData object.
#' @param data A data frame containing id columns, return column and factor
#' value column.
#' @param date_col_name A character representing the column name of dates.
#' @param id_col_name A character representing the column name of identifiers.
#' @param factor_col_name A character string representing the
#'  column name of the factor.
#' @param return_col_name A character string representing the
#'  column name of the returns.
#' @param group_col_name A character string representing the column name of
#'  grouping varialble.
#' @param weight_col_name A character string representing the column name
#'  of weighting variable.
#' @returns A single_period_factor_data object.
#' @importFrom tidyr replace_na
#' @keywords internal
create_single_period_factor_data <- function( # nolint: object_length_linter.
  data, date_col_name, id_col_name, factor_col_name, return_col_name,
  group_col_name, weight_col_name, ...
) {
  dargs <- list(...)

  if ("date_col_name" %in% names(dargs))
    date_col_name <- dargs$date
  if ("id_col_name" %in% names(dargs))
    id_col_name <- dargs$id_col_name
  if ("return_col_name" %in% names(dargs))
    return_col_name <- dargs$return_col_name
  if ("factor_col_name" %in% names(dargs))
    factor_col_name <- dargs$factor_col_name
  if ("group_col_name" %in% names(dargs))
    group_col_name <- dargs$group_col_name
  if ("weight_col_name" %in% names(dargs))
    weight_col_name <- dargs$weight_col_name

  # Check if necessary inputs are included
  if (missing(data))
    stop("There is no data provided")

  if (missing(date_col_name))
    stop("No date arguement provided")

  if (missing(id_col_name))
    stop("No identifier column name provided")

  if (missing(factor_col_name))
    stop("No factor column name provided")

  if (missing(return_col_name)) {
    returns <- list(rep(NA_real_, nrow(data)))
  } else {
    returns <- sapply(
      return_col_name,
      \(x, data) tidyr::replace_na(data[[x]], 0),
      data = data,
      simplify = FALSE
    )
    #returns <- tidyr::replace_na(data[[return_col_name]], 0)
  }

  if (missing(group_col_name)) {
    group <- rep("All", nrow(data))
  } else if (is.na(group_col_name)) {
    group <- rep("All", nrow(data))
  } else {
    group <- data[[group_col_name]]
  }

  if (missing(weight_col_name)) {
    weights <- rep(1, nrow(data))
  } else if (is.na(weight_col_name)) {
    weights <- rep(1, nrow(data))
  } else {
    weights <- data[[weight_col_name]] / sum(data[[weight_col_name]], na.rm = TRUE) # nolint: line_length_linter.
  }

  date <- unique(data[[date_col_name]])[1]
  ids <- data[[id_col_name]]
  fvals <- setNames(data[[factor_col_name]], ids)
  returns <- lapply(returns, setNames, ids)
  group <- setNames(group, ids)
  weights <- setNames(weights, ids)

  new("single_period_factor_data",
    factor = factor_col_name,
    date = date,
    ids = ids,
    fvals = fvals,
    returns = returns,
    group = group,
    weights = weights
  )
}

# UniverseReturnStats -----------------------------------------------------
setGeneric("calc_universe_return_stats",
  function(.data) standardGeneric("calc_universe_return_stats")
)

setMethod("calc_universe_return_stats",
  signature(.data = "single_period_factor_data"),
  function(.data) {

    .calc_hr <- function(returns, benchmark = 0) {
      length(which(returns > benchmark)) / length(which(!is.na(returns)))
    }

    n <- length(.data@ids)
    n_avail <- length(which(!is.na(.data@fvals)))
    avg_ret <- sapply(.data@returns, mean, na.rm = TRUE)
    med_ret <- sapply(.data@returns, median, na.rm = TRUE)
    hit_rate_zero <- sapply(.data@returns, .calc_hr)
    hit_rate_uavg <- mapply(.calc_hr, .data@returns, avg_ret) # nolint: line_length_linter.

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
calc_qtile_return_stats <- function(.object, fftile = 5, ...) UseMethod("calc_qtile_return_stats") # nolint: line_length_linter.

setMethod("calc_qtile_return_stats",
  signature(.object = "single_period_factor_data"),
  function(.object, fftile) {
    # Calculate the Quantile of Factors
    fq <- ctq(.object@fvals, fftile)

    .calc_hr <- function(returns, benchmark = 0) {
      length(which(returns > benchmark)) / length(which(!is.na(returns)))
    }

    single_group_return_stats <- function(.object, fq, ftile) {
      group_loc <- which(fq == ftile)
      group_ids <- .object@ids[group_loc]
      group_fvals <- .object@fvals[group_loc]
      group_weights <- .object@weights[group_loc]
      group_returns <- sapply(.object@returns, \(x) x[group_loc], simplify = FALSE) # nolint: line_length_linter.

      group_n <- length(group_ids)
      group_n_avail <- length(which(!is.na(group_fvals)))

      list(
        "n" = group_n,
        "n_avail" = group_n_avail,
        "avg_return" = sapply(group_returns, mean, na.rm = TRUE, simplify = TRUE), # nolint
        "med_return" = sapply(group_returns, median, na.rm = TRUE, simplify = TRUE), # nolint
        "hit_rate_zero" = sapply(group_returns, .calc_hr, simplify = TRUE),
        "weights" = group_weights
      )
    }

    quantile_stats <- sapply(
      levels(fq),
      single_group_return_stats,
      .object = .object,
      fq = fq,
      simplify = FALSE
    )

    qn <- length(levels(fq)[which(levels(fq) != "NA")])
    qspread <- quantile_stats[[1]]$avg_return - quantile_stats[[qn]]$avg_return
    qspreadweights <- c(
      quantile_stats[[1]]$weights,
      quantile_stats[[qn]]$weights
    )

    list(
      "q_spread" = qspread,
      "q_spread.weights" = qspreadweights,
      "q_stats" = quantile_stats
    )
  }
)