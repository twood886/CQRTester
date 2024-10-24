# single_period_at_data (S4 Object) --------------------------------------------
#' @title Single Period Alpha Test Data (S4 Object)
#' @description An S4 Class to represent Alpha Test Data for a single period.
#' @slot factor A character representing the name of the factor.
#' @slot date A date object representing the date of the data.
#' @slot ids A character vector representing the company ids.
#' @slot fvales An orderedList of factor values.
#' @slot returns An orderedList of returns.
#' @slot group A named character vector representing the company grouping.
#' @slot weights A named numeric vector of weights from factor values.
setClass(
  "single_period_at_data",
  representation(
    factor = "character",
    date = "Date",
    ids = "character",
    fvals = "orderedList",
    returns = "orderedList",
    group = "character",
    weights = "numeric"
  )
)

# Create Single Period AT Data -------------------------------------------------
#' @title Create single_period_at_data S4 Object
#' @description Function to create single_period_at_data object
#' @details This function converts data into single_period_at_data object.
#' @param date Date.
#' @param data A data frame containing id columns, return column and factor
#' value column.
#' @param date_col_name A character representing the column name of dates.
#' @param id_col_name A character representing the column name of identifiers.
#' @param factor_col_name A character string representing the
#'  column name of the factor.
#' @param return_col_name A character string representing the
#'  column name of the returns.
#' @param include_col_name A character string representing the column name of
#'  include at period variable.
#' @param group_col_name A character string representing the column name of
#'  grouping varialble.
#' @param weight_col_name A character string representing the column name
#'  of weighting variable.
#' @returns A single_period_factor_data object.
#' @importFrom tidyr replace_na
#' @keywords internal
#' @include utilities.R
#' @include orderedList.R
create_single_period_at_data <- function(
  date, data, date_col_name, id_col_name, factor_col_name, return_col_name,
  include_col_name, group_col_name, weight_col_name, horizon = 0
) {
  # Find rows in data to include
  inc <- which(data[[include_col_name]] == TRUE & data[[date_col_name]] == date)

  # Get available IDs for date
  ids <- data[inc, id_col_name][[1]]

  # Get the groups if group_col_name is provided
  if (missing(group_col_name)) {
    group <- rep("All", length(ids))
  } else if (is.na(group_col_name)) {
    group <- rep("All", length(ids))
  } else {
    group <- data[inc, group_col_name][[1]]
  }
  names(group) <- ids

  # Get the weights if weight_col_name is provided
  if (missing(weight_col_name)) {
    weights <- rep(1, length(ids)) / length(ids)
  } else if (is.na(weight_col_name)) {
    weights <- rep(1, length(ids)) / length(ids)
  } else {
    weights_raw <- data[inc, weight_col_name][[1]]
    weights <- weights_raw / sum(weights_raw, na.rm = TRUE)
  }
  names(weights) <- ids

  # Get smaller version of data for creating prv factor and fwd returns
  dates_unique <- sort(unique(data[[date_col_name]]))
  dates_horizon <- dates_unique[
    (which(dates_unique == date) - (horizon - 1)) :
      (which(dates_unique == date) + (horizon - 1))
  ]
  sub_data <- data[
    which(data[[id_col_name]] %in% ids &
        data[[date_col_name]] %in% dates_horizon
    ),
  ]

  # Get factor values
  fdata <- get_prv_factor_vals(
    sub_data, date_col_name, id_col_name, factor_col_name, horizon
  )
  fvals_df <- fdata[
    which(fdata[[date_col_name]] == date & fdata[[id_col_name]] %in% ids),
    -c(which(colnames(fdata) %in% c(date_col_name, id_col_name)))
  ]
  fvals <- orderedList(
    lapply(as.list(fvals_df), setNames, nm = ids),
    ncol(fvals_df),
    tidyr::replace_na(
      -as.numeric(stringr::str_extract(colnames(fvals_df), "[0-9]+")),
      0
    )
  )

  # Get return data
  rdata <- get_fwd_returns(
    sub_data, date_col_name, id_col_name, return_col_name, horizon
  )
  returns_df <- rdata[
    which(rdata[[date_col_name]] == date & rdata[[id_col_name]] %in% ids),
    -c(which(colnames(rdata) %in% c(date_col_name, id_col_name)))
  ]
  returns <- orderedList(
    lapply(as.list(returns_df), setNames, nm = ids),
    ncol(returns_df),
    tidyr::replace_na(
      as.numeric(stringr::str_extract(colnames(returns_df), "[0-9]+")),
      0
    )
  )

  new("single_period_at_data",
    factor = factor_col_name,
    date = date,
    ids = ids,
    fvals = fvals,
    returns = returns,
    group = group,
    weights = weights
  )
}
