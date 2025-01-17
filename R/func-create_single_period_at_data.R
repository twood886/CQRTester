#' @title Create Single Period Alpha Test Data
#' @description
#' Converts raw data into a `single_period_at_data` object for single-period
#' alpha testing.
#'
#' @param date A `Date` object representing the specific period.
#' @param data A data frame containing the required columns: ID, return, and
#'   factor values.
#' @param date_col_name A character string for the column name of dates.
#' @param id_col_name A character string for the column name of IDs.
#' @param factor_col_name A character string for the column name of factors.
#' @param return_col_name A character string for the column name of returns.
#' @param include_col_name A character string for the column name of include
#'   flags (TRUE/FALSE).
#' @param group_col_name A character string for the column name of groups.
#' @param weight_col_name A character string for the column name of weights.
#' @param horizon A numeric value representing the horizon for returns and
#'   factors. Defaults to 0.
#' @return A `single_period_at_data` S4 object.
#' @include class-single_period_at_data.R
#' @keywords internal
create_single_period_at_data <- function(
  date, data, date_col_name, id_col_name, factor_col_name, return_col_name,
  include_col_name, group_col_name, weight_col_name, horizon = 0
) {
  inc <- which(data[[include_col_name]] == TRUE & data[[date_col_name]] == date)

  ids <- data[inc, id_col_name][[1]]

  group <- if (missing(group_col_name) || is.na(group_col_name)) {
    rep("All", length(ids))
  } else {
    data[inc, group_col_name][[1]]
  }
  names(group) <- ids

  weights <- if (missing(weight_col_name) || is.na(weight_col_name)) {
    rep(1, length(ids)) / length(ids)
  } else {
    weights_raw <- data[inc, weight_col_name][[1]]
    weights_raw / sum(weights_raw, na.rm = TRUE)
  }
  names(weights) <- ids

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