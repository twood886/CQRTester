#' @title Names Group Split
#' @description Splits Data by Group into List and Names
#' @details Splits Data by Group into List and Names
#' @param ... dynamic dots
#' @import dplyr
#' @import purrr
#' @export
named_group_split <- function(...) {
  data <- dplyr::group_by(...)

  names <- dplyr::group_keys(data) %>% # nolint: object_usage_linter.
    purrr::map(as.character) %>%
    purrr::reduce(paste, sep = "~~")

  dplyr::group_split(data) %>% # nolint: object_usage_linter.
    purrr::set_names(names)
}

#' @title Expand Datatable for Factor/Returns
#' @description Creates a datatable from data, date column name, id column name,
#'  and target data column name. Expands the data for all combinations of dates
#'  and ids. Non-existing data will return NA. To be used with
#'  get_prv_factor_data and get_fwd_return_data.
#' @param data Data frame
#' @param date_col_name string representing name of date column in data.
#' @param id_col_name string representing name of id column in data.
#' @param data_col_name string representing name of data in data.
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table CJ
expand_data_dt <- function(
  data, date_col_name, id_col_name, data_col_name
) {
  # Create datatable of dates, id, and data
  dt <- data.table::data.table(
    data[, date_col_name],
    data[, id_col_name],
    data[, data_col_name]
  )[
    !is.finite(get(data_col_name)), (data_col_name) := NA
  ]
  # Create datatables of all combinations of dates & id
  expand_dt <- data.table::CJ(
    data[, date_col_name][[1]],
    data[, id_col_name][[1]],
    unique = TRUE
  )
  names(expand_dt) <- c(date_col_name, id_col_name)
  # Merge data tables to get expanded dt
  merge(
    expand_dt,
    dt,
    by = c(date_col_name, id_col_name),
    all.x = TRUE
  )[order(get(id_col_name), get(date_col_name))]
}

#' @title Get lagged factor values
#' @description For every date, id get the factor and lagged values of factor.
#' @param data Data frame
#' @param date_col_name string representing name of date column in data.
#' @param id_col_name string representing name of id column in data.
#' @param factor_col_name string representing name of factor column in data.
#' @param horizon numeric representing the number of periods to lag factor data.
#' @importFrom data.table shift
#' @importFrom data.table :=
get_prv_factor_vals <- function(
  data, date_col_name, id_col_name, factor_col_name, horizon
) {
  # Calculate step
  s <- seq(1, horizon - 1, by = 1)
  dt <- expand_data_dt(
    data,
    date_col_name,
    id_col_name,
    factor_col_name
  )[
    ,
    paste0(factor_col_name, "_lag", s) :=
      lapply(s, function(n) data.table::shift(get(factor_col_name), n)),
    by = get(id_col_name)
  ]
  as.data.frame(dt)
}

#' @title Get forward periodic return values
#' @description For every date, id get the periodic forward returns.
#' @param data Data frame
#' @param date_col_name string representing name of date column in data.
#' @param id_col_name string representing name of id column in data.
#' @param return_col_name string representing name of return in data.
#' @param horizon numeric representing the number of forward return periods.
#' @importFrom data.table shift
#' @importFrom data.table :=
get_fwd_p_returns <- function(
  data, date_col_name, id_col_name, return_col_name, horizon
) {
  # Calculate step
  s <- seq(1, horizon, by = 1)
  dt <- expand_data_dt(
    data,
    date_col_name,
    id_col_name,
    return_col_name
  )[
    ,
    paste0(return_col_name, "_fwd_p", s) :=
      lapply(
        (s - 1),
        function(n) data.table::shift(get(return_col_name), -n)
      ),
    by = get(id_col_name)
  ][, (return_col_name) := NULL]
  as.data.frame(dt)
}

#' @title Get forward cumulative return values
#' @description For every date, id get the cumulative forward returns.
#' @param data Data frame
#' @param date_col_name string representing name of date column in data.
#' @param id_col_name string representing name of id column in data.
#' @param return_col_name string representing name of return in data.
#' @param horizon numeric representing the number of forward return periods.
#' @importFrom data.table data.table
#' @importFrom data.table :=
get_fwd_returns <- function(
  data, date_col_name, id_col_name, return_col_name, horizon
) {
  dt <- data.table::data.table(
    get_fwd_p_returns(
      data,
      date_col_name,
      id_col_name,
      return_col_name,
      horizon
    )
  )[
    ,
    paste0(return_col_name, "_fwd_1") := get(paste0(return_col_name, "_fwd_p1"))
  ][
    ,
    paste0(return_col_name, "_fwd_p1") := NULL
  ]
  for (i in 2:horizon) {
    dt[
      ,
      paste0("return_fwd_", i) :=
        (1 + get(paste0("return_fwd_p", i))) *
        (1 + get(paste0("return_fwd_", (i - 1)))) -
        1
    ][, paste0("return_fwd_p", i) := NULL]
  }
  as.data.frame(dt)
}

#' @title Calculate Factor Z-Score with Winsorization
#' @description Function to calculate normalized value with windsorization.
#' @details Used in Alpha Testing Functions
#' @param values a numeric vector of factor values
#' @param weights a numeric vector of weights.
#' @param group a character vector of grouping.
#' @param win.prob numeric vector of probabilities with values in [0,1]
#' as used in quantile.
#' @return ord
#' @import data.table
#' @export
ctz <- function(
  values, weights = NA_real_, group = NA_real_, win_prob = c(0, 1)
) {

  if (length(weights) == 1 && is.na(weights)) {
    weights <- rep(1, length(values))
  }

  if (length(group) == 1 && is.na(group)) {
    group <- rep("1", length(values))
  }

  dt <- data.table::data.table(
    values = values,
    weights = weights,
    group = group
  )
  # Replace NA weights with 0
  dt[is.na(values), weights := 0]
  dt[is.na(weights), weights := 0]
  # Perform winsorization
  dt[, c("minval", "maxval") := {
    q <- quantile(values, probs = win_prob, na.rm = TRUE)
    .(minval = q[1], maxval = q[2]) # nolint
  }, by = group]
  dt[, "win_values" := pmin(pmax(values, minval), maxval)] # nolint
  # Calculate grouped weighted mean and standard deviation
  dt[, "wmean" := sum(values * weights, na.rm = TRUE) / sum(weights), by = group] # nolint
  dt[, "wvar" := {
    avg <- wmean[1] # nolint
    dof <- (.N - 1) / .N
    sum(weights * (values - avg)^2, na.rm = TRUE) / (dof * sum(weights))
  }, by = group]
  dt[, "wsd" := sqrt(wvar)] # nolint
  # Compute normalized values
  dt[, "norm_x" := (win_values - wmean) / wsd] # nolint
  # Return the normalized values in the original order
  out <- dt$norm_x
  names(out) <- names(values)
  return(out)
}