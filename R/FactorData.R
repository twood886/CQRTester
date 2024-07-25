# factor_data_params (S4 Object) -------------------------------------------
#' @title Factor Data Parameters
#' @description
#' An S4 class containing the parameters for creating a factor_data object
#' @slot data_col_name
#' @slot id_col_name
#' @slot factor_col_name
#' @slot return_col_name
#' @slot group_col_name
setClass(
  "factor_data_params",
  representation(
    date_col_name = "character",
    id_col_name = "character",
    factor_col_name = "character",
    return_col_name = "character",
    group_col_name = "character"
  )
)


create_factor_data_params <- function(
  date_col_name = NA_character_, id_col_name = NA_character_,
  factor_col_name = NA_character_, return_col_name = NA_character_,
  group_col_name = NA_character_
) {
  new("factor_data_params",
    date_col_name = date_col_name,
    id_col_name = id_col_name,
    factor_col_name = factor_col_name,
    return_col_name = return_col_name,
    group_col_name = group_col_name
  )
}

setGeneric("check_factor_data_params",
  function(object) standardGeneric("check_factor_data_params")
)

setMethod("check_factor_data_params",
  signature(object = "factor_data"),
  function(object) {
    date_col_name <- object@factor_data_params@date_col_name
    id_col_name <- object@factor_data_params@id_col_name
    factor_col_name <- object@factor_data_params@factor_col_name
    return_col_name <- object@factor_data_params@return_col_name
    group_col_name <- object@factor_data_params@group_col_name
    data_col_names <- colnames(object@data)

    entered_names <-
      all(!is.na(c(
        date_col_name, id_col_name, factor_col_name, return_col_name
      )))

    names_in_data <-
      all(c(
        date_col_name, id_col_name, factor_col_name, return_col_name, 
        group_col_name
      ) %in% data_col_names,
      na.rm = TRUE)

    all(c(entered_names, names_in_data))
  }
)

check_factor_data_params <- function(facotr_data){
  UseMethod("check_factor_data_params")
}



setGeneric(".set_date_col",
  function(object, date_col_name) standardGeneric(".set_date_col")
)
setMethod(".set_date_col",
  signature(object = "factor_data"),
  function(object, date_col_name) {
    object@params@date_col_name <- date_col_name
    object
  }
)
set_date_col <- function(x, date_col_name) {
  UseMethod(".set_date_col")
}

setGeneric("set_id_col",
  function(object, id_col_name) standardGeneric("set_id_col")
)
setGeneric("set_factor_col",
  function(object, factor_col_name) standardGeneric("set_factor_col")
)
setGeneric("set_return_col",
  function(object, return_col_name) standardGeneric("set_return_col")
)
setGeneric("set_group_col",
  function(object, group_col_name) standardGeneric("set_group_col")
)





# factor_data (S4 Object) --------------------------------------------------
#'  An S4 Class to represent Factor Data over time
#'
#' @slot data A list of SinglePeriodFactorData objects.
#' @include SinglePeriodFactorData.R
setClass(
  "factor_data",
  representation(
    factor_data = "list",
    data = "data.frame",
    params = "factor_data_params"
  )
)


# FactorData --------------------------------------------------------------
#' @title Create Factor Data
#' @description
#' Function to create factor data from data frame.
#' @details
#' Details
#' @param data A data frame containing, id column, data column, factor column,
#' return column.
#' @param date_col_name A character representing the column name of the dates.
#' @param id_col_name A character representing the column name of the
#'  identifiers.
#' @param factor_col_name A character representing the column name of the
#'  factor.
#' @param return_col_name A character representing the column name of the
#'  returns.
#' @returns A FactorData object.
#' @import dplyr
#' @import methods
#' @include Utilities.R
#' @include SinglePeriodFactorData.R
#' @export
create_factor_data <- function(
  data,
  date_col_name = NA_character_,
  id_col_name = NA_character_,
  factor_col_name = NA_character_,
  return_col_name = NA_character_,
  group_col_name = NA_character_
) {

  factor_data_params <- create_factor_data_params(
    date_col_name = date_col_name,
    id_col_name = id_col_name,
    factor_col_name = factor_col_name,
    return_col_name = return_col_name,
    group_col_name = group_col_name
  )

  if (check_factor_data_params(factor_data_params, data)) {
    # Split Data by Data Column
    ldata <- data %>% named_group_split(!!sym(factor_data_params@date_col_name))

    # Convert split data into SinglePeriodFactorData
    fdata <- lapply(
      ldata,
      SinglePeriodFactorData,
      date = as.Date(names(ldata)),
      id_col_name = id_col_name,
      factor_col_name = factor_col_name,
      return_col_name = return_col_name
    )
  }else {
    fdata <- as.list(NULL)
  }

  new("factor_data",
    factor_data = fdata,
    data = data,
    params = factor_data_params
  )
}

setGeneric("create_factor_data_fdata",
  function(object) standardGeneric("create_factor_data_fdata")
)

setMethod("create_factor_data_fdata",
  signature(object = "factor_data"),
  function(object) {
    if(check_factor_data_params(object)) {
      ldata <- data %>%
        named_group_split(!!sym(object@factor_data_params@date_col_name))
      fdata <- lapply(
        ldata,
        SinglePeriodFactorData,
        date = as.Date(names(ldata)),
        id_col_name = object@factor_data_params@id_col_name,
        factor_col_name = object@factor_data_params@factor_col_name,
        return_col_name = object@factor_data_params@return_col_name
      )
      object@factor_data <- fdata
    }else {
      object@factor_data <- as.list(NULL)
    }
    object
  }
)











#' @export
calc_factor_avail <- function(factor_data, ...) UseMethod("calc_factor_avail")
setMethod("calc_factor_avail",
  signature(factor_data = "factor_data"),
  function(factor_data, ...) {
    .avail <- function(.data) {
      fvals <- .data@fvals
      length(which(!is.na(fvals))) / length(fvals)
    }
    unlist(lapply(FactorData@data, .avail))
  }
)