setGeneric("check_at_data_params",
  function(params, data) standardGeneric("check_at_data_params")
)

#' @include class-at_data_params.R
setMethod("check_at_data_params",
  signature(params = "at_data_params", data = "data.frame"),
  function(params, data) {
    date_col_name <- params@date_col_name
    id_col_name <- params@id_col_name
    factor_col_name <- params@factor_col_name
    return_col_name <- params@return_col_name
    include_col_name <- params@include_col_name
    group_col_name <- params@group_col_name
    weight_col_name <- params@weight_col_name
    data_col_names <- colnames(data)

    entered_names <-
      all(!is.na(c(
        date_col_name, id_col_name,
        factor_col_name, return_col_name,
        include_col_name
      )))

    names_in_data <-
      all(c(
        date_col_name, id_col_name,
        factor_col_name, return_col_name, include_col_name,
        group_col_name, weight_col_name
      ) %in% c(data_col_names, NA_character_),
      na.rm = TRUE)

    all(c(entered_names, names_in_data))
  }
)