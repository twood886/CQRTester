setGeneric("check_at_data_params",
  function(params, data) standardGeneric("check_at_data_params")
)

#' @title Validate Alpha Test Data Parameters
#' @description
#' Checks the validity of an `at_data_params` object against a given dataset.
#'
#' @param params An `at_data_params` object specifying parameter metadata.
#' @param data A `data.frame` containing the dataset to validate.
#' @return A logical value indicating whether the parameters are valid.
#' @include class-at_data_params.R
#' @export
setMethod(
  "check_at_data_params",
  signature(params = "at_data_params", data = "data.frame"),
  function(params, data) {
    required_cols <- c(
      params@date_col_name,
      params@id_col_name,
      params@factor_col_name,
      params@return_col_name,
      params@include_col_name
    )

    optional_cols <- c(
      params@group_col_name,
      params@weight_col_name
    )

    all_required <- all(required_cols %in% colnames(data))
    all_optional <- all(optional_cols %in% colnames(data) | is.na(optional_cols)) #nolint

    all_required && all_optional
  }
)