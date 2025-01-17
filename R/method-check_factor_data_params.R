#' @title Validate Factor Data Parameters
#' @description
#' Ensures that a `factor_data_params` object is consistent with the provided
#' dataset.
#'
#' @param params A `factor_data_params` object specifying parameter metadata.
#' @param data A `data.frame` containing the dataset to validate.
#' @return A logical value indicating whether the parameters are valid.
#' @include class-factor_data_params.R
#' @include gen-check_factor_data_params.R
#' @export
setMethod(
  "check_factor_data_params",
  signature(params = "factor_data_params", data = "data.frame"),
  function(params, data) {
    required_cols <- c(
      params@date_col_name,
      params@id_col_name,
      params@factor_col_name,
      params@return_col_name
    )

    optional_cols <- c(
      params@group_col_name,
      params@weight_col_name
    )

    all_required <- all(required_cols %in% colnames(data))
    all_optional <- all(optional_cols %in% colnames(data) | is.na(optional_cols))

    all_required && all_optional
  }
)