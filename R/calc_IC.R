#' @include factor_z_score.R
#' @include return_z_score.R
calc_ic <- function(factor_z, return_z) UseMethod("calc_ic")


#' @include orderedList.R
#' @importFrom stringr str_replace
setMethod("calc_ic",
  signature(
    factor_z = "factor_z_score",
    return_z = "orderedList"
  ),
  function(factor_z, return_z) {
    fwd_returns <- lapply(return_z@list, as.numeric)
    fwd_ics <- lapply(
      fwd_returns,
      cor,
      as.numeric(factor_z),
      use = "pairwise.complete.obs"
    )
    names(fwd_ics) <- lapply(
      names(fwd_ics),
      \(x) stringr::str_replace(x, "return", "ic")
    )
    orderedList(fwd_ics, return_z@n, return_z@order)
  }
)

#' @include orderedList.R
#' @importFrom tidyr replace_na
#' @importFrom stringr str_extract
setMethod("calc_ic",
  signature(
    factor_z = "orderedList",
    return_z = "return_z_score"
  ),
  function(factor_z, return_z) {
    prv_factors <- lapply(factor_z@list, as.numeric)
    ics_fwd <- lapply(
      prv_factors,
      cor,
      as.numeric(return_z),
      use = "pairwise.complete.obs"
    )
    names(ics_fwd) <- lapply(
      names(ics_fwd),
      \(x) {
        paste0(
          "ic_factor_",
          tidyr::replace_na(stringr::str_extract(x, "lag[0-9]+"), "lag0")
        )
      }
    )
    orderedList(ics_fwd, factor_z@n, factor_z@order)
  }
)
