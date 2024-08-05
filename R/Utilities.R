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
