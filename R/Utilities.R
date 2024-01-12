#' @title Names Group Split
#' @description Splits Data by Group into List and Names
#' @details Splits Data by Group into List and Names
#' @param ... dynamic dots
#' @import dplyr
#' @import purrr
#' @export
named_group_split <- function(...) {
  data <- group_by(...)

  names <- group_keys(data) %>%
    purrr::map(as.character) %>%
    purrr::reduce(paste, sep = "~~")

  dplyr::group_split(data) %>%
    purrr::set_names(names)
}
