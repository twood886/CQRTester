#' @title Create orderedList
#' @description Creates an orderedList S4 Object
#' @param list a list
#' @param n a numeric representing the length of the list
#' @param order a numeric array representing the order of the list
#' @include class-orderedList.R
#' @export
orderedList <- function(list, n, order) {
  new("orderedList",
    list = list,
    n = n,
    order = order
  )
}