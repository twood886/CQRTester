# stackorderedList ------------------------------------------------------------
#' @title Stack orderedLists
#' @description Pivot the data in a list of orderedLists
#' @param lol a list of orderedLists
#' @param y an array of names
#' @param simplify logical, should new data be combined or remain list
#' @include class-orderedList.R
#' @include func-orderedList.R
#' @export
stackorderedLists <- function(lol, y = NULL, simplify = TRUE) {
  # Check that all items in lol are orderedLists
  if (!all(sapply(lol, function(x) inherits(x, "orderedList")))) {
    stop("Not all items are orderedList")
  }
  # Check that all n in ols in lol are the same
  n <- unique(sapply(lol, \(x) x@n))
  if (length(n) > 1) {
    stop("Differing lenghts in orderedLists")
  }
  # Check that all order in ols in lol are the same
  order <- unique(lapply(lol, \(x) x@order))
  if (length(order) > 1) {
    stop("Differing orders in orderedLists")
  }
  order <- order[[1]]
  # Pivot the ols in lol
  x_list <- lapply(
    order, function(o) sapply(lol, function(ol) ol[[o]], simplify = simplify)
  )
  x_list <- setNames(x_list, names(lol[[1]]@list))
  # Set names if provided
  if (!is.null(y)) {
    x_list <- lapply(x_list, \(x) setNames(x, y))
  }
  # Return new orderedList
  orderedList(x_list, n, order)
}