# orderedList (S4 Object) ------------------------------------------------------
#' @title orderedList (S4 Object)
#' @description An S4 Class to represent a list with a given order
#' @slot list A list
#' @slot n the length of the list
#' @order a numeric array representing the order of the list
setClass(
  "orderedList",
  representation(
    list = "list",
    n = "numeric",
    order = "numeric"
  )
)

#' @export
"[[.orderedList" <- function(x, i = NULL) {
  if (is.null(i)) {
    u_x <- lapply(x@list, unlist)
    len_u_x <- sapply(u_x, length)
    if (all(len_u_x == 1)) {
      return(unlist(x@list))
    } else {
      return(u_x)
    }
  }
  j <- which(x@order == i)
  if (length(j) == 0) stop(paste0("There is no period of ", i, "in data"))
  x@list[[j]]
}

#' @export
"[.orderedList" <- function(x, i = NULL) {
  j <- which(x@order == i)
  if (length(j) == 0) stop(paste0("There is no period of ", i, "in data"))
  x@list[j]
}

# orderedList ------------------------------------------------------------------
#' @title Create orderedList
#' @description Creates an orderedList S4 Object
#' @param list a list
#' @param n a numeric representing the length of the list
#' @param order a numeric array representing the order of the list
#' @export
orderedList <- function(list, n, order) {
  new("orderedList",
    list = list,
    n = n,
    order = order
  )
}

# stackorderedList ------------------------------------------------------------
#' @title Stack orderedLists
#' @description Pivot the data in a list of orderedLists
#' @param lol a list of orderedLists
#' @param y an array of names
#' @param simplify logical, should new data be combined or remain list
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
