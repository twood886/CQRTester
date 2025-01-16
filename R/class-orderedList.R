# orderedList (S4 Object) ------------------------------------------------------
#' @title orderedList (S4 Object)
#' @description An S4 Class to represent a list with a given order
#' @slot list A list
#' @slot n the length of the list
#' @slot order a numeric array representing the order of the list
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