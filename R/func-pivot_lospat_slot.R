#' @title Pivot slot in list of single_period_at objects
#' @description Function to pivot a list of orderedLists
#'  For use in converting a list of single_period_at objects to
#'  at slots.
#' @param x list of single_period_at objects
#' @param slotname slot name to pivot in single_period_at object
#' @param dates an array of dates
#' @param simplify a logical. Is slot a value or list
#' @return stackedorderedLists object
#' @include func-stackorderedLists.R
#' @export
pivot_lospat_slot <- function(x, slotname, dates = NULL, simplify = TRUE) {
  lol <- lapply(x, \(x) slot(x, slotname))
  stackorderedLists(lol, dates)
}
