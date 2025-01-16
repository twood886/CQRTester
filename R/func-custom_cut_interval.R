#' @title Discretise numeric data into categorical
#' @param x numeric vector.
#' @param n number of intervals to create.
#' @param labels lables for the levels of the resulting category.
#' @param right logical, indicating if the intervals should be closed
#'  on the right (and open on the left) or vice versa.
custom_cut_interval <- function(x, n, labels = NULL, right = TRUE) {
  finite_x <- x[is.finite(x)]

  if (length(finite_x) == 0) {
    warning("No finite values available for calculating intervals. Setting all to NA") # nolint
    return(factor(x, levels = labels))
  }

  if (length(finite_x) < length(labels)) {
    warning("Not enough finite values available for calculating intervals. Setting all to NA") # nolint
    return(factor(rep(NA, length(x)), levels = labels))
  }

  # Calculate range and breaks
  b <- quantile(finite_x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)

  # Use base R cut to assign intervals
  cut(x, breaks = b, labels = labels, right = TRUE, include.lowest = TRUE)
}
