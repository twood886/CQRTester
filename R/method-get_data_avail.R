#' @include gen-get_at_data_avail.R
#' @include class-orderedList.R
#' @include class-single_period_at_data.R
setMethod("get_at_data_avail",
  signature(
    data = "single_period_at_data",
    field = "character"
  ),
  function(data, field, ...) {
    if (!field %in% slotNames(data)) {
      stop("Specified Field is not a slot in single period at data object")
    }
    vals <- slot(data, field)
    .avail <- function(x) length(which(!is.na(x))) / length(x)

    if (class(vals) == "orderedList") {
      new("orderedList",
        list = lapply(vals@list, .avail),
        n = vals@n,
        order = vals@order
      )
    } else {
      .avail(vals)
    }
  }
)