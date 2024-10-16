#' @title flag all equal
#' @description Small function to flag range issues in 5 min wind speed data
flag_all_equal <- function(x, ...) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    d <- NA
  } else {
    l <- length(unique(x))
    d <- data.table::fifelse(l == 1, 1, 0)
  }
  return(d)
}
