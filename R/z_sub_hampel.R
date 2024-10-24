#' @title Hampel filter
#' @description Applies the non-linear hampel filter to elements of a vector. The hampel is used to detect ourliers and produce estimates for imputation.
#' @param x Values on which to perform the hampel filter evaluation.
#' @param w_width
#' @param x_devs The number of deviations from the central tendacy of the window used to detect outlier values.
#' @param robust If FALSE the mean is used for interpolation instead of the median value.
#' @keywords outlier detection; imputation; missing values; patching;
#' @export
#' @author Paul J Gordijn
hampel <- function(
  x = NULL,
  w_width = 21,
  x_devs = 3,
  na_t = 1,
  tighten = 1,
  robust = TRUE,
  align = "right",
  ...
) {

  # get number of nas
  if (length(x[is.na(x)]) > na_t) return(NA)

  # get position of v in x
  # follows data.table align argument
  if (align %in% "right") {
    pos <- length(x)
  } else if (align %in% "left") {
    pos <- 1
  } else { # centre alighn is biased to the left
    pos <- floor(w_width / 2) - 1
  }

  v <- x[pos]
  # functions to be used in rolling function
  median_f <- function(x) stats::median(x, na.rm = TRUE)
  mad_f <- function(x) stats::mad(x, na.rm = TRUE)

  v <- data.table::fifelse(
    (mad_f(x) * x_devs) + abs(median_f(x)) >= v,
    median_f(x), NA
  )

  return(v)
}