#' @title Hampel filter
#' @description Applies the non-linear hampel filter to elements of a vector. The hampel is used to detect ourliers and produce estimates for imputation. If the focal value is an NA value the median is returned.
#' @param x Values on which to perform the hampel filter evaluation.
#' @param w Total width of the filter window.
#' @param d The number of deviations from the central tendacy of the window used to detect outlier values.
#' @param robust If FALSE the mean is used for interpolation instead of the median value.
#' @keywords outlier detection; imputation; missing values; patching;
#' @details
#'  - hampel ref
#'  - customistion here
#'  - and warnings
#' @export
#' @author Paul J Gordijn
hampel <- function(
  x = NULL,
  w = 21,
  d = 3,
  na_t = 1,
  tighten = 1,
  robust = TRUE,
  align = "right",
  ...
) {

  # get number of nas
  if (length(x[is.na(x)]) > na_t) return(NA_real_)

  # determine the proportion of unique values in series
  if (max(table(x)) >= (w / 2)) return(NA_real_)

  # get position of v in x
  # follows data.table align argument
  if (align %in% "right") {
    pos <- length(x)
  } else if (align %in% "left") {
    pos <- 1
  } else { # centre align is biased to the left
    pos <- floor(w / 2)
  }

  v <- x[pos]
  # functions to be used in rolling function
  median_f <- function(x) stats::median(x, na.rm = TRUE)
  mad_f <- function(x) stats::mad(x, na.rm = TRUE)

  # if the focal value falls outside the accepted madf or
  # the value is na replace with median
  m <- median_f(x)
  if (is.na(v)) return(m)
  if (((mad_f(x) * d) + abs(m)) < v && v != m) return(m)
  return(NA_real_)
}