#' @title Internal function for returning concatenation or not
#' @description An alias for concatenation.
#' @param x A string.
#' @param verbose If TRUE the message will print, and _vice versa_ for FALSE.
#' @author Paul J. Gordijn
#' @export
#' @noRd
#' @keywords Internal
mcat <- function(x = NULL, verbose = TRUE) {
  if (verbose) return(cat(x)) else return(NULL)
}