% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_sub_hampel.R
\name{hampel}
\alias{hampel}
\title{Hampel filter}
\usage{
hampel(
  x = NULL,
  w = 21,
  d = 3,
  na_t = 1,
  tighten = 1,
  robust = TRUE,
  align = "right",
  ...
)
}
\arguments{
\item{x}{Values on which to perform the hampel filter evaluation.}

\item{w}{Total width of the filter window.}

\item{d}{The number of deviations from the central tendacy of the window used to detect outlier values.}

\item{robust}{If FALSE the mean is used for interpolation instead of the median value.}
}
\description{
Applies the non-linear hampel filter to elements of a vector. The hampel is used to detect ourliers and produce estimates for imputation. If the focal value is an NA value the median is returned.
}
\details{
\itemize{
\item hampel ref
\item customistion here
\item and warnings
}
}
\author{
Paul J Gordijn
}
\keyword{detection;}
\keyword{imputation;}
\keyword{missing}
\keyword{outlier}
\keyword{patching;}
\keyword{values;}
