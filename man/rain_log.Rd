% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rain_c_log.r
\name{rain_log}
\alias{rain_log}
\title{Generate log of standardised hobo rain files}
\usage{
rain_log(hobo_in = NULL, log_dir = NULL, ...)
}
\arguments{
\item{hobo_in}{Can be one of the following: a list of files in the R
environment, or NULL for reading files in the 'nomvet_room'.}

\item{log_dir}{The directory for which to generate a log of all hobo
raninfall file exports. If NULL then a list of standardised hobo rainfall
files must be supplied to the 'hobo_in' argument. Character string.}
}
\value{
List containing the inventory of standardised hobo rainfall files
and a list of hobo rainfall file imports which failed to be standardised
due to incorrect formatting of the file exported from Hoboware.
}
\description{
Creates an inventory of standardised hobo rain files using
a list of files in the R environment, or reading archived RDS files
in the 'nomvet_room'.
}
\details{
Extracts the data summary from each standardised hobo rainfall
file and combines these into a table. A list of 'failed' hobo files (
\strong{see} rain_hobo_clean()) is also provided.
}
\author{
Paul J. Gordijn
}
\keyword{data}
\keyword{hobo}
\keyword{inventory}
\keyword{log;}
\keyword{pipeline;}
\keyword{rainfall}
