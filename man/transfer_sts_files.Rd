% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d_transfer_sts_files.R
\name{transfer_sts_files}
\alias{transfer_sts_files}
\title{Transfer standardised ipayipi data files to the 'nomvet_room'}
\usage{
transfer_sts_files(
  pipe_house = NULL,
  prompt = FALSE,
  recurr = TRUE,
  unwanted = NULL,
  wanted = NULL,
  verbose = FALSE,
  xtra_v = FALSE,
  ...
)
}
\arguments{
\item{pipe_house}{List of pipeline directories. \strong{See} \code{\link[=ipip_house]{ipip_house()}} \strong{for details}.}

\item{prompt}{Set to TRUE for interactive mode. Note this will not work if embedded in a parallel processing instance.}

\item{recurr}{Should the function search recursively i.e., thorugh sub-folders as well - \code{TRUE}/\code{FALSE.}}

\item{unwanted}{Regex string of files to filter out the listing. Seperate search tags by using the bar character '|'.
in the import.}

\item{wanted}{Regex string of files to select for listing. Seperate search tags by using the bar character '|'.}

\item{verbose}{Logical. Print some details and progress of function progress?}

\item{xtra_v}{Logical. Should some 'x'tra messaging be done? Use to help diagnose problems, and for guidance.}
}
\value{
Saves files in native R format for 'ipayipi'. Returns a list of file names which were successfully transferred.
}
\description{
Moves files from the 'wait_room' to the 'nomvet_room', which contains standardised logger data files. This function avoids duplicating files in the 'nomvet_room' and ensures file names are unique. Files will be named using their station and table names, their date coverage, and an integer (that is unique per data file & respective date-time stamps).
}
\details{
Aim: Archive each standardised data files in a set directory, that is, the '\strong{nomvet_room}' --- part of the 'ipayipi'  data pipeline. Once archived the data will be used for further processing. Here are some of the specific steps of this funtion:
\enumerate{
\item Generate inventories of standardised files (with file extension ".ipi"`) that have been processed by \code{\link[=nomenclature_sts]{nomenclature_sts()}} and \code{\link[=phenomena_sts]{phenomena_sts()}} in the 'nomvet_room'.
\item Determine which files need to be transferred (to avoid duplication) into the 'nomvet_room'.
\item Transfer files---saving these in RDS format in the 'nomvet_room' with the ".ipi" file extension.
}
}
\author{
Paul J. Gordijn
}
\keyword{Meteorological}
\keyword{batch}
\keyword{data}
\keyword{data;}
\keyword{files;}
\keyword{processing;}
\keyword{saving}
\keyword{standardisation;}
