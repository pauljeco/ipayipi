% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_a_logger_data_import_batch.R
\name{logger_data_import_batch}
\alias{logger_data_import_batch}
\title{Import logger data}
\usage{
logger_data_import_batch(
  pipe_house = NULL,
  prompt = FALSE,
  recurr = TRUE,
  wanted = NULL,
  unwanted = NULL,
  file_ext = NULL,
  verbose = FALSE,
  xtra_v = FALSE,
  ...
)
}
\arguments{
\item{pipe_house}{List of pipeline directories. \strong{See} \code{ipayipi::ipip_house()} \strong{for details}.}

\item{prompt}{Should the function use an interactive file selection function otherwise all files are returned. \code{TRUE} or \code{FALSE}.}

\item{recurr}{Should the function search recursively into sub directories for hobo rainfall csv export files? \code{TRUE} or \code{FALSE}.}

\item{wanted}{Vector of strings listing files that should not be included in the import.}

\item{unwanted}{Vector of strings listing files that should not be included in the import.}

\item{file_ext}{The file extension defaults to NULL. Other file types could be incorporatted if required.}

\item{verbose}{Print some details of the files being processed? Logical.}
}
\description{
Copies and pastes data matching search criteria from the \code{source_room} into the 'wait_room'.
}
\details{
\code{logger_data_import_batch()} copies logger data files from multiple stations in the 'source_room' into the 'wait_room' where data standardisation will take place using: 1) the \code{imbibe_raw_batch()}, 2) \code{header_sts()}, and 3) \code{phenomena_sts()}, in that order. Once standardised in native R format files the data is transferred into the \code{nomvet_room} with \code{transfer_sts_files()}. The standardised files in the 'nomvet_room' are kept there and station records are developed in the 'ipip_room' using \code{append_station_batch()}. Data can be furhter processed with \code{dt_process_batch()}. \cr
Notes:
\itemize{
\item This function will process data from multiple stations from the 'source_room'. Note: duplicate file names will have unique consequtive integers added as a suffix.
}
}
\author{
Paul J. Gordijn
}
\keyword{automatic}
\keyword{batch}
\keyword{data}
\keyword{data;}
\keyword{files;}
\keyword{hydrological}
\keyword{import}
\keyword{logger}
\keyword{meteorological}
\keyword{process;}
\keyword{station;}
\keyword{weather}
