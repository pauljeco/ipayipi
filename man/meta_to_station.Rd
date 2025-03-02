% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g1_meta_to_station.R
\name{meta_to_station}
\alias{meta_to_station}
\title{Open and check rainfall event metadata database.}
\usage{
meta_to_station(
  pipe_house = NULL,
  input_dir = NULL,
  meta_file = "aa_event_db",
  file_ext = ".rmds",
  in_station_meta_name = "meta_events",
  stnd_title_col_name = "stnd_title",
  wanted = NULL,
  unwanted = NULL,
  verbose = FALSE,
  xtra_v = FALSE,
  chunk_v = FALSE,
  ...
)
}
\arguments{
\item{pipe_house}{List of pipeline directories. \strong{See} \code{\link[=ipip_house]{ipip_house()}} \strong{for details}.}

\item{input_dir}{Directory from which to retreieve a metadata file read and saved by \code{ipayipi::meta_read()}.}

\item{meta_file}{File name of the standardised rainfall metadata database.}

\item{file_ext}{Extension of the metadata file. Defaults to ".rmds".}

\item{in_station_meta_name}{The file name (without extension) of the metadata file that will be filtered and appended to a station file.}

\item{wanted}{Regex string of files to select for listing. Seperate search tags by using the bar character '|'.}

\item{unwanted}{Regex string of files to filter out the listing. Seperate search tags by using the bar character '|'.
in the import.}

\item{verbose}{Logical. Print some details and progress of function progress?}

\item{xtra_v}{Logical. Should some 'x'tra messaging be done? Use to help diagnose problems, and for guidance.}

\item{station_ext}{The extension of station files. Used by \code{ipayipi::dta_list()} to search for station files. Defaults to ".ipip".}
}
\value{
Standardised data table of events.
}
\description{
Reads standardised metadata and appends records the matching station standard titles.
}
\details{
Reads in an events database or sheet in 'csv' format. Checks that column names have been standardised. Transforms the date-time columns to a standardised format --- ** this format and timezone must match that used by the data pipeline **.
}
\author{
Paul J. Gordijn
}
\keyword{data}
\keyword{data;}
\keyword{field}
\keyword{logger}
\keyword{metadata;}
\keyword{notes}
\keyword{pipeline;}
\keyword{processing;}
\keyword{supplementary}
