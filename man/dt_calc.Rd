% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_h1_station_dt_calc.R
\name{dt_calc}
\alias{dt_calc}
\title{Perform data processing calculations}
\usage{
dt_calc(
  sfc = NULL,
  station_file = NULL,
  f_params = NULL,
  station_file_ext = ".ipip",
  ppsij = NULL,
  verbose = FALSE,
  xtra_v = FALSE,
  chunk_v = FALSE,
  ...
)
}
\arguments{
\item{sfc}{List of file paths to the temporary station file directory. Generated using \code{ipayipi::open_sf_con()}.}

\item{station_file}{Name of the station being processed.}

\item{f_params}{Function parameters evaluated by \code{ipayipi::calc_param_eval()}. These are parsed to \code{dt_calc()} from \code{dt_process()}.}

\item{station_file_ext}{The station file extension (period included '.'). Defaults to '.ipip'.}

\item{ppsij}{Data processing \code{pipe_seq} table from which function parameters and data are extracted/evaluated. This is parsed to this function automatically by \code{ipayipi::dt_process()}.}

\item{verbose}{Logical. Whether or not to report messages and progress.}

\item{xtra_v}{Logical. Whether or not to report xtra messages, progess, plus print data tables.}

\item{chunk_v}{Whether or not to indicate the start of data chunking.}
}
\value{
A list containing the processed data sets 'dts_dt'.
}
\description{
Calculations are parsed to data.table.
}
\details{
Note that gap metadata will be transferred to the station file's 'gap' table.

*Testing import of gap info and saving to sfc gaps
}
\author{
Paul J. Gordijn
}
