% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_sub_rng_rici.r
\name{rng_rici}
\alias{rng_rici}
\title{Data extractor by row and column indicies}
\usage{
rng_rici(
  r_rng = NULL,
  c_rng = NULL,
  r_fx = NULL,
  c_fx = NULL,
  c_fx_start = NULL,
  c_fx_end = NULL,
  setup_name = NA,
  string_extract = NULL,
  ...
)
}
\arguments{
\item{r_rng}{The row indicies provided as a vector.}

\item{c_rng}{The column indicies provided as a vector.}

\item{r_fx}{Single integer value corresponding to the row number wherein
lie all row indicies of interest.}

\item{c_fx}{Single integer value corresponding to the column number wherein
lie all column indicies of interest.}

\item{c_fx_start}{If the \code{c_rng} or \code{c_fx} parameters are not supplied the
function needs the start (\code{c_fx_start}) and end (\code{c_fx_end}) of the column
indicies where phenomena are stored.}

\item{c_fx_end}{If the \code{c_rng} or \code{c_fx} parameters are not supplied the
function needs the start (\code{c_fx_start}) and end (\code{c_fx_end}) of the column
indicies where phenomena are stored. \code{c_fx_end} can be extracted from the
supplied file if this option is set to "extract".}

\item{setup_name}{The name of the item/data found at the defined index. A
character string.}

\item{string_extract}{If these options are specified the function will store
parameters for the extraction of a single character string from the defined
rici range when reading logger data.
A list made up of the following objects (all requried):
\enumerate{
\item \code{rng_pattern} A string used to search the range used to identify the
position of the substring of interest to be extracted.
\item \code{rel_start} The starting position (integer) of the string relative to
the begining of the \code{rng_pattern}.
\item \code{rel_end} The end position (integer) of the string relative to the
begining of the \code{rng_pattern}.
}}
}
\value{
A list of length two containing interger indicies for a row and
column, respectively.
}
\description{
A predefined function to return the row and column indicies
in a standardised way, that is, providing the row, then column indicies, in
that order, respectively, in a list of length two.
}
\details{
This is an internal function not meant to be used as stand alone
feature.
}
\author{
Paul J. Gordijn
}
