% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_sub_dta_list.R
\name{dta_list}
\alias{dta_list}
\title{Generate file list}
\usage{
dta_list(
  input_dir = ".",
  file_ext = NULL,
  wanted = NULL,
  prompt = FALSE,
  recurr = FALSE,
  baros = FALSE,
  unwanted = NULL,
  single_out = FALSE,
  ...
)
}
\arguments{
\item{input_dir}{Folder in which to search for R solonist data files.}

\item{file_ext}{Character string of the file extension of the input
data files. E.g., ".csv" for hobo rainfall file exports. This input
character string should contain the period as in the previous sentence.}

\item{wanted}{Regex string of files to select for listing. Seperate search tags by using the bar character '|'.}

\item{prompt}{Set to TRUE for interactive mode.}

\item{recurr}{Should the function search recursively
i.e., thorugh sub-folders as well - TRUE/FALSE.}

\item{baros}{Should the function include barometric files
in the final list - TRUE/FALSE. This parameter is specifically for working
with groundwater data.}

\item{unwanted}{Regex string of files to filter out the listing. Seperate search tags by using the bar character '|'.
in the import.}
}
\value{
A vector of selected R data solonist files.
}
\description{
Interactively or not, create a list (actually vector)
of files in a specified directory.
}
\author{
Paul J. Gordijn
}
\keyword{Data}
\keyword{R}
\keyword{data}
\keyword{files;}
\keyword{hobo}
\keyword{solonist}
\keyword{vector;}
