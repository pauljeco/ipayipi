% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_plot_cumm.R
\name{plot_cumm}
\alias{plot_cumm}
\title{Plot cummulative sum}
\usage{
plot_cumm(
  phen_name = NULL,
  tbl_search_key = NULL,
  wanted = NULL,
  unwanted = NULL,
  x_lab = "Date-time",
  y_lab = NULL,
  input_dir = ".",
  file_ext = ".ipip",
  prompt = FALSE,
  recurr = TRUE,
  ...
)
}
\arguments{
\item{phen_name}{The standardised phenomena name. This needs to be an exact match to the desired phenomena.}

\item{tbl_search_key}{A character string used to filter station tables within which the \code{phen_name} is found. This is a search string filters station tables using the \verb{\%ilike\%} operator. Multiple search strings should be separated using the '|' symbol (regex apply).}

\item{wanted}{Character string of the station keyword. Use this to filter out stations that should not be plotted alongside eachother. If more than one search key is included, these should be separated by the bar character, e.g., \code{"mcp|manz"}, for use with data.table's \verb{\%ilike\%} operator.}

\item{unwanted}{Vector of strings listing files that should not be included in the import.}

\item{x_lab}{The x-axis label to be rendered in the 'ggplot'.}

\item{y_lab}{The y-axis label.}

\item{input_dir}{Folder in which to search for standardised station data. Defaults to the current directory.}

\item{file_ext}{The station file extension. Inherits the 'ipayipi' defult of '.ipip'.}

\item{prompt}{Logical. If \code{TRUE} the function will prompt the user to identify stations for plotting. The wanted and unwanted filters still apply when in interactive mode (\code{TRUE}).}

\item{recurr}{Should the function search recursively? I.e., thorugh sub-folders as well --- \code{TRUE}/\code{FALSE}. Defaults to \code{FALSE}.}

\item{cores}{Number of CPU's to use for processing in parallel. Only applies when working on Linux systems.}
}
\value{
A list containing the plot data and a ggplot object.
}
\description{
Calculates the cummulative sum of a selected phenomena for one or more stations. Filter stations by using the \code{wanted} and \code{unwwanted} search keys (below).
}
\details{
This function will plot the cummulative sum of similar phenomena from a number of stations.The plot can be made into an interactive chart using the \code{plotly} and \code{ggplot2} plotting interoperability.
\strong{Filtering station files}
\code{unwanted}: By default the following files (when searching for stations) are filtered out:
\itemize{
\item Unstandarised \code{ipayipi} files: '\emph{.ipr$|}.ipi$|*.iph$|wait_room|nomvet_room',
\item Other 'ipayipi' files: '\emph{.rps|}.rns$|*.rmds$', and
\item Miscellaneous: '\emph{.ods$|}.doc$|\emph{.xls$|}.r$'.
}
}
\author{
Paul J. Gordijn
}
\keyword{cummulative}
\keyword{data}
\keyword{over}
\keyword{phenomena;}
\keyword{plotting;}
\keyword{reporting;}
\keyword{view;}
