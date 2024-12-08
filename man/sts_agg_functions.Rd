% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sts_agg_functions}
\alias{sts_agg_functions}
\title{Aggregation function table}
\format{
\subsection{'sts_agg_functions'}{

A data.table with six columns:
\describe{
\item{measure}{The standardised measure, e.g., an average or sample.}
\item{f_continuous_desc}{Brief description of the measure function that is
used by default when aggregating different types of data.}
\item{f_continuous}{Text to be evaluated during aggregation of continuous.
variables. When aggregating data the '<>' is replaced with the values and
extra arguments to be evaluated.}
\item{f_factor}{Text to be evaluated during aggregation of factoral
variables.}
\item{f_circular}{Text to be evaluated during aggregation of circular
variables.}
\item{f_logical}{Text to be evaluated during aggregation of circular
variables.}
}
}
}
\source{
Internal.
}
\usage{
sts_agg_functions
}
\description{
Table of aggregation functions based on phenomena or variable types, the
type of measure (e.g., mean or standard deviation).
Based on the variable type and measure the aggregataion function is
selected from this table.
}
\keyword{datasets}
