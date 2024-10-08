% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rain_i_aggregate_batch.r
\name{rain_agg_batch}
\alias{rain_agg_batch}
\title{Summarize rainfall data by multiple time periods}
\usage{
rain_agg_batch(
  input_dir = NULL,
  aggs = c("5 mins", "day", "month"),
  output_pref = "agg_",
  recurr = FALSE,
  wanted = NULL,
  unwanted = NULL,
  prompt = FALSE,
  save_data = FALSE,
  ignore_nas = FALSE,
  agg_offset = c("0 secs", "0 secs"),
  ...
)
}
\arguments{
\item{aggs}{vector of strings giving the time periods to use for aggregating
the data.}

\item{output_pref}{A string for the file name prefix. The time periods given
in the \code{aggs} argument will form the suffex. The default preffix used in
the data pipeline is 'agg_'. The 'agg_' preffix must be used for further
processing.}

\item{save_data}{Choose whether to append the new data aggregation to the
standardised data saved in the pipeline or the updated objects.}

\item{ignore_nas}{Choose whether to ignore gaps and other \code{NA} values when
aggregating the data by sum.}

\item{agg_offset}{A vector of two strings that can be coerced to a
\code{lubridate} period. These can be used to offset the date-time from
which aggregations are totalled. For example, for rainfall totals estimated
from 8 am to 8pm the \code{agg_offset} should be set to c(8 hours, 8 hours).}

\item{input_file}{Output from \code{clean_rain_hobo}}

\item{csv_out}{If \code{TRUE} the function will write a csv to the working
for each set of aggregated data.}
}
\value{
See the 'save_data' parameter.
}
\description{
Reads rainfall data from the \code{clean_rain_hobo()} function
then aggregates the data by specified time periods.
}
\details{
The function uses data.table to summarize the data by the time
periods of interest.
}
\author{
Paul J. Gordijn
}
\keyword{aggregation}
\keyword{bucket}
\keyword{data}
\keyword{gauge,}
\keyword{hoboware,}
\keyword{rain}
\keyword{tipping}
