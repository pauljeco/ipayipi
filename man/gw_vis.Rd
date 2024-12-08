% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gw_plot_basic.R
\name{gw_vis}
\alias{gw_vis}
\title{Visualisation of GW corrections}
\usage{
gw_vis(
  file = NULL,
  log_dwd = TRUE,
  dips = TRUE,
  drift_neutral = TRUE,
  ly = FALSE,
  time_labs_y = NULL,
  zoom_x = NULL,
  focus_y = TRUE,
  date_labels = "\%Y-\%m-\%d",
  y_break_inc = NULL,
  date_breaks = NULL,
  ...
)
}
\arguments{
\item{file}{A level file object read into R.}

\item{log_dwd}{If TRUE this adds interference events, that is, the start and stop plus logger logger download annotations.}

\item{dips}{If TRUE this will annotate dip level readings.}

\item{drift_neutral}{If TRUE the barometrically corrected data with a constant drift correction factor calculated with the mean of the drift offset.}

\item{ly}{If TRUE the transformed water level is plotted in the interactive plotly interface.}

\item{time_labs_y}{The position on the y-axis on which the time labels are drawn for the interference events.}

\item{zoom_x}{Vector of start and end date times to limit x axis e.g. c("2018-03-12", "2018-04-11").}

\item{focus_y}{If TRUE, the y axis is limited scaled to focus on the corrected data.}

\item{date_labels}{Default format is \%Y-\%m-\%d.}

\item{y_break_inc}{Optional specification of increment between tick marks.}

\item{date_breaks}{Optional increment of date time axis tick mark increments,}
}
\description{
This function uses ggplot functionality to plot and visualize corrected and uncorrected water level data.
}
\details{
In addiion to the basic plot of corrected water level data, using ggplot this function adds basic information that may be used to interpret whether the corrections performed on the data are valid.
}
\author{
Paul J. Gordijn
}
\keyword{Ground}
\keyword{graph}
\keyword{visualisation;}
\keyword{water}
