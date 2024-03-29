% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allSomnograms.R
\name{allSomnograms}
\alias{allSomnograms}
\title{Generate actograms for sleep data (Somnograms)}
\usage{
allSomnograms(
  data,
  sleep.def = c(5),
  bin = 30,
  t.cycle = 24,
  color = rgb(0, 0, 0, 1)
)
}
\arguments{
\item{data}{Input data file. The input for this function must be the output of the function trimData(). See ??trimData().}

\item{sleep.def}{Definition of sleep. Traditionally, a single bout of sleep is defined as any duration of inactivity that is equal to or greater than 5-minutes. However, sometimes it may be of interest to examine longer bouts of sleep or specific bout durations; sleep.def allows users to change the definition of sleep. The default input is a single value vector of value 5. If users wish to analyse sleep only between 5 to 20 mins, the input must be c(5,20).}

\item{bin}{Intervals in which data are saved (in minutes). This defaults to 30. The value of bin cannot be lower than that of sleep.def.}

\item{t.cycle}{Define the period of the environmental cycle or a single day in hours. This defaults to 24.}

\item{color}{Color of somnograms in rgb format. The input for this must be a vector with 4 values, i.e., r,g,b,transparency. The values for r,g,b can only be between 0 and 1. 0,0,0 would be black and 1,1,1 would be white. Transparency values can also go from 0 to 1, 0 being fully transparent and 1 being fully opaque.}
}
\value{
A \code{plotly} \code{htmlwidget} with 32 somnograms in a 4-by-8 array.
}
\description{
This function generates a composite figure with actograms (referred to as somnograms, here) for all flies in a DAM scanned monitor file. Input for this function must be an output from the trimData() function. The output of this function is a large plotly object. This function requires the packages "plotly" and "zoo".
In a particular bin, sleep is calculated as the total minutes of inactivity equal to or greater than the defined threshold (sleep.def; typically, 5-minutes).
}
\examples{
td <- trimData(data = df, start.date = "19 Dec 20", start.time = "21:00",
n.days = 3, bin = 1, t.cycle = 24)
somnograms <- allSomnograms(data = td[,1:15])
}
