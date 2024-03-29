% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allActograms.R
\name{allActograms}
\alias{allActograms}
\title{Generate actograms}
\usage{
allActograms(data, bin = 30, t.cycle = 24, color = rgb(0, 0, 0, 1))
}
\arguments{
\item{data}{Input data file. The input for this function must be the output of the function binData(). See ??binData().}

\item{bin}{Intervals in which data are saved (in minutes). This defaults to 30.}

\item{t.cycle}{Define the period of the environmental cycle or a single day in hours. This defaults to 24.}

\item{color}{Color of actograms in rgb format. The input for this must be a vector with 4 values, i.e., r,g,b,transparency. The values for r,g,b can only be between 0 and 1. 0,0,0 would be black and 1,1,1 would be white. Transparency values can also go from 0 to 1, 0 being fully transparent and 1 being fully opaque.}
}
\value{
A \code{plotly} \code{htmlwidget} with 32 actograms in a 4-by-8 array.
}
\description{
This function generates a composite figure with actograms for all flies in a DAM scanned monitor file. Input for this function must be an output from the binData() function. The output of this function is a large plotly object. This function requires the packages "plotly" and "zoo".
}
\examples{
td <- trimData(data = df, start.date = "19 Dec 20", start.time = "21:00",
n.days = 3, bin = 1, t.cycle = 24)
bd <- binData(data = td)
actograms <- allActograms(data = bd)
}
