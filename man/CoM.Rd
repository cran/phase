% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoM.R
\name{CoM}
\alias{CoM}
\title{Objectively quantify and visualise phases and calculate consolidation in pre-defined time-windows}
\usage{
CoM(
  input,
  data = "Activity",
  bin = 30,
  t.cycle = 24,
  window = list(c(21, 3), c(9, 15)),
  rm.channels = c()
)
}
\arguments{
\item{input}{Input data file. The input for this function must be the output of the function binData(), sleepData() or ??wakeData(). See ??binData(), ??sleepData() and ??wakeData().}

\item{data}{If the input for this function is the output of binData() or sleepData(), then this must be "Activity" or "Sleep", respectively.}

\item{bin}{Intervals in which input data are saved (in minutes). This defaults to 30.}

\item{t.cycle}{Define the period of the environmental cycle or a single day in hours. This defaults to 24.}

\item{window}{A list of vectors, each defining the start and end ZT values of the window of interest. For instance, if users want to define the morning window as ZT21 to ZT03 and evening window as ZT09 to ZT15, then this should be list(c(21,3), c(9,15)). This defaults to list(c(18,6), c(6,18)). For the purposes of the polar plot, data from only the first two windows are used. But, for the data table, users can define as many windows as they like, and the function should work as expected.}

\item{rm.channels}{A vector of channels from a DAM monitor that must be discarded from analysis. If channels 1 to 5 must be removed, type in c(1:5). If channels 1 to 5 and 10 to 13 and 15 and 17 must be removed, type in c(1:5,10:13,15,17). Default is to include all individuals.}
}
\value{
A \code{list} with two items:
\describe{
\item{Plot}{A \code{plotly} \code{htmlwidget} with the center of mass plotted and averages plotted for each window.}
\item{Data}{A \code{data.frame} with 32 rows (one for each fly) and 1 + (number of user defined windows * 2) columns (First column contains channel/fly identity and the remaining columns contain the fly-wise consolidation and center of mass values.)}
}
}
\description{
This function calculates phase of center of mass of either activity or sleep data, and generates polar plots. The function also computes consolidation of activity or sleep in the defined windows of time. The output of this function is a list, the elements of which are a polar plot with the phases in each window for each fly, depicted, and a table which has the window specific times in ZT units and the consolidation values. For this function to work as expected, users must start the analysis at ZT00.
}
\examples{
td <- trimData(data = df, start.date = "19 Dec 20", start.time = "21:00",
n.days = 10, bin = 1, t.cycle = 24)
bd <- binData(td)
phase <- CoM(input = bd)
}
