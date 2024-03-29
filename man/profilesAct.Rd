% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profilesAct.R
\name{profilesAct}
\alias{profilesAct}
\title{Compute and plot activity profiles}
\usage{
profilesAct(
  data,
  bin = 30,
  t.cycle = 24,
  average.type = "Both",
  rm.channels = c()
)
}
\arguments{
\item{data}{Input data file. The input for this function must be the output of the function binData(). See ??binData().}

\item{bin}{Intervals in which data are saved (in minutes). This defaults to 30. This value must be the same as that for binData().}

\item{t.cycle}{Define the period of the environmental cycle or a single day in hours. This defaults to 24. This value must be the same as that for binData().}

\item{average.type}{Define how the averaging must be done for computing profiles. There are 4 choices; i) "Flies": This will average over all the flies for the number of days of data that there are, and produce an averaged time-series, ii) "Days": This will average activity over all the days but for each fly and provide 32 averaged plots, iii) "Both": This will average over both flies and days and provide one composite average profile, iv) "None": This will not average and produce any plots; the output will be the same as the input file. This defaults to "Both".}

\item{rm.channels}{All the channels that users want to remove from their averaging. This must be a vector, i.e., channels must be separated by commas. For instance, if users choose to remove channels 1 to 5, 25 and 32, then the input should be either c(1,2,3,4,5,25,32) or c(1:5,25,32). This defaults to an empty vector, meaning no individuals are removed from analysis.}
}
\value{
Except when average.type = "None", a \code{list} with two items. When average.type = "None", input file is returned.
\describe{
If average.type = "Days":
\item{Profiles}{
\describe{
\item{ZT}{Column with ZT values.}
\item{I1:I32}{Data averaged over days for each of 32 flies.}
\item{ZT}{Column with ZT values.}
\item{I1:I32}{SEM (across days) for each of 32 flies.}
}
}
\item{Plot}{A \code{plotly} \code{htmlwidget} with the activity profiles in a 4-by-8 array.}

If average.type = "Flies":
\item{Profiles}{
\describe{
\item{ZT}{Column with ZT values.}
\item{Mean}{Data averaged over all 32 flies for the entire duration of chosen days.}
\item{SEM}{SEM (across flies).}
}
}
\item{Plot}{A \code{plotly} \code{htmlwidget} with the activity time-series.}

If average.type = "Both":
\item{Profiles}{
\describe{
\item{ZT}{Column with ZT values.}
\item{Mean}{Data averaged over all days and all 32 flies.}
\item{SEM}{SEM (across flies).}
}
}
\item{Plot}{A \code{plotly} \code{htmlwidget} with the activity profile.}
}
}
\description{
Users can compute average profiles and visualise the same. Averages can be performed either over Flies, Days, Both or None. Except in the case of "None" the output of this function will be a list with two elements. One is the generated plot and the other is a table with the activity values and corresponding standard errors of the mean.
}
\examples{
td <- trimData(data = df, start.date = "19 Dec 20", start.time = "21:00",
n.days = 10, bin = 1, t.cycle = 24)
bd <- binData(td)
pro <- profilesAct(data = bd)
pro <- profilesAct(data = bd, bin = 60, t.cycle = 32,
average.type = "Days", rm.channels = c(1:5,25,32))
}
