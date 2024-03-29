% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sleepOnsetBoutLength.R
\name{sleepOnsetBoutLength}
\alias{sleepOnsetBoutLength}
\title{Computes, tabulates and plots day-time and night-time onset of sleep bouts and their respective lengths}
\usage{
sleepOnsetBoutLength(data, sleep.def = c(5), t.cycle = 24, photoperiod = 12)
}
\arguments{
\item{data}{Input data file. The input for this function must be the output of the function trimData(). See ??trimData().}

\item{sleep.def}{Definition of sleep. Traditionally, a single bout of sleep is defined as any duration of inactivity that is equal to or greater than 5-minutes. However, sometimes it may be of interest to examine longer bouts of sleep or specific bout durations; sleep.def allows users to change the definition of sleep. The default input is a single value vector of value 5. If users wish to analyse sleep only between 5 to 20 mins, the input must be c(5,20).}

\item{t.cycle}{Define the period of the environmental cycle or a single day in hours. This defaults to 24.}

\item{photoperiod}{Duration (in hours) of what can be considered day-phase. This defaults to 12.}
}
\value{
A \code{list} with three items:
\describe{
\item{Daytime.Data}{
\describe{
\item{Channel}{Fly identity.}
\item{Start}{Onset of sleep bout (in minutes since the start of day window)}
\item{BoutLength}{Length of the sleep bout (in minutes)}
}
}

\item{Nighttim.Data}{
\describe{
\item{Channel}{Fly identity.}
\item{Start}{Onset of sleep bout (in minutes since the start of night window)}
\item{BoutLength}{Length of the sleep bout (in minutes)}
}
}
}
}
\description{
This function allows users to compute day-time and night-time onset of sleep bouts and their respective bout lengths. Also generates a plot to visualise and compare within and across day and night-time windows. The input for this function must be the output from the trimData() function. Number of days to analyse must be at least 2 days. If the number of days is more than 2 days, the function will compute statistics for the first day only. The output of this function is a list with three elements, i.e., "Daytime.Data" is a data frame which has the onset of sleep bout (minutes since the start of the day window) and the length of that bout (in minutes) for each fly, "Nighttime.Data" which is the same as "Daytime.Data" but for the night window, and "Plots" which allows the visualisation of the two data sets.
}
\examples{
td <- trimData(data = df, start.date = "28 Dec 20", start.time = "21:00",
n.days = 2, bin = 1, t.cycle = 24)
bout.onset.vs.length <- sleepOnsetBoutLength(data = td)
}
