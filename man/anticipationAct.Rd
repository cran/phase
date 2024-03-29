% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anticipationAct.R
\name{anticipationAct}
\alias{anticipationAct}
\title{Compute anticipation for activity data}
\usage{
anticipationAct(
  data,
  method = "Slope",
  t.cycle = 24,
  morn.win.start,
  eve.win.start,
  eve.win.end,
  rm.channels = c(),
  max.y.morn = "auto",
  max.y.eve = "auto"
)
}
\arguments{
\item{data}{Input data file. The input for this function must be the output of the function trimData(). See ??trimData().}

\item{method}{Choose the method for estimating anticipation. The three possible choices are, (i) "Slope", (ii) "Stoleru" and (iii) "Harrisingh". This defaults to "Slope". See Description for more details on these methods.}

\item{t.cycle}{Define the period of the environmental cycle or a single day in hours. This defaults to 24.}

\item{morn.win.start}{Define the start of morning window in ZT. ZT00 is the onset of zeitgeber; in case of light/dark cycles, ZT00 is the time at which lights turn ON. The end of the morning window is considered as ZT00.}

\item{eve.win.start}{Define the start of evening window in ZT.}

\item{eve.win.end}{Define the end of evening window.}

\item{rm.channels}{A vector of channels from a DAM monitor that must be discarded from analysis. If channels 1 to 5 must be removed, type in c(1:5). If channels 1 to 5 and 10 to 13 and 15 and 17 must be removed, type in c(1:5,10:13,15,17). Default is to include all individuals.}

\item{max.y.morn}{Set the upper limit of the y-axis in the plot for morning window.}

\item{max.y.eve}{Set the upper limit of the y-axis in the plot for evening window.}
}
\value{
If method = "Slope", a \code{list} with two items, else a \code{matrix} \code{array} with 32 rows (one for each fly) and 3 columns (Channel/Fly identity, Morning anticipation index and Evening anticipation index).
\describe{
If method = "Slope":
\item{Plot.morn}{A \code{plotly} \code{htmlwidget} with the anticipation estimates for the morning window.}
\item{Plot.eve}{A \code{plotly} \code{htmlwidget} with the anticipation estimates for the evening window.}
\item{Data}{A \code{matrix} \code{array} with 32 rows (one for each fly) and 3 columns (Channel/Fly identity, Morning anticipation index and Evening anticipation index).}
}
}
\description{
This function calculates anticipation of activity in user-defined windows. The input for this function must be the output of the trimData() function. There are three choices for the estimation of anticipation. The "Slope" method simply finds best-fitting lines to activity data in the defined windows (by using linear regressions). The slope of this line, for each fly, is used as a measure of anticipation. The "Stoleru" method is based on the estimates used in the paper by Stoleru et al., 2004 (https://doi.org/10.1038/nature02926). The authors estimate progressive increase in activity before environmental transitions, scaled by the startle response (immediately post-transition). The "Harrisingh" method is based on the method used by Harrisingh et al., 2007 (10.1523/JNEUROSCI.3680-07.2007). In this method, the authors simply examine activity in a 3-hour interval before environmental transitions, scaled by activity in the 6-hour interval before the transitions. Only if method = "Slope", the output of this function is a list with three elements, i.e., plots of the regression line along with the activity in the morning window, plots of the regression line along with the activity in the evening window, and a table with the morning and evening anticipation values. In the other two cases, the output of this function is only a table with the morning and evening anticipation values. No plots will be generated.
}
\examples{
td <- trimData(data = df, start.date = "19 Dec 20", start.time = "21:00",
n.days = 10, bin = 1, t.cycle = 24)
anticip <- anticipationAct(data = td, method = "Stoleru", t.cycle = 24,
                           morn.win.start = 21,
                           eve.win.start = 9, eve.win.end = 12,
                           rm.channels = c())
}
