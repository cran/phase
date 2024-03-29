#' Generate actograms for sleep data (Somnograms) for individual flies
#'
#' @description
#' This function generates a somnogram for a single fly. Input for this function must be an output from the trimData() function. The output of this function is a plotly object.
#' In a particular bin, sleep is calculated as the total minutes of inactivity equal to or greater than the defined threshold (sleep.def; typically, 5-minutes).
#'
#' @param data Input data file. The input for this function must be the output of the function trimData(). See ??trimData().
#' @param sleep.def Definition of sleep. Traditionally, a single bout of sleep is defined as any duration of inactivity that is equal to or greater than 5-minutes. However, sometimes it may be of interest to examine longer bouts of sleep; sleep.def allows users to change the definition of sleep. This defaults to 5.
#' @param bin Intervals in which data are saved (in minutes). This defaults to 30. The value of bin cannot be lower than that of sleep.def.
#' @param t.cycle Define the period of the environmental cycle or a single day in hours. This defaults to 24.
#' @param ind The channel number (or individual) whose periodogram must be plotted.
#' @param key.somno Key for reactive input tables in the shiny app.
#' @param color Color of somnograms in rgb format. The input for this must be a vector with 4 values, i.e., r,g,b,transparency. The values for r,g,b can only be between 0 and 1. 0,0,0 would be black and 1,1,1 would be white. Transparency values can also go from 0 to 1, 0 being fully transparent and 1 being fully opaque.
#'
#' @importFrom zoo rollapply
#' @importFrom plotly plot_ly add_trace layout %>% subplot
#' @importFrom grDevices rgb
#' @importFrom stats aggregate fitted lm na.omit sd
#' 
#' @return A \code{plotly} \code{htmlwidget} with the somnogram of a user defined fly.
#'
#' @export indSomnogram
#'
#' @examples
#' td <- trimData(data = df, start.date = "19 Dec 20", start.time = "21:00",
#' n.days = 10, bin = 1, t.cycle = 24)
#' somnogram <- indSomnogram(data = td, ind = 21)


indSomnogram <- function(data, sleep.def = c(5), bin = 30, t.cycle = 24, ind = 1, key.somno = 1, color = rgb(0,0,0,1)) {

  requireNamespace("plotly")
  requireNamespace("zoo")
  
  if (requireNamespace("plotly", quietly = T)) {
    # library(plotly)
    # library(zoo)
    
    n.plot = 2
    s_per_day <- (60/bin)*t.cycle
    dummy <- matrix(0, nrow = s_per_day*(n.plot-1), ncol = 1)
    
    if (length(sleep.def) == 1) {
      
      raw <- data[,10+ind]
      
      x <- raw
      y <- rle(x)
      d_y <- as.data.frame(unclass(y))
      d_y$end <- cumsum(d_y$lengths)
      d_y$start <- d_y$end - d_y$lengths + 1
      
      dd_y <- subset(d_y, d_y$values == 0 & d_y$lengths >= sleep.def)
      
      if(length(dd_y[,1]) == 0) {
        x = 0
      } else {
        for (j in 1:length(dd_y[,1])) {
          x[dd_y[j,"start"]:dd_y[j,"end"]] = -1
        }
      }
      
      x[x > -1] = 0
      x[x == -1] = 1
      raw <- x
      
      binned_full_run.sleep <- (length(raw)/(60*t.cycle))*s_per_day
      sleep <- matrix(NA, nrow = binned_full_run.sleep, ncol = 1)
      index.sleep <- seq(1, length(raw), by = bin)
      
      for (i in 1:length(index.sleep)) {
        x <- raw[index.sleep[i]:(index.sleep[i]+bin-1)]
        sleep[i,1] <- sum(x)
      }
      
    } else if (length(sleep.def) == 2) {
      
      raw <- data[,10+ind]
      
      x <- raw
      y <- rle(x)
      d_y <- as.data.frame(unclass(y))
      d_y$end <- cumsum(d_y$lengths)
      d_y$start <- d_y$end - d_y$lengths + 1
      
      dd_y <- subset(d_y, d_y$values == 0 & d_y$lengths >= sleep.def[1] & lengths < sleep.def[2])
      
      if(length(dd_y[,1]) == 0) {
        x = 0
      } else {
        for (j in 1:length(dd_y[,1])) {
          x[dd_y[j,"start"]:dd_y[j,"end"]] = -1
        }
      }
      
      x[x > -1] = 0
      x[x == -1] = 1
      raw <- x
      
      binned_full_run.sleep <- (length(raw)/(60*t.cycle))*s_per_day
      sleep <- matrix(NA, nrow = binned_full_run.sleep, ncol = 1)
      index.sleep <- seq(1, length(raw), by = bin)
      
      for (i in 1:length(index.sleep)) {
        x <- raw[index.sleep[i]:(index.sleep[i]+bin-1)]
        sleep[i,1] <- sum(x)
      }
    }
    
    
    data <- rbind(dummy, sleep, dummy)
    
    p <- list()
    
    f1 <- list(
      family = "Arial, sans-serif",
      size = 24,
      color = "black"
    )
    f2 <- list(
      family = "Arial, sans-serif",
      size = 20,
      color = "black"
    )
    
    a <- t(as.matrix(zoo::rollapply(data[,1],
                                    width = s_per_day*n.plot,
                                    by = s_per_day, as.numeric)))
    for (j in 1:length(a[1,])){
      p[[j]] <- plot_ly(
        # x = seq(0, ((length(a[,j])*(bin/60))-(bin/60)), by = bin/60),
        y = a[,j]/max(a[,j]),
        type = "bar",
        marker = list(
          color = color,
          line = list(
            color = color
          )
        ),
        source = "actogram.phases",
        key = key.somno
      )%>%
        layout(
          barmode = 'overlay',
          bargap = 0,
          yaxis = list(
            showticklabels = F,
            showline = T,
            showgrid = F,
            linecolor = "black"
          ),
          xaxis  = list(
            showgrid = F,
            showline = T,
            titlefont = f1,
            tickfont = f2,
            title = "",
            linecolor = "black",
            autotick = FALSE,
            ticks = "outside",
            tick0 = 0,
            dtick = length(a[,1])/6,
            ticklen = 7,
            tickcolor = "black",
            range = c(0, (length(a[,1])+1))
          )
        )
    }
    plot.ind.somnogram <- subplot(
      p,
      nrows = length(a[1,]),
      shareX = T,
      margin = 0.0, widths = NULL, heights = NULL
    )%>%
      layout(
        showlegend = F,
        yaxis = list(
          showticklabels = F,
          showline = T,
          showgrid = F,
          linecolor = "black"
          # range = c(y.min, y.max)
        ),
        xaxis = list(
          showline = T,
          showgrid = F,
          titlefont = f1,
          tickfont = f2,
          title = "Time index",
          linecolor = "black",
          # linewidth = 4,
          # mirror = TRUE,
          autotick = FALSE,
          ticks = "outside",
          tick0 = 0,
          dtick = length(a[,1])/6,
          ticklen = 7,
          tickcolor = "black",
          # tickwidth = 4,
          range = c(0, (length(a[,1])+1))
        )
      )
    
    return(plot.ind.somnogram)
  }
}
