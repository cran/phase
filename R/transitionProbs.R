#' Plot and generate data for transition frequencies between sleep stages
#'
#' @description
#' This function generates a sankey plot enabling the visualization of percentage of times transtions from various states of activity and sleep occur. This function also generates a data table to download individual values of these transition percentages. Sleep stages are defined as follows: 5 to 30-min as short sleep (light blue), 30 to 60-min as intermediate sleep (medium blue) and 60 to 720-min as deep sleep (dark blue). Activity is defined as the standard number of beam crosses. The input for this function must be the output of the trimData() function. The output of this function is a list. Note: At this moment, this works accurately only for 24-h days.
#'
#' @param data Input data file. The input for this function must be the output of the function trimData(). See ??trimData().
#' @param n.days The number of cycles for which sleep stages must be visualized.
#' @param photoperiod This value determines the duration of photo-phase and scoto-phase of the 24-h day. Defaults to 12.
#' 
#' @importFrom wesanderson wes_palette
#' @importFrom plotly plot_ly add_trace layout %>% subplot
#' 
#' @return A \code{list} with two items:
#' \describe{
#' \item{Plots}{A \code{plotly} \code{htmlwidget} with the Sankey diagram (representing the average transition percentages).}
#' \item{Data}{A \code{matrix} \code{array} with individual wise transition percentages.}
#' }
#'
#' @export transitionProbs
#'
#' @examples
#' \dontrun{
#' td <- trimData(data = df, start.date = "19 Dec 20", start.time = "21:00",
#' n.days = 2, bin = 1, t.cycle = 24)
#' tp <- transitionProbs(data = td, n.days = 1)
#' }


transitionProbs <- function (data, n.days, photoperiod = 12) {
  requireNamespace("wesanderson")
  requireNamespace("plotly")
  
  if (requireNamespace("plotly", quietly = T)) {
    day.start.ind <- seq(1, 1440*n.days, by = 1440)
    day.end.ind <- seq((photoperiod * 60), 1440*n.days, by = 1440)
    night.start.ind <- seq((photoperiod * 60)+1, 1440*n.days, by = 1440)
    night.end.ind <- seq(1440, 1440*n.days, by = 1440)
    
    pre.day.td <- matrix(NA, nrow = length(data[,1]), ncol = 32)
    pre.night.td <- matrix(NA, nrow = length(data[,1]), ncol = 32)
    
    for (i in 1:n.days) {
      for (j in 1:32) {
        pre.day.td[day.start.ind[i]:day.end.ind[i],j] <- data[day.start.ind[i]:day.end.ind[i],10+j]
        pre.night.td[night.start.ind[i]:night.end.ind[i],j] <- data[night.start.ind[i]:night.end.ind[i],10+j]
      }
    }
    
    day.td <- as.data.frame(na.omit(pre.day.td))
    night.td <- as.data.frame(na.omit(pre.night.td))
    
    # CODE FOR DIFFERENT SLEEP CRITERIA: 1-Wake, 2-Short sleep, 3-Intermediate sleep, 4-Long sleep
    out.table <- data.frame(matrix(NA, nrow = 12, ncol = 34))
    colnames(out.table)[1:2] <- c("From", "To")
    out.table[,"From"] <- c(
      "From Wake", "From Wake", "From Wake",
      "From Short", "From Short", "From Short",
      "From Inter", "From Inter", "From Inter",
      "From Long", "From Long", "From Long"
    )
    out.table[,"To"] <- c(
      "To Short", "To Inter", "To Long",
      "To Wake", "To Inter", "To Long",
      "To Wake", "To Short", "To Long",
      "To Wake", "To Short", "To Inter"
    )
    
    for (kk in 1:32) {
      x <- night.td[,kk]
      
      x[x == 0] <- -1 # Sleep
      x[x > 0] <- -4 # Activity
      
      y <- rle(x)
      
      d_y <- as.data.frame(unclass(y))
      d_y$end <- cumsum(d_y$lengths)
      d_y$start <- d_y$end - d_y$lengths + 1
      
      d_y$values[d_y$lengths > 5 & d_y$lengths < 30] <- -2
      d_y$values[d_y$lengths > 30 & d_y$lengths < 60] <- -3
      
      
      s <- subset(d_y, d_y$values == -2)
      ss <- subset(d_y, d_y$values == -3)
      for (i in 1:length(s[,1])) {
        if(!is.na(s[1,1])) {
          x[s$start[i]:s$end[i]] <- 2 
        }
      }
      
      for (i in 1:length(ss[,1])) {
        if(!is.na(ss[1,1])) {
          x[ss$start[i]:ss$end[i]] <- 3
        }
      }
      
      x[x == -4] <- 4
      x[x == -1] <- 1
      
      x1 <- factor(paste0(head(x,-1), tail(x,-1)), levels = c('12','13','14','21','23','24','31','32','34','41','42','43'))
      
      # xdf <- data.frame(x = x[1:length(x) - 1], y = x[2:length(x)])
      
      t.xdf <- table(x1)
      
      out.table[1,2+kk] <- (as.numeric(t.xdf["12"])/sum(as.numeric(t.xdf["12"]), as.numeric(t.xdf["13"]), as.numeric(t.xdf["14"]))) * 100
      out.table[2,2+kk] <- (as.numeric(t.xdf["13"])/sum(as.numeric(t.xdf["12"]), as.numeric(t.xdf["13"]), as.numeric(t.xdf["14"]))) * 100
      out.table[3,2+kk] <- (as.numeric(t.xdf["14"])/sum(as.numeric(t.xdf["12"]), as.numeric(t.xdf["13"]), as.numeric(t.xdf["14"]))) * 100
      
      out.table[4,2+kk] <- (as.numeric(t.xdf["21"])/sum(as.numeric(t.xdf["21"]), as.numeric(t.xdf["23"]), as.numeric(t.xdf["24"]))) * 100
      out.table[5,2+kk] <- (as.numeric(t.xdf["23"])/sum(as.numeric(t.xdf["21"]), as.numeric(t.xdf["23"]), as.numeric(t.xdf["24"]))) * 100
      out.table[6,2+kk] <- (as.numeric(t.xdf["24"])/sum(as.numeric(t.xdf["21"]), as.numeric(t.xdf["23"]), as.numeric(t.xdf["24"]))) * 100
      
      out.table[7,2+kk] <- (as.numeric(t.xdf["31"])/sum(as.numeric(t.xdf["31"]), as.numeric(t.xdf["32"]), as.numeric(t.xdf["34"]))) * 100
      out.table[8,2+kk] <- (as.numeric(t.xdf["32"])/sum(as.numeric(t.xdf["31"]), as.numeric(t.xdf["32"]), as.numeric(t.xdf["34"]))) * 100
      out.table[9,2+kk] <- (as.numeric(t.xdf["34"])/sum(as.numeric(t.xdf["31"]), as.numeric(t.xdf["32"]), as.numeric(t.xdf["34"]))) * 100
      
      out.table[10,2+kk] <- (as.numeric(t.xdf["41"])/sum(as.numeric(t.xdf["41"]), as.numeric(t.xdf["42"]), as.numeric(t.xdf["43"]))) * 100
      out.table[11,2+kk] <- (as.numeric(t.xdf["42"])/sum(as.numeric(t.xdf["41"]), as.numeric(t.xdf["42"]), as.numeric(t.xdf["43"]))) * 100
      out.table[12,2+kk] <- (as.numeric(t.xdf["43"])/sum(as.numeric(t.xdf["41"]), as.numeric(t.xdf["42"]), as.numeric(t.xdf["43"]))) * 100
      
      # out.table[1,2+kk] <- as.numeric(t.xdf["12"])
      # out.table[2,2+kk] <- as.numeric(t.xdf["13"])
      # out.table[3,2+kk] <- as.numeric(t.xdf["14"])
      # 
      # out.table[4,2+kk] <- as.numeric(t.xdf["21"])
      # out.table[5,2+kk] <- as.numeric(t.xdf["23"])
      # out.table[6,2+kk] <- as.numeric(t.xdf["24"])
      # 
      # out.table[7,2+kk] <- as.numeric(t.xdf["31"])
      # out.table[8,2+kk] <- as.numeric(t.xdf["32"])
      # out.table[9,2+kk] <- as.numeric(t.xdf["34"])
      # 
      # out.table[10,2+kk] <- as.numeric(t.xdf["41"])
      # out.table[11,2+kk] <- as.numeric(t.xdf["42"])
      # out.table[12,2+kk] <- as.numeric(t.xdf["43"])
      
      # out.table[1,2+kk] <- (t.xdf["1","2"]/sum(t.xdf["1",-1], na.rm = T))*100
      # out.table[2,2+kk] <- (t.xdf["1","3"]/sum(t.xdf["1",-1], na.rm = T))*100
      # out.table[3,2+kk] <- (t.xdf["1","4"]/sum(t.xdf["1",-1], na.rm = T))*100
      # 
      # out.table[4,2+kk] <- (t.xdf["2","1"]/sum(t.xdf["2",-2], na.rm = T))*100
      # out.table[5,2+kk] <- (t.xdf["2","3"]/sum(t.xdf["2",-2], na.rm = T))*100
      # out.table[6,2+kk] <- (t.xdf["2","4"]/sum(t.xdf["2",-2], na.rm = T))*100
      # 
      # out.table[7,2+kk] <- (t.xdf["3","1"]/sum(t.xdf["3",-3], na.rm = T))*100
      # out.table[8,2+kk] <- (t.xdf["3","2"]/sum(t.xdf["3",-3], na.rm = T))*100
      # out.table[9,2+kk] <- (t.xdf["3","4"]/sum(t.xdf["3",-3], na.rm = T))*100
      # 
      # out.table[10,2+kk] <- (t.xdf["4","1"]/sum(t.xdf["4",-4], na.rm = T))*100
      # out.table[11,2+kk] <- (t.xdf["4","2"]/sum(t.xdf["4",-4], na.rm = T))*100
      # out.table[12,2+kk] <- (t.xdf["4","3"]/sum(t.xdf["4",-4], na.rm = T))*100
      
    }
    
    out.table$mean <- rowMeans(out.table[,-c(1:2)], na.rm = T)
    
    nodes <- data.frame(
      name=c(as.character(out.table$From), 
             as.character(out.table$To)) %>% unique()
    )
    
    out.table$IDfrom <- match(out.table$From, nodes$name)-1
    out.table$IDto <- match(out.table$To, nodes$name)-1
    
    q <- plot_ly(
      type = "sankey",
      orientation = "h",
      domain = list(
        x = c(0,1),
        y = c(0,1)
      ),
      arrangement = "snap",
      node = list(
        label = as.factor(nodes$name),
        x = c(0.05,0.05,0.05,0.05,0.95,0.95,0.95,0.95),
        y = c(0.1,0.2,0.3,0.6,-1,-0.6,-0.2,0.5),
        color = c(
          wes_palette("FantasticFox1")[3], wes_palette("FantasticFox1")[2], wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[5],
          wes_palette("FantasticFox1")[2], wes_palette("FantasticFox1")[1], wes_palette("FantasticFox1")[5], wes_palette("FantasticFox1")[3]
        ),
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = out.table$IDfrom,
        target = out.table$IDto,
        value = out.table$mean,
        color = list(
          rgb(70/255,172/255,200/255,0.2),
          rgb(70/255,172/255,200/255,0.2),
          rgb(70/255,172/255,200/255,0.2),
          rgb(226/255,210/255,0,0.2),
          rgb(226/255,210/255,0,0.2),
          rgb(226/255,210/255,0,0.2),
          rgb(221/255,141/255,41/255,0.2),
          rgb(221/255,141/255,41/255,0.2),
          rgb(221/255,141/255,41/255,0.2),
          rgb(180/255,15/255,32/255,0.2),
          rgb(180/255,15/255,32/255,0.2),
          rgb(180/255,15/255,32/255,0.2)
        )
      )
    )
    out.table.for.output <- out.table[,1:35]
    colnames(out.table.for.output) <- c(
      "Source", "Target",
      "Ch1", "Ch2", "Ch3", "Ch4", "Ch5", "Ch6", "Ch7", "Ch8",
      "Ch9", "Ch10", "Ch11", "Ch12", "Ch13", "Ch14", "Ch15", "Ch16",
      "Ch17", "Ch18", "Ch19", "Ch20", "Ch21", "Ch22", "Ch23", "Ch24",
      "Ch25", "Ch26", "Ch27", "Ch28", "Ch29", "Ch30", "Ch31", "Ch32",
      "Mean"
    )
    output <- list(
      "Plot" = q,
      "Data" = out.table.for.output
    )
    
    return(output)
  }
}