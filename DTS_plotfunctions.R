################### Plotting functions for DTS data ######################

######## Plot torque and position data for each period ########

trq_pos_traces <- function(temp)
{
  ##modify a_Pos to introduce gaps between -180° and 180°
  pos <- temp$a_pos
  for(p in 2:nrow(temp))
  {
    if (abs(as.numeric(pos[p]))>1950 && sign(as.numeric(pos[p]))!=sign(as.numeric(pos[p-1]))) {pos[p-1]="NA"}
  }
  par(mar=c(5, 4, 4, 4) + 0.1)
  traces <- plot(x = temp$time, y = temp$a_pos, type = "n", axes=FALSE, xaxs = "i", yaxs = "i", ylim=c(-2047,2048), ylab = "",xlab="")
  #shade quadrants
  rect(temp$time[1],-1538,temp$time[nrow(temp)],-512, col = "grey95")
  rect(temp$time[1],512,temp$time[nrow(temp)],1538, col = "grey95")
    par(new=TRUE)
  plot(x = temp$time, y = pos, type = "l", col="red3", xaxs = "i", yaxs = "i", ylim=c(-2047,2048), ylab = "position[arb.units]",xlab="time [ms]")
    par(new=TRUE)
  plot(x = temp$time, temp$torque, type = "l", col="blue", xaxs = "i", ylim=c(-500,500), main = paste("Fly Traces", flyname, "Period", i), axes=F, xlab=NA, ylab="")
  lines(c(temp$time[1],temp$time[nrow(temp)]),c(0,0),type="l",lty=1,lwd=1, col="black")
  axis(4)
  mtext("torque [arb_units]", side = 4, line = 3)
  
  return(traces)
}

dytraces <- function(rawdata)
{
  traces <- dygraph(rawdata, main = paste("Time Traces", flyname)) %>%
    dySeries("a_pos", label = "position", color = "darkred") %>%
    dySeries("torque", axis = 'y2', color = "blue") %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y", label = "Position [arb_units]", valueRange = c(-2048,2048)) %>%
    dyOptions(gridLineColor = "lightgrey") %>%
    dyAxis("y2", label = "Torque [arb_units]", independentTicks = TRUE, valueRange = c(-700,700), drawGrid = FALSE) %>%
    dyOptions(includeZero = TRUE) %>%
    dyRangeSelector()

  return(traces)
}

############ Function to plot several plots into a single plot #########

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) 
{
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############ Function to annotate plots after stats #########

#Wilcoxon annotations
wilcox.annotate <- function(boxes, wilcoxon)
{
  annotate("text",
              x=boxes,
              y=1.1,
              label=paste("p=",wilcoxon[boxes]))
}

#samplesizes annotations
samplesizes.annotate <- function(boxes, samplesizes)
{
  annotate("text",
           x=boxes,
           y=-1.1,
           label=paste("N=", samplesizes[boxes]))
}