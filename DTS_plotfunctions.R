################### Plotting functions for DTS data ######################

######## Plot fl behavior and position data for each period ########

fly_pos_traces <- function(temp)
{
  ##modify a_Pos to introduce gaps between -180° and 180°
  pos <- temp$a_pos
  for(p in 2:nrow(temp))
  {
    if (abs(as.numeric(pos[p]))>1750 && sign(as.numeric(pos[p]))!=sign(as.numeric(pos[p-1]))) {pos[p-1]="NA"}
  }
  par(mar=c(5, 4, 4, 4) + 0.1)
  traces <- plot(x = temp$time, y = temp$a_pos, type = "n", axes=FALSE, xaxs = "i", yaxs = "i", ylim=c(-1800,1800), ylab = "",xlab="")
  #shade quadrants
  rect(temp$time[1],-1350,temp$time[nrow(temp)],-450, col = "grey95")
  rect(temp$time[1],450,temp$time[nrow(temp)],1350, col = "grey95")
    par(new=TRUE)
  plot(x = temp$time, y = pos, type = "l", col="red3", xaxs = "i", yaxs = "i", ylim=c(-1800,1800), ylab = "position[arb.units]",xlab="time [ms]")
    par(new=TRUE)
  plot(x = temp$time, temp$fly, type = "l", col="blue", xaxs = "i", ylim=maxfly, main = paste("Fly Traces", flyname, "Period", i), axes=F, xlab=NA, ylab="")
  lines(c(temp$time[1],temp$time[nrow(temp)]),c(0,0),type="l",lty=1,lwd=1, col="black")
  axis(4)
  mtext("torque [arb_units]", side = 4, line = 3)
  
  return(traces)
}

dytraces <- function(rawdata)
{
  traces <- dygraph(rawdata, main = paste("Time Traces", flyname)) %>%
    dySeries("a_pos", label = "position", color = "darkred") %>%
    dySeries("fly", axis = 'y2', color = "blue") %>%
    dyAxis("x", drawGrid = FALSE) %>%
    dyAxis("y", label = "Position [arb_units]", valueRange = c(-1800,1800)) %>%
    dyOptions(gridLineColor = "lightgrey") %>%
    dyAxis("y2", label = "Torque [arb_units]", independentTicks = TRUE, valueRange = maxfly, drawGrid = FALSE) %>%
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
#source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

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

########### Function to calculate False Positive Risk

#======================================================
calc.FPR =  function(samplesizes,pval,prior,delta1){
  sdiff=sqrt(1/samplesizes[1] + 1/samplesizes[2])
  df=samplesizes[1]+samplesizes[2]-2
  # Note FPR doesn't need calculation of power for p-equals case  
  #  
  #under H0, use central t distribution
  tcrit=qt((1-pval/2),df,ncp=0)
  x0=tcrit
  y0=dt(x0,df,0)
  #
  # under H1 use non-central t distribution
  ncp1=delta1/sdiff    #non-centrality paramater
  x1=x0  #tcrit
  y1=dt(x1,df,ncp=ncp1)
  
  # Calc false positive risk
  p0=2*y0
  p1=y1
  FPR=((1-prior)*p0)/(((1-prior)*p0) + prior*p1)
  FPR
  output=c(FPR,x0,y0,x1,y1)
  return(output)
}


############### plot split violin plots for two groups #################
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}
# usage: ggplot(my_data, aes(x, y, fill = m)) + geom_split_violin()

