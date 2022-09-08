################### Plotting functions for DTS data ######################

######## Plot fly behavior and position data for each period ########

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
  plot(x = temp$time, y = pos, type = "l", col="red3", xaxs = "i", yaxs = "i", ylim=c(-1800,1800), ylab = "Position [arb.units]",xlab="time [ms]")
    par(new=TRUE)
  plot(x = temp$time, temp$fly, type = "l", col="blue", xaxs = "i", ylim=maxfly, main = paste("Fly Traces", flyname, "Period", i), axes=F, xlab=NA, ylab="")
  lines(c(temp$time[1],temp$time[nrow(temp)]),c(0,0),type="l",lty=1,lwd=1, col="black")
  axis(4)
  mtext(paste(FlyBehavior, "[arb units]"), side = 4, line = 3)
  
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
    dyAxis("y2", label = paste(FlyBehavior, "[arb units]"), independentTicks = TRUE, valueRange = maxfly, drawGrid = FALSE) %>%
    dyOptions(includeZero = TRUE) %>%
    dyRangeSelector()

  return(traces)
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
           y=1.1,
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

############### generate hyperlinks to flybase IDs

hyperlinks.FBids <- function(FBids){
  #generate dataframe with hyperlinked FBids where each group gets one row
    id.frame<-as.data.frame(lapply(read.csv(text = FBids, header = FALSE, na.strings=c("","NA")), function(x) ifelse(!is.na(x), paste('<a href="http://flybase.org/reports/',x,'">',x,'</a>', sep = ''),NA)))
    id.frame[is.na(id.frame)]<-""                     #remove NAs
    FBids=apply(id.frame,1,paste,collapse=",")        #create strings
    FBids=gsub("^,*|(?<=,),|,*$", "", FBids, perl=T)  #remove trailing/leading commas
    FBids=gsub('<a href="http://flybase.org/reports/none">none</a>', "none", FBids) #remove the link from 'none' FBids
  return(FBids)
}

###########################################################################
#                                                                         #
#                   Collected Optomotor Functions                         #
#                                                                         #
###########################################################################



##################generate optomotor data for plotting ######################
generateOMdata <- function(OMperiods){
  OMdata<-filter(rawdata, rawdata$period %in% OMperiods) #extract only right turning arena periods
  OMdata <- OMdata %>% select(-c(a_pos,date)) #drop unnecessary columns
  OMdata$time=ave(OMdata$period, OMdata$period, FUN=seq_along) #match the time values to start at each period start
  OMdata$time=(OMdata$time-1)*50 #make 20Hz data into ms time scale
  OMdata$period=as.factor(OMdata$period)
  return(OMdata)
}

##########plot raw optomotor traces of a single fly######################
plotrawOMtraces <- function(OMdata, omtitle){
  OMtraces <- ggplot(data = OMdata, aes(x=time/1000, y=fly)) + 
    geom_hline(yintercept = 0, color="black") +
    geom_line(aes(group=period, colour=period), size=1) + 
    geom_smooth(method="loess", span = 0.1) +
    ylab("Optomotor Response [rel. units]")+ 
    xlab("Time [s]") + 
    ggtitle(paste(omtitle, flyname)) +
    theme_classic()+
    scale_x_continuous(expand = expand_scale(add = 0))+
    theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_line( size=.1, color="grey"))
  return(OMtraces)
}

#########plot averaged optomotor traces of a single fly##################
plotaveOMtraces <- function(OMdata){
  OMtraces <- ggplot(data=OMdata, aes(x=OMdata$time/1000, y=OMdata[[as.character(flyname)]])) + 
    geom_rect(aes(xmin = mean(OMdata$time/1000),xmax = Inf,ymin = -Inf, ymax = Inf),fill=("grey"), alpha = 0.01)+
    geom_hline(yintercept = 0) +
    geom_line() +
    ylab("Optomotor Resoponse [rel. units]")+ 
    xlab("Time [s]") +
    theme_classic() + 
    annotate("text", -Inf, -Inf, label = "Right (clockwise) arena rotations", hjust = -.4, vjust = -1.3)+
    annotate("text", Inf, Inf, label = "Left (counter-clockwise) arena rotations", hjust =1.1, vjust = 1.5)+ 
    theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_line(colour = "black",linetype="dashed",size=0.1))+
    scale_x_continuous(expand = expand_scale(add = 0))
  return(OMtraces)
}

#########plot averaged optomotor traces of several flies #################
plotOMtracesMean <- function(OMdata){
    plotOM=ldply(OMdata, data.frame)            #move the dataframes for each group into a single dataframe
    plotOM=plotOM[,c("time","means","sd","group")]
    
    #plot averaged OM traces
    
    meanOMtraces <- ggplot(plotOM, aes(x=time/1000, y=means, group = group)) +
            theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_line( size=.1, color="grey"))+
            geom_rect(aes(xmin = mean(plotOM$time/1000),xmax = Inf ,ymin = -Inf, ymax = Inf),fill=("grey"), alpha = 0.01)+
            geom_hline(yintercept = 0, color="black") +
            geom_ribbon(aes(ymin=means-sd, ymax=means+sd, fill = group), alpha=0.5) +
            scale_fill_manual(values = boxcolors) +
            geom_line(aes(colour = group), size = 1) + 
            scale_color_manual(values = boxcolors) +
            ggtitle("Mean Optomotor Traces and Standard Deviations") +
            guides(colour = guide_legend(override.aes = list(size=3))) +
            theme_light(base_size = 16) + 
            theme(legend.justification=c(1,0),
                  legend.position="right", 
                  legend.title=element_blank(), 
                  legend.key.size = unit(2, 'lines'),
                  legend.key = element_rect(size = 6),
                  legend.box.background = element_rect(fill="white"),
                  legend.box.margin = margin(4, 4, 4, 4),
                  legend.text=element_text(size=14), 
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
            theme(axis.text.y = element_text(size=12))+
            ylab("Optomotor Response [rel. units]") + 
            xlab("Time [s]")+
            annotate("text", -Inf, -Inf, label = "Right (clockwise) arena rotations", hjust = -.4, vjust = -1.3)+
            annotate("text", Inf, Inf, label = "Left (counter-clockwise) arena rotations", hjust =1.1, vjust = 1.5)+ 
            theme(panel.grid.major.x = element_blank(),panel.grid.major.y = element_line( size=.1, color="grey"))+
            scale_x_continuous(expand = expand_scale(add = 0))
    return(meanOMtraces)

}


############plot box/whisker plots of optomotor parameters ###############
plotOMParamBox <- function(v, plotOMparams, samplesizes, OMvariables, OMtitles){
  utest = signif(wilcox.test(plotOMparams[[OMvariables[v]]] ~ plotOMparams$group)$p.value, 3) #compare the two groups with a U-test and collect p-value
  w.statistic = signif(wilcox.test(plotOMparams[[OMvariables[v]]] ~ plotOMparams$group)$statistic, 3)
  #compute effect size Cohen's D
  cohend = signif(cohen.d(plotOMparams[[OMvariables[v]]] ~ plotOMparams$group)$estimate, 3)
  #calculate statistical power
  alt = project.data[["statistics"]][["two.groups"]][["power"]]
  power=signif(pwr.t2n.test(n1 = samplesizes[1], n2= samplesizes[2], d = cohend, alternative = alt, sig.level = signif[1])$power, 3)
  #calculate Bayes Factor
  bayesF=extractBF(ttestBF(plotOMparams[[OMvariables[v]]][plotOMparams$group==groupnames[1]], plotOMparams[[OMvariables[v]]][plotOMparams$group==groupnames[2]]))
  #calculate FPR for priors set in project file#
  #run first prior  
  prior=priorval[1]
  out=calc.FPR(samplesizes,utest,prior,abs(cohend))  #output=c(FPR,x0,y0,x1,y1)
  fpz1=out[1]
  #run second prior  
  prior=priorval[2]
  out=calc.FPR(samplesizes,utest,prior,abs(cohend))  #output=c(FPR,x0,y0,x1,y1)
  fpz2=out[1]
  #Power and likelihood ratio: NB for two sided test, need 2*y0
  LR=out[5]/(2*out[3])        #lik ratio (Hi1/H0) =y1/2*y0
  
  #make tidy table of results
  results.utest<-data.frame(values=c(signif[1],
                                     w.statistic,
                                     cohend,
                                     power,
                                     signif(bayesF$bf, 3),
                                     signif(bayesF$error, 3),
                                     signif(fpz1, 3),
                                     signif(fpz2, 3),
                                     signif(LR, 3)))
  rownames(results.utest)<-c("Significance level",
                             "MW U-Test, W",
                             "Cohen's D",
                             "stat. Power",
                             "Bayes Factor",
                             "Bayes Factor error",
                             paste("FP risk, prior ",priorval[1]),
                             paste("FP risk, prior ",priorval[2]),
                             "Likelihood Ratio")
  
  # plot two optomotor parameters with asterisks
  plots.2test<-list(ggplot(plotOMparams, aes(group, plotOMparams[[OMvariables[v]]])) +
                      geom_boxplot(fill = boxcolors, notch = TRUE, outlier.color=NA, width=0.8, size=0.6) +
                      geom_jitter(data = plotOMparams, aes(group, plotOMparams[[OMvariables[v]]]), position=position_jitter(0.3), shape=21, size=3, colour="black", fill="grey50", alpha=0.4) +
                      ggtitle(paste("U-Test, p=", utest)) +
                      theme_light(base_size = 16) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
                      theme(axis.text.y = element_text(size=18))+ ylab(paste(OMtitles[v], " [rel. units]", sep = ""))+ xlab("Groups")+ theme(aspect.ratio=3/NofGroups)+
                      geom_signif(comparisons = list(c(groupnames)), map_signif_level= c("***"= signif[3],"**"= signif[2], "*"= signif[1]), textsize=8, vjust=0.5) +
                      samplesizes.annotate(boxes, samplesizes))
  
  #add table with results
  plots.2test[[2]]<-tableGrob(results.utest)
  return(plots.2test)
}


####fit models for parameter estimation to optomotor traces #####

OMparamextract <- function(OMdata){

  if(experiment$meter_type=="Goetz"){ #adjust for differences in measuring devices
  startOM=20
  slopeduration=100
  crit_diff=10
  crit_slope=0.001
  } else {
    startOM=200
    slopeduration=300
    crit_diff=50
    crit_slope=0.01
  }
  
 ########################################################  subset traces (right and left)
  
  OMmidpoint = OMdata$time[nrow(OMdata)]/2
   
  #right turning traces
  x1=OMdata$time[OMdata$time<=OMmidpoint]                     #time
  y=OMdata[[as.character(flyname)]][OMdata$time<=OMmidpoint]  #mean right OM response                 
  righttraces <-sortedXyData(x1,y)                                     #create sortedXYdata object

  #left turning traces
  
  x1=OMdata$time[OMdata$time>OMmidpoint]                      #time
  y=OMdata[[as.character(flyname)]][OMdata$time>OMmidpoint]   #mean left OM response
  lefttraces <-sortedXyData(x1,y)                             #create sortedXYdata object
  
  #all traces
  
  xall = OMdata$time   #time
  yall = OMdata[[as.character(flyname)]] # mean OM response
 
  
  AllOMtraces <- sortedXyData(xall,yall)   #create sortedXYdata object
  
 
  
  #################################################################### Optomotor Differences start-end
 
  
###right
  
  rightbegin <-head(righttraces, startOM)  #beginning of OM
  rightend <- tail(righttraces, 200) #traces at last 10s

  meanrbegin <-mean(rightbegin$y)    #mean response begin
  meanrend <-mean(rightend$y)        #mean response end

  diffresponseright <-diff(c(meanrbegin,meanrend))   # Difference OM Response begin and end
  
###left
 
  leftbegin <-head(lefttraces, startOM)  #beginning of OM
  leftend <- tail(lefttraces, 200) #traces at last 10s

  meanlbegin <-mean(leftbegin$y)    #mean response begin
  meanlend <-mean(leftend$y)        #mean response end
  
  diffresponseleft <-diff(c(meanlbegin,meanlend))   # Differenz Response begin and end
## mean
  
  diffresponse <- mean(c(abs(diffresponseright),abs(diffresponseleft)))     # mean Difference Optomotor Response
  
  ################################################################# Optomotor Slope

  rightstart <-head(righttraces, 200)  #traces at first 10s
  sloperight <- lm(y~x,data = rightstart)$coefficients[2]

  
  leftstart <-head(lefttraces, 200)  #traces at first 10s
  slopeleft <- lm(y~x,data = leftstart)$coefficients[2]
  
  
  slope <- mean(c(abs(sloperight),abs(slopeleft)))
  ###################################################################### Plot Parameters for debugging and checking
  decidemod <-ggplot(data=AllOMtraces, aes(x=x, y=y)) + geom_point() +
    geom_vline(xintercept = 30000,size=1.5)+
    geom_segment(x = 0, y = meanrbegin, xend = 5000, yend = meanrbegin,col="blue")+
    geom_segment(x = 25000, y = meanrend, xend = OMmidpoint, yend = meanrend,col="blue")+
    geom_segment(x = 25000, y = meanrbegin, xend = 25000, yend = meanrend,col="red")+
    geom_segment(x = OMmidpoint, y = meanlbegin, xend = 35000, yend = meanlbegin,col="blue")+
    geom_segment(x = 55000, y = meanlend, xend = OMmidpoint*2, yend = meanlend,col="blue")+
    geom_segment(x = 35000, y = meanlbegin, xend = 35000, yend = meanlend,col="red")+
    
    geom_smooth(method='lm', formula= y~x,data = rightstart,col ="orange")+
    geom_smooth(method='lm', formula= y~x,data = leftstart,col ="orange")
  
#  plot(decidemod)
  
  ################################################################# Decide between Models

 
  
  if (diffresponse<crit_diff | slope<crit_slope | sloperight<0 | slopeleft>0) {   #if OM/Slope  low, or slope for right traces negative, slope for left traces positive ->  take linear Model
    
    ####################################linear model
    
  ###right traces
    linfitright <- lm(y~x,data = righttraces)      # fit linear model
    linmagnright <- mean(righttraces$y)            # mean OM response right
    linsloperight <-linfitright$coefficients[2]    # slope right
  
  ###left traces
    linfitleft <- lm(y~x,data = lefttraces)        # fit linear Model
    linmagnleft <- mean(lefttraces$y)              # mean OM response left
    linslopeleft <-linfitleft$coefficients[2]      # slope left

  ### plot linear Model for debugging and testing
    linplot <- ggplot(data=AllOMtraces, aes(x=x, y=y)) + geom_point() +geom_vline(xintercept = 30000,size=1.5) + 
      geom_smooth(method='lm', formula= y~x,data = righttraces,col ="blue")+
      geom_smooth(method='lm', formula= y~x,data = lefttraces,col ="red")
    plot(linplot)
    
  ### Optomotor magnnitude and slop   
    OS <- mean(c(abs(linsloperight),abs(linslopeleft)))           # mean slopes lin mod
    OM <- (abs(diff(c(linmagnright,linmagnleft)))/2)              # mean Opto mags lin mod
  
  ### Asymmetry index linear Model
    AI_OM <- (linmagnright+linmagnleft)/(abs(linmagnright)+abs(linmagnleft))   # normalized AI OM for linear Model
    AI_OS <- (linsloperight+linslopeleft)/(abs(linsloperight)+abs(linslopeleft)) #normalized AI OS for linear Model
    
  } else {           
    
    #################################  double sigmoidal Model
    transform <- abs(min(AllOMtraces$y))      # find smallest value in OM traces for shifting to positive values
    AllOMtraces$y <- AllOMtraces$y + transform # transform y values positive
    
    colnames(AllOMtraces) <- c("time", "intensity")   #change colnames
    
    fitObjall <- fitAndCategorize(AllOMtraces, threshold_minimum_for_intensity_maximum  = 0.3,    # fit sigmoidal model
                                  threshold_intensity_range=0.1,
                                  threshold_t0_max_int = 3000)
    
    DoubleSig <- figureModelCurves(dataInput = fitObjall$normalizedInput,
                                   doubleSigmoidalFitVector = fitObjall$doubleSigmoidalModel,    # double sigmoid plot
                                   showParameterRelatedLines = TRUE)
    
    
    plot(DoubleSig)   #plot souble sigmoid model
    
    parameter_fit<- fitObjall$doubleSigmoidalModel    # parameter 
  
    #str(parameter_fit)
    
    ############################Extract parameter
    ### slopes
    
    asloperight <- parameter_fit$slope1    #slope right
    aslopeleft <- parameter_fit$slope2     #slope left
    
    OS <- mean(c(abs(asloperight),abs(aslopeleft)))
    
    ### magnitudes
    
    aMaxasympright <- (parameter_fit$maximum_y)-transform   #Asymp  max right 
    aMinasymp <- (parameter_fit$finalAsymptoteIntensity)-transform    #Asymp  min left
    
    OM <- (abs(diff(c(aMaxasympright,aMinasymp)))/2)    # Opt magnitude
    
  ######## Asymmetry Index sigmoidal Model
    AI_OM <- (aMaxasympright +aMinasymp)/(abs(aMaxasympright)+abs(aMinasymp))   #normalized AS OM for doublesigmoidal fit
    AI_OS <- (asloperight +aslopeleft)/(abs(asloperight)+abs(aslopeleft))   #normalized AS OM for doublesigmoidal fit
  }

  allparameters <- c(OM,OS,AI_OM,AI_OS)
  
  
  
 
  tempOMparams <- as.data.frame(rbind(as.numeric(allparameters)))      #convert to dataframe
  names(tempOMparams) = c("OM","OS","AI(OM)", "AI(OS)")    #set column names
  rownames(tempOMparams)[1]=as.character(flyname)                   #set row names to flynames
  
  return(tempOMparams)
}
