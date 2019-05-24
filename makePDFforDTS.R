################## An R-script to import DTS data from one or more files and to plot them into a PDF file several ways

library(ggplot2)
library(tidyr)
library(dygraphs)
library(grid)
library(reshape2)
library(dplyr)
library(gridExtra)

## source the script with the functions needed for analysis
source("readXMLdatafile.R")
source("DTS_plotfunctions.R")

#create lists for collecting all single fly data by period
period.data <- list()
grouped.data <- list()

############# read file list and plot graphs for each file #############################
xml_list <- choose.files()
#make sure the data are written in a subfolder of the data folder
evaluation.path = paste(dirname(xml_list[1]),"evaluations", sep = "/")
dir.create(evaluation.path, showWarnings = FALSE)
setwd(evaluation.path)

#start evaluating

  for (l in 1:length(xml_list)) 
  {
    xml_name=xml_list[l]
    
    ##### read the data with the corresponding function #######
    singleflydata <- flyDataImport(xml_name)
    
    ##extract sequence meta-data
    NofPeriods = singleflydata[[5]]
    sequence <- singleflydata[[6]]
    
    ##extract fly meta-data
    fly <- singleflydata[[3]]
    flyname = fly$name[1]
    
    ##extract the rawdata
    rawdata <- singleflydata[[9]]
    
    #create plot lists
    poshistos <- list()
    trqhistos <- list()

    #start writing to single-fly PDF
    filename = paste(flyname,"descr_anal.pdf", sep="_")
    pdf(file=filename, paper="a4r", pointsize=14, width = 0, height = 0)
    
    plot.new() #write text header to summary data
    #title
    mtext("Descriptive Data Evaluation Sheet", line = 2, font=2, cex=2)
    mtext(paste("for single fly experiment", flyname), line = 1, font=2, cex=1.3)
    #summary metadata text
    mdata <- collect.metadata(singleflydata)
    for(i in 1:length(mdata)){mtext(mdata[i], line = -i)}
    #plot table of period sequence
    periods=t(sequence)
    colnames(periods)<-sprintf("Period %s",1:NofPeriods)

    grid.table(periods)
    ##### analyze data from each period separately #######
    
    #add columns for PIs to sequence data
    sequence$lambda <- NA
    
    for(i in 1:NofPeriods){
      
      #save colors for later plotting
      if(sequence$outcome[i]==1){sequence$color[i]="orange"} else {sequence$color[i]="lightyellow"}
      if(sequence$outcome[i]==1){sequence$histocolor[i]="orange"} else {sequence$histocolor[i]="darkgreen"}
      
      #only look at period data
      temp  <- rawdata[rawdata$period == i, ]
      keeps = c("a_pos","torque")
      period.data[[i]] <- temp[keeps] #list only position and torque data by period
      
      if(sequence$type[i]=="fs"||sequence$type[i]=="color"||sequence$type[i]=="optomotor")
      {
        ## plot the torque and position time traces
        #par(mfrow=c(3, 1))
        trq_pos_traces(temp)
      }
      
      #plot period histograms
      #torque
      trqhistos[[i]] <- ggplot(data=temp, aes_string(temp$torque)) +
        geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
        labs(x="torque [arb units]", y="frequency") +
        xlim(-600,600) +
        ggtitle(paste(flyname, "Period", i))
      
      #position
      if(sequence$type[i]=="fs"||sequence$type[i]=="color"||sequence$type[i]=="optomotor")
      {
        poshistos[[i]] <- ggplot(data=temp, aes_string(temp$a_pos)) +
          geom_histogram(binwidth=10, fill = sequence$histocolor[i]) +
          labs(x="position [arb units]", y="frequency") +
          xlim(0,3600) +
          ggtitle(paste(flyname, "Period", i))
      }
      
      ##calculate PIs
      
      #find out from which periods we need PIs
      #NofPIperiods = sum(sequence$contingency != "")
      
      ##create data.frame for adding PIs if it doesn't eist, yet
      if(!exists("PIprofile")){PIprofile <- data.frame(matrix(ncol = NofPeriods))}
      
      #for position
      if(sequence$type[i]=="fs"||sequence$type[i]=="color")
      {
        t1 = sum((abs(temp$a_pos) >= 450 & abs(temp$a_pos) <= 1350))
        t2 = nrow(temp)-t1
        sequence$lambda[i] = (t1-t2)/(t1+t2)
        if (sequence$contingency[i] == '2_4_Q'){sequence$lambda[i]=-sequence$lambda[i]}
        if(l==1){PIprofile[1,i]=sequence$lambda[i]}
      }
      #for torque
      #if(sequence$type[i]=="yt"||"sw_blue"||"sw_green")
      #{}
    } #for Number of Periods
    
    ########## analyze data for entire experiment ##############
    
    
    ## plot position and torque histograms ##
    #torque
    trqhistos[[NofPeriods+1]] <- ggplot(data=rawdata, aes_string(rawdata$torque)) + 
      geom_histogram(binwidth=3) + 
      labs(x="torque [arb units]", y="frequency") + 
      xlim(-600,600) +
      ggtitle(paste(flyname, "total"))
    
    #position
    poshistos[[NofPeriods+1]] <- ggplot(data=rawdata, aes_string(rawdata$a_pos)) + 
      geom_histogram(binwidth=10) +
      labs(x="position [arb units]", y="frequency") + 
      xlim(0,3600) +
      ggtitle(paste(flyname, "total"))
    
    ##write histograms to file
    #torque
    multiplot(plotlist = trqhistos, cols=2)
    #position
    multiplot(plotlist = poshistos, cols=2)

    ##plot PI bargraph
    
    barplot(sequence$lambda, main = paste("Performance Indices", flyname),
            xlab="Time [periods]",
            ylab="PI [rel. units]",
            space=0,
            col = sequence$color)
    dev.off()
    ##dyplot traces
    traces <- dytraces(rawdata)
    print(traces)
    
    ##move PIs to multi-experiment data.frame
    if(l>1){PIprofile <- rbind2(PIprofile, as.vector(t(sequence$lambda)))}
    
    ##add period data to grouped data
    grouped.data[[l]] <- period.data
    
  } #for number of flies
  
  ########### plot graphs for all experiments #####################
  
  ##pool all data except "optomotor" by period
  
  pooled.data<-list()
  
  for(i in 1:NofPeriods)
  {
    period.data<-data.frame()
    if(sequence$type[i]!="optomotor")
    {  
      for (l in 1:length(xml_list)) 
      {
        period.data <- rbind(period.data, grouped.data[[l]][[i]])
      }
    }
    pooled.data[[i]] <- period.data
  }
  
  ## plot pooled position and torque histograms by period ##
  
  for(i in 1:NofPeriods)
  {
    temp<-pooled.data[[i]]
    
    #torque
    trqhistos[[i]] <- ggplot(data=temp, aes_string(temp$torque)) +
      geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
      labs(x="torque [arb units]", y="frequency") +
      xlim(-600,600) +
      ggtitle(paste("Period", i))
    
    #position
    if(sequence$type[i]=="fs"||sequence$type[i]=="color"||sequence$type[i]=="optomotor")
    {
      poshistos[[i]] <- ggplot(data=temp, aes_string(temp$a_pos)) +
        geom_histogram(binwidth=10, fill = sequence$histocolor[i]) +
        labs(x="position [arb units]", y="frequency") +
        xlim(-2047,2048) +
        ggtitle(paste("Period", i))
    }
  }
  
  ## pool all torque and position data into single data.frame
  
  all.data <- do.call(rbind, pooled.data)
  

  ## plot pooled histograms for all flies over all periods
  
  #torque
  trqhistos[[NofPeriods+1]] <- ggplot(data=all.data, aes_string(all.data$torque)) + 
    geom_histogram(binwidth=3) + 
    labs(x="torque [arb units]", y="frequency") + 
    xlim(-600,600) +
    ggtitle("Pooled Torque Histogram")
  
  #position
  poshistos[[NofPeriods+1]] <- ggplot(data=all.data, aes_string(all.data$a_pos)) + 
    geom_histogram(binwidth=10) +
    labs(x="position [arb units]", y="frequency") + 
    xlim(-2047,2048) +
    ggtitle("Pooled Position Histogram")
  
  #######Start Plotting into PDF file
    if(length(xml_list)>1)
    {
      pdf(file="group_descriptive_plots.pdf", paper="a4r", pointsize=1, width = 0, height = 0)
      
      multiplot(plotlist = trqhistos, cols=2) #torquehistos
      multiplot(plotlist = poshistos, cols=2) #positionhistos
    
      ###make colnames in PIprofile for plotting###
      colnames(PIprofile) <- sprintf("PI%d", 1:NofPeriods)
      
      
      ## plot bar plot with sem
         # compute summary statistics
      error <- data.frame(period=integer(ncol(PIprofile)), mean=numeric(ncol(PIprofile)), sem=numeric(ncol(PIprofile)))
      for(e in 1:ncol(PIprofile))
      {
        error$period[e]=e
        error$mean[e]=mean(PIprofile[,e])
        error$sem[e]=sd(PIprofile[,e]/sqrt(nrow(PIprofile)))
      }
         # plot graph
      print(ggplot(error, aes(x=period, y=mean)) + 
      geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
      geom_bar(fill = sequence$color, position=position_dodge(), stat="identity", colour="black") +
      geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),
                    width=0,
                    size=1.5,
                    position=position_dodge(.9)) +
      ggtitle(paste("PI Profile, N=",length(xml_list))) +
        scale_x_continuous(breaks = seq(1, NofPeriods, 1)) +
        scale_y_continuous(breaks = seq(-1, 1, .2)) +
      theme_light(base_size = 18) + theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(), panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
      theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/NofPeriods))
      
      ## Plot box&dotplot with notches
      print(ggplot(melt(PIprofile), aes(variable, value)) +
        geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
        geom_boxplot(fill = sequence$color, notch = TRUE, outlier.color=NA, width=0.8, size=0.6) +
        geom_jitter(data = melt(PIprofile), aes(variable, value), position=position_jitter(0.3),  shape=21, size=3, colour="black", fill="grey50", alpha=0.4) +
        ggtitle(paste("PI Profile, N=",length(xml_list))) +
          scale_y_continuous(breaks = seq(-1, 1, .2)) +
        theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
        theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/NofPeriods))
      
      ## Plot box&dotplot without notches
      print(ggplot(melt(PIprofile), aes(variable, value)) +
              geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
              geom_boxplot(fill = sequence$color, notch = FALSE, outlier.color=NA, width=0.8, size=0.6) +
              geom_jitter(data = melt(PIprofile), aes(variable, value), position=position_jitter(0.3),  shape=21, size=3, colour="black", fill="grey50", alpha=0.4) +
              ggtitle(paste("PI Profile, N=",length(xml_list))) +
              scale_y_continuous(breaks = seq(-1, 1, .2)) +
              theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
              theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/NofPeriods))

      ## plot violin plot
      print(ggplot(melt(PIprofile), aes(variable, value)) +
        geom_hline(yintercept = 0, colour = "#887000", size = 1.2) +
        geom_violin(width = 1.1) +
        geom_boxplot(fill = sequence$color, width = 0.1, outlier.color="darkred") +
        ggtitle(paste("PI Profile, N=",length(xml_list))) +
          scale_y_continuous(breaks = seq(-1, 1, .2)) +
        theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
        theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/NofPeriods))

    dev.off()
    }
