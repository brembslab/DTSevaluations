################## An R-script to import DTS data and to plot it in several ways

library(ggplot2)
library(tidyr)
library(dygraphs)
library(grid)
library(reshape2)
library(dplyr)

## source the script with the functions needed for analysis
source("readXMLdatafile.R")
source("DTS_plotfunctions.R")

#create lists for collecting all single fly data by period
period.data <- list()
grouped.data <- list()

############# read file list and plot graphs for each file #############################
xml_list <- choose.files()
if(MultiFlyDataVerification(xml_list)==TRUE) { # make sure all flies in a group have the identical experimental design
  print("identical")
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
        flyhistos <- list()
        
        ##### analyze data from each period separately #######
        
        for(i in 1:NofPeriods){
          
          #save colors for later plotting
          if(sequence$outcome[i]==0){sequence$color[i]="lightyellow"} else {sequence$color[i]="orange"}
          if(sequence$outcome[i]==0){sequence$histocolor[i]="darkgreen"} else {sequence$histocolor[i]="orange"}
          
          #only look at period data
          temp  <- rawdata[rawdata$period == i, ]
          
          if (sequence$type[i] == "OptomotoL" | sequence$type[i] == "OptomotoR") {
            keeps = c("a_pos","fly")
            period.data[[i]] <- temp[keeps] #list only position and torque data by period
          } else {
            keeps = c("a_pos","torque")
            period.data[[i]] <- temp[keeps] #list only position and torque data by period
          }

          if(sequence$type[i]!="optomotor") {
            ## plot the torque and position time traces
            filename = paste(flyname,"timetraces",i,".png", sep="_")
            png(file = filename, width = 1920) # direct the following output to the image
            trq_pos_traces(temp)
            graphics.off()
          }
          
          #plot period histograms
          #platform
          flyhistos[[i]] <- ggplot(data=temp, aes_string(temp$fly)) +
            geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
            labs(x="fly [arb units]", y="frequency") +
            xlim(-600,600) +
            ggtitle(paste(flyname, "Period", i))
          
          #torque
          trqhistos[[i]] <- ggplot(data=temp, aes_string(temp$torque)) +
            geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
            labs(x="torque [arb units]", y="frequency") +
            xlim(-600,600) +
            ggtitle(paste(flyname, "Period", i))
          
          #position
          if(sequence$type[i]!="optomotor") {
          poshistos[[i]] <- ggplot(data=temp, aes_string(temp$a_pos)) +
            geom_histogram(binwidth=10, fill = sequence$histocolor[i]) +
            labs(x="position [arb units]", y="frequency") +
            xlim(-2047,2048) +
            ggtitle(paste(flyname, "Period", i))
          }
          
          ##calculate PIs
          ##create data.frame for adding PIs if it doesn't eist, yet
          if(!exists("PIprofile")){PIprofile <- data.frame(matrix(ncol = NofPeriods))}
          
          #for position
          if(sequence$type[i]!="optomotor" & sequence$type[i]!="OptomotoR" & sequence$type[i]!="OptomotoL") {
            t1 = sum(abs(temp$a_pos) >= 512 & abs(temp$a_pos) <= 1538)
            t2 = nrow(temp)-t1
            sequence$lambda[i] = (t1-t2)/(t1+t2)
            if (sequence$contingency[i] == '1_3_Q'){sequence$lambda[i]=-sequence$lambda[i]}
            if(l==1){PIprofile[1,i]=sequence$lambda[i]}
          }
          #for torque
          #if(sequence$type[i]=="yt"||"sw_blue"||"sw_green")
          #{}
        } #for Number of Periods
        
        ########## analyze data for entire experiment ##############
        
        
        ## plot position and torque histograms ##
        
        #platform
        flyhistos[[NofPeriods+1]] <- ggplot(data=rawdata, aes_string(rawdata$fly)) + 
          geom_histogram(binwidth=3) + 
          labs(x="fly [arb units]", y="frequency") + 
          xlim(-600,600) +
          ggtitle(paste(flyname, "total"))
        
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
          xlim(-2047,2048) +
          ggtitle(paste(flyname, "total"))
        
        ##write histograms to file
        if (sequence$type[i] == "OptomotoL" | sequence$type[i] == "OptomotoR") {
          #fly
          filename = paste(flyname,"fly_histos.png", sep="_")
          png(file = filename, width = 1000) # direct the following output to the image
          multiplot(plotlist = flyhistos, cols=2)
          dev.off()
        } else {
          #torque
          filename = paste(flyname,"torque_histos.png", sep="_")
          png(file = filename, width = 1000) # direct the following output to the image
          multiplot(plotlist = trqhistos, cols=2)
          dev.off()
        }
        #position
        filename = paste(flyname,"position_histos.png", sep="_")
        png(file = filename, width = 1000) # direct the following output to the image
        multiplot(plotlist = poshistos, cols=2)
        dev.off()
        
        ##plot PI bargraph if !platform
        if (sequence$type[i] != "OptomotoL" & sequence$type[i] != "OptomotoR") {
          filename = paste(flyname,"performance.png", sep="_")
          png(file = filename, width = 1000) # direct the following output to the image
          barplot(sequence$lambda, main = paste("Performance Indices", flyname),
                  xlab="Time [periods]",
                  ylab="PI [rel. units]",
                  space=0,
                  col = sequence$color)
          graphics.off()
          ##move PIs to multi-experiment data.frame
          if(l>1){PIprofile <- rbind2(PIprofile, as.vector(t(sequence$lambda)))}
        }
        
        ##dyplot traces
        if (sequence$type[i] == "OptomotoL" | sequence$type[i] == "OptomotoR") {
          traces <- dytraces_platform(rawdata)
        } else {
          traces <- dytraces(rawdata)
        }
        print(traces)
        
        ##add period data to grouped data
        grouped.data[[l]] <- period.data
        
  } #for number of flies
  
  ########### plot graphs for all experiments #####################
  
  ##pool all data except "optomotor" and "platform" by period
  
  pooled.data<-list()
  
  for(i in 1:NofPeriods)
    {
      period.data<-data.frame()
      if(sequence$type[i]!="optomotor" & sequence$type[i] != "OptomotoL" & sequence$type[i] != "OptomotoR")
      {  
        for (l in 1:length(xml_list)) 
        {
          period.data <- rbind(period.data, grouped.data[[l]][[i]])
        }
      }
      pooled.data[[i]] <- period.data
    }
  
  ## plot pooled position and torque or platform histograms by period ##
  
  for(i in 1:NofPeriods)
  {
    temp<-pooled.data[[i]]
    
    if (sequence$type[i] == "OptomotoL" | sequence$type[i] == "OptomotoR") {
      #platform
      flyhistos[[i]] <- ggplot(data=temp, aes_string(temp$fly)) +
        geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
        labs(x="fly [arb units]", y="frequency") +
        xlim(-600,600) +
        ggtitle(paste("Period", i))
    } else {
      #torque
      trqhistos[[i]] <- ggplot(data=temp, aes_string(temp$torque)) +
        geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
        labs(x="torque [arb units]", y="frequency") +
        xlim(-600,600) +
        ggtitle(paste("Period", i))
    }
      
    #position
    if(sequence$type[i]!="optomotor") {
      poshistos[[i]] <- ggplot(data=temp, aes_string(temp$a_pos)) +
        geom_histogram(binwidth=10, fill = sequence$histocolor[i]) +
        labs(x="position [arb units]", y="frequency") +
        xlim(-2047,2048) +
        ggtitle(paste("Period", i))
    }
  }
  
  ## pool all torque or platform and position data into single data.frame
  all.data <- do.call(rbind, pooled.data)
  
  
  ## plot and write pooled histograms for all flies over all periods if !platform
  ## also refrain from using any PI calcualtions for "platform" (at least for now)
  if (sequence$type[i] != "OptomotoL" & sequence$type[i] != "OptomotoR") {
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
  
    ###write pooled histograms to file###
    #torque
    filename = paste(flyname,"torque_histos.png", sep="_")
    png(file = "pooled_torque_histos.png", width = 1000) # direct the following output to the image
    multiplot(plotlist = trqhistos, cols=2)
    dev.off()
    #position
    png(file = "pooled_position_histos.png", width = 1000) # direct the following output to the image
    multiplot(plotlist = poshistos, cols=2)
    dev.off()

  
    ###make colnames in PIprofile for plotting###
    colnames(PIprofile) <- sprintf("PI%d", 1:NofPeriods)
    
    # Plot box&dotplot with notches
    
    ggplot(melt(PIprofile), aes(variable, value)) +
      geom_boxplot(fill = unique(sequence$color), notch = TRUE, outlier.color=NA, width=0.8, size=0.6) +
      geom_jitter(data = melt(PIprofile), aes(variable, value), position=position_jitter(0.3), cex=2, color="grey80") +
      ggtitle(paste("PI Profile, N=",length(xml_list))) +
      ylim(-1,1)+theme_light(base_size = 18) + theme(panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
      theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/NofPeriods)
    
    # plot violin plot
    ggplot(melt(PIprofile), aes(variable, value)) +
      geom_violin(fill = unique(sequence$color), adjust = .6) +
      stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .9),
                   size = 6, shape = 4) +
      ggtitle(paste("PI Profile, N=",length(xml_list))) +
      ylim(-1,1)+theme_light(base_size = 18) + theme(panel.grid.major.x = element_blank() ,panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
      theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/NofPeriods)
    
        
    # plot bar plot with sem
    #compute summary statistics
    error <- data.frame(period=integer(ncol(PIprofile)), mean=numeric(ncol(PIprofile)), sem=numeric(ncol(PIprofile)))
    for(e in 1:ncol(PIprofile))
    {
      error$period[e]=e
      error$mean[e]=mean(PIprofile[,e])
      error$sem[e]=sd(PIprofile[,e]/sqrt(ncol(PIprofile)))
    }
    # plot graph
    ggplot(error, aes(x=period, y=mean)) + 
      geom_bar(position=position_dodge(), stat="identity", fill = unique(sequence$color), colour="black") +
      geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),
                    width=0,
                    size=1.5,
                    position=position_dodge(.9)) +
      ggtitle(paste("PI Profile, N=",length(xml_list))) +
      ylim(-1,1)+theme_light(base_size = 18) + theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(), panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA)) +
        theme(axis.text.y = element_text(size=18))+ ylab("PI [rel. units]") + theme(aspect.ratio=4/NofPeriods)
  }
} else {print("You have selected files with differing metadata")}
