################## An R-script to read YAML DTS project files, visualize and statistically evaluate data

library(ggplot2)
library(tidyr)
library(dygraphs)
library(grid)
library(reshape2)
library(dplyr)
library(gridExtra)
library(yaml)
library(ggsignif)
library(effsize)
library(pwr)
library(BayesFactor)
library(genefilter)
library(seewave)
library(lubridate)
library(rmarkdown)
library(markdown)
library(knitr)

## source the script with the functions needed for analysis
source("readXMLdatafile.R")
source("DTS_plotfunctions.R")

start.wd = getwd()

## read single YAML project file
project.file <- file.choose()
project.path = dirname(project.file)
project.data<-yaml.load_file(project.file)

#make sure the evaluations are written in a subfolder of the data folder
evaluation.path = paste(project.path,"evaluations", sep = "/")
dir.create(evaluation.path, showWarnings = FALSE)
setwd(evaluation.path)

#how many experimental groups will need to be evaluated
NofGroups=lengths(project.data["resources"])

grouped.trqhistos <- list()   #Torque histograms for group in a list of length NofPeriods
grouped.poshistos <- list()   #Position histograms for group in a list of length NofPeriods
grouped.flyhistos <- list()
grouped.flyhistosL <- list()   #Platform position histograms for group in a list of length NofPeriods
grouped.flyhistosR <- list()   #Platform position histograms for group in a list of length NofPeriods
grouped.flytraces <- list()   #Platform position traces in a list of length NofGroups
grouped.PIprofiles <- list()  #PIProfile data frames in a list of length NofGroups
grouped.periods <- list()     #Period designs in a list of length NofGroups
grouped.spectra <- list()      #Power spectra in a list of length NofGroups
#create empty list for individual fly names in each group for display in project evaluation
exp_groups <- list() 

for(x in 1:NofGroups)
{
grp_title = project.data[["resources"]][[x]][["title"]] #collect title of the group 
grp_description = project.data[["resources"]][[x]][["description"]] #collect description of the group

xml_list = paste(project.path, project.data[["resources"]][[x]][["data"]], sep = "/") #create list of file names

#create/empty lists for collecting all single fly data by period
period.data <- list()     #data grouped by period
grouped.data <- list()    #total data grouped
grouped.data.periodsL <- list() #total data grouped by left or right turning arena for platform
grouped.data.periodsR <- list()
grouped.data.tracesL <- list() #mean trace of left or right Periods
grouped.data.tracesR <- list()
grouped.data.flyL <- list()
grouped.data.flyR <- list()
speclist <- list()
#vectors for pooled  platform period traces
flytracesL <- list()
flytracesR <- list()

#start evaluating
if(MultiFlyDataVerification(xml_list)==TRUE) # make sure all flies in a group have the identical experimental design
{
  for (l in 1:length(xml_list)) 
  {
    xml_name=xml_list[l]
    
    ##### read the data with the corresponding function #######
    singleflydata <- flyDataImport(xml_name)
    
    ##extract sequence meta-data
    NofPeriods = singleflydata[[5]]
    sequence <- singleflydata[[6]]
    samplerate = as.numeric(as.character(singleflydata[[4]]$sample_rate))
    
    ##extract fly meta-data
    fly <- singleflydata[[3]]
    flyname = fly$name[1]
    
    ##extract experiment meta-data
    experimenter <- singleflydata[[2]]
    experiment <- singleflydata[[4]]

    ##extract the rawdata
    rawdata <- singleflydata[[9]]
    
    #create/empty plot lists
    poshistos <- list()
    trqhistos <- list()
    flyhistos <- list()
    flyhistosR <- list()
    flyhistosL <- list()
    flytraces <- list()
    #vectors for pooled platform period histograms
    optomotoL_flydata <- list() #unused?
    optomotoR_flydata <- list() #unused?
    periodL <- data.frame() #these empty data frames are needed if no Platform data is used
    periodR <- data.frame()
    flytracesL_data_frame <- data.frame()
    flytracesR_data_frame <- data.frame()
#### call RMarkdown for single fly evaluations ################################################
    single_fly_path <- paste(start.wd, "single_fly.Rmd", sep = "/")
    rmarkdown::render(single_fly_path, 
                      output_file = paste(flyname,"descr_anal.html", sep="_"), 
                      output_dir = evaluation.path)
#### end RMarkdown for single fly evaluations ################################################
    
    ##move PIs to multi-experiment data.frame
    if(l>1 & isTRUE(sequence$type[1]=="fs"||sequence$type[1]=="color")){
      PIprofile <- rbind2(PIprofile, as.vector(t(sequence$lambda)))
      }
    
    ##add period data to grouped data
    grouped.data[[l]] <- period.data
    grouped.data.periodsL[[l]] <- list(periodL) #special lists for left and right p_opomtor periods
    grouped.data.periodsR[[l]] <- list(periodR)

    xml_list[[l]] = paste('<a href="',flyname,'_descr_anal.html">', flyname,'</a>', sep = '')  #create link to each single fly evaluation HTML document to be used in project evaluation 
  
  } #for number of flies in xml_list

  exp_groups[[x]] <- c(grp_title, grp_description, xml_list) #add name and description and file links to dataframe to be used in project evaluation document 

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
  } #for number of periods
  
  ## plot pooled position and torque or platform histograms by period ##
  
  for(i in 1:NofPeriods)
  {
    temp<-pooled.data[[i]]
    if(sequence$type[i]=="fs"||sequence$type[i]=="color"||sequence$type[i]=="optomotor") {
      #torque
      trqhistos[[i]] <- ggplot(data=temp, aes_string(temp$torque)) +
        geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
        labs(x="torque [arb units]", y="frequency") +
        xlim(-600,600) +
        ggtitle(paste("Period", i))
      
      #position
      poshistos[[i]] <- ggplot(data=temp, aes_string(temp$a_pos)) +
        geom_histogram(binwidth=10, fill = sequence$histocolor[i]) +
        labs(x="position [arb units]", y="frequency") +
        xlim(-2047,2048) +
        ggtitle(paste("Period", i))
    } else if (sequence$type[i]=="OptomotoL"||sequence$type[i]=="OptomotoR") {
      #platform
      flyhistos[[i]] <- ggplot(data=temp, aes_string(temp$fly)) +
        geom_histogram(binwidth = 0.0175, fill = sequence$histocolor[i]) +
        labs(x="fly [arb units]", y="frequency") +
        xlim(-5,2) +
        ggtitle(paste("Period", i))
    }
  }
  
  ## pool all torque/platform and position data into single data.frame
  for (n in 1:length(grouped.data.periodsL)) {
    grouped.data.flyL[[n]] <- grouped.data.periodsL[[n]][[1]]$fly
    grouped.data.flyR[[n]] <- grouped.data.periodsR[[n]][[1]]$fly
  }
  
  ## make final data frames for plotting
  
    #for histogram with all fly data
  all.data <- do.call(rbind, pooled.data) 
  
  if (sequence$type[1]=="OptomotoL"||sequence$type[1]=="OptomotoR") {
    #for histogram with all fly data sorted by left turning arena
  all.data.periodsL <- data.frame(unlist(grouped.data.flyL))
  names(all.data.periodsL) <- "fly"
  
    #for histogram with all fly data sorted by right turning arena
  all.data.periodsR <- data.frame(unlist(grouped.data.flyR))
  names(all.data.periodsR) <- "fly"
  
    #for multiple traces graph with mean trace of each period in left/right
    #fit "flytraces" list for the "data.frame()" function
  flytracesL <- lapply(flytracesL, function(x) {
    length(x) <- min(lengths(flytracesL)); x}) 
  flytracesR <- lapply(flytracesR, function(x) {
    length(x) <- min(lengths(flytracesR)); x})
    #create data frame for pooled traces plot
    fly <- unlist(flytracesL)
    period <- rep(c(1:length(flytracesL)), each=length(flytracesL[[1]]))
    time <- rep(seq(0, (length(flytracesL[[1]])-1)*10, by=10), times=length(flytracesL))
  all.data.periodL <- data.frame(fly, period, time)
    #create data frame for pooled traces plot
    fly <- unlist(flytracesR)
    period <- rep(c(1:length(flytracesR)), each=length(flytracesR[[1]]))
    time <- rep(seq(0, (length(flytracesR[[1]])-1)*10, by=10), times=length(flytracesR))
  all.data.periodR <- data.frame(fly, period, time)
  
    #for mean traces graph of all left/right periods 
  all.data.flytracesL <- rowMeans(data.frame(flytracesL))
  all.data.flytracesL <- data.frame(all.data.flytracesL, seq(0, (length(all.data.flytracesL)-1)*10, by=10))
  names(all.data.flytracesL) <- c("fly", "time")
  all.data.flytracesR <- rowMeans(data.frame(flytracesR))
  all.data.flytracesR <- data.frame(all.data.flytracesR, seq(0, (length(all.data.flytracesR)-1)*10, by=10))
  names(all.data.flytracesR) <- c("fly", "time")
  }
  
  ## plot pooled histograms for all flies over all periods
  if(sequence$type[1]=="fs"||sequence$type[1]=="color"||sequence$type[1]=="optomotor") {
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
  } else if (sequence$type[1]=="OptomotoL"||sequence$type[1]=="OptomotoR") {
    #platform
    flyhistos[[NofPeriods+1]] <- ggplot(data=all.data, aes_string(all.data$fly)) + 
      geom_histogram(binwidth=0.0175) +
      labs(x="fly [arb units]", y="frequency") + 
      xlim(-5,2) +
      ggtitle("Pooled P_Pos Histogram")
    
    flyhistos[[NofPeriods+2]] <- ggplot(data=all.data.periodsL, aes_string(all.data.periodsL$fly)) + 
      geom_histogram(binwidth=0.0175) +
      labs(x="fly [arb units]", y="frequency") + 
      xlim(-5,2) +
      ggtitle("Pooled P_Pos Histogram\n(left turning arena)")
    
    flyhistos[[NofPeriods+3]] <- ggplot(data=all.data.periodsR, aes_string(all.data.periodsR$fly)) + 
      geom_histogram(binwidth=0.0175) +
      labs(x="fly [arb units]", y="frequency") + 
      xlim(-5,2) +
      ggtitle("Pooled P_Pos Histogram\n(right turning arena)")
    
    flytraces[[1]] <- ggplot(data=all.data.periodL, aes_string(y = all.data.periodL$fly, x = all.data.periodL$time, colour = all.data.periodL$period)) +
      geom_line(aes_string(group = all.data.periodL$period)) + 
      scale_color_gradientn(colours=rainbow(4)) +
      geom_smooth() + 
      labs(x="time (ms)", y="fly") + 
      ggtitle("Multi Traces\n(left turning arena)")
    
    flytraces[[2]] <- ggplot(data=all.data.periodR, aes_string(y = all.data.periodR$fly, x = all.data.periodR$time, colour = all.data.periodR$period)) +
      geom_line(aes_string(group = all.data.periodR$period)) + 
      scale_color_gradientn(colours=rainbow(4)) +
      geom_smooth() + 
      labs(x="time (ms)", y="fly") + 
      ggtitle("Multi Traces\n(right turning arena)")
    
    flytraces[[3]] <- ggplot(data=all.data.flytracesL, aes_string(x=all.data.flytracesL$time, y=all.data.flytracesL$fly)) +
      geom_line() +
      ggtitle("Pooled traces\n(left turning arena)") +
      labs(x="time(ms)", y="fly")
    
    flytraces[[4]] <- ggplot(data=all.data.flytracesR, aes_string(x=all.data.flytracesR$time, y=all.data.flytracesR$fly)) +
      geom_line() +
      ggtitle("Pooled traces\n(right turning arena)") +
      labs(x="time(ms)", y="fly")

  }

} else {print("You have selected files with differing metadata")}

###Collect the data from each group in their respective lists and empty the variables

#Period sequence design meta-data
grouped.periods[[x]] = periods

#Torque Histograms
grouped.trqhistos[[x]] = trqhistos #add torque histograms to list of grouped histograms
trqhistos <- list() #empty torque histograms

#Platform Histograms
grouped.flyhistos[[x]] = flyhistos   #add platform histograms to list of grouped p_position histograms
flyhistos <- list() #empty list of p_position histograms
grouped.flytraces[[x]] <- flytraces #add platform traces to list of grouped p_position traces
flytraces <- list() #empty list of p_position traces

#Position Histograms
grouped.poshistos[[x]] = poshistos #add torque histograms to list of grouped position histograms
poshistos <- list() #empty list of position histograms

#PI data
if(sequence$type[1]=="fs"||sequence$type[1]=="color"||sequence$type[1]=="optomotor") {
  colnames(PIprofile) <- sprintf("PI%d", 1:NofPeriods) #make colnames in PIprofile
  grouped.PIprofiles[[x]] = PIprofile #add PIprofile to list of grouped PIs
  PIprofile <- PIprofile[0,] #empty PIprofile
}

#Power spectra
spectemp <- do.call(cbind, speclist) #combine all power spectra
colnames(spectemp)[1]<-"freq" #label the first x-axis as frequency
spectemp$freq <- spectemp$freq*1000 #convert kHz to Hz
spectemp <- spectemp[-grep("x", colnames(spectemp))] #drop all x-axes exept the one now labelled "freq"
spectemp[length(spectemp)+1] <- rowMeans(spectemp[, grep("y", colnames(spectemp))]) #calculate the mean power spectrum in the group
spectemp[length(spectemp)+1] <- rowSds(spectemp[, grep("y", colnames(spectemp))])/sqrt(length(project.data[["resources"]][[x]][["data"]])) #calculate the standard deviation in the group
spectemp[, grep("y", colnames(spectemp))] <- NULL #drop all raw data for summary data
spectemp$group <- as.factor(rep(paste(project.data[["resources"]][[x]][["name"]], ", N=", length(project.data[["resources"]][[x]][["data"]]), sep = ""), nrow(spectemp))) #add grouping variable for later plotting
colnames(spectemp)[2] <- "mean"
colnames(spectemp)[3] <- "sd"
grouped.spectra[[x]] = spectemp #save group mean/sd

} #for nofGroups

####################################################
######## plots and statistical evaluations ########
###################################################

###### Plots ######

###generate important variables for later plotting and annotation

#check if statistcs are included, if not generate color range manually
if (!is.null(project.data[["statistics"]])) {
  colorrange = project.data[["statistics"]][["color-range"]]
} else {colorrange = c("khaki", "olivedrab3", "cornflowerblue", "goldenrod1", "indianred1", "plum3")}
boxcolors = c(colorrange[1:NofGroups])
boxes<-c(1:NofGroups)

###### if there are more than two groups, try to pool the data into 'experimental' and 'control'
if(NofGroups>2){}

###### continue for all projects with two groups

#### call RMarkdown for project evaluations ################################################
project_path <- paste(start.wd, "project.Rmd", sep = "/")
rmarkdown::render(project_path, 
                  output_file = paste(project.data$name,"html", sep = "."), 
                  output_dir = evaluation.path)
#### end RMarkdown for project evaluations #################################################


setwd(start.wd)

