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
grouped.PIprofiles <- list()  #PIProfile data frames in a list of length NofGroups
grouped.periods <- list()     #Period designs in a list of length NofGroups
grouped.spectra <- list()      #Power spectra in a list of length NofGroups

for(x in 1:NofGroups)
{
xml_list = paste(project.path, project.data[["resources"]][[x]][["data"]], sep = "/")

#create/empty lists for collecting all single fly data by period
period.data <- list()     #data grouped by period
grouped.data <- list()    #total data grouped
speclist <- list()

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
    
#### call RMarkdown for single fly evaluations ################################################
    rmarkdown::render('b:/GitHub/DTSevaluations/single_fly.Rmd', 
                      output_file = paste(flyname,"descr_anal.html", sep="_"), 
                      output_dir = evaluation.path)
#### end RMarkdown for single fly evaluations ################################################
    
    ##move PIs to multi-experiment data.frame
    if(l>1){PIprofile <- rbind2(PIprofile, as.vector(t(sequence$lambda)))}
    
    ##add period data to grouped data
    grouped.data[[l]] <- period.data
    

  } #for number of flies in xml_list

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

} else {print("You have selected files with differing metadata")}

###Collect the data from each group in their respective lists and empty the variables

#Period sequence design meta-data
grouped.periods[[x]] = periods

#Torque Histograms
grouped.trqhistos[[x]] = trqhistos #add torque histograms to list of grouped histograms
trqhistos <- list() #empty torque histograms

#Position Histograms
grouped.poshistos[[x]] = poshistos #add torque histograms to list of grouped position histograms
poshistos <- list() #empty list of position histograms

#PI data
colnames(PIprofile) <- sprintf("PI%d", 1:NofPeriods) #make colnames in PIprofile
grouped.PIprofiles[[x]] = PIprofile #add PIprofile to list of grouped PIs
PIprofile <- PIprofile[0,] #empty PIprofile

#Power spectra
spectemp <- do.call(cbind, speclist) #combine all power spectra
colnames(spectemp)[1]<-"freq" #label the first x-axis as frequency
spectemp$freq <- spectemp$freq*1000 #convert kHz to Hz
spectemp <- spectemp[, -grep("x", colnames(spectemp))] #drop all x-axes exept the one now labelled "freq"
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
colorrange = project.data[["statistics"]][["color-range"]]
boxcolors = c(colorrange[1:NofGroups])
boxes<-c(1:NofGroups)

###### if there are more than two groups, try to pool the data into 'experimental' and 'control'
if(NofGroups>2){}

###### continue for all projects with two groups

#### call RMarkdown for project evaluations ################################################
rmarkdown::render('b:/GitHub/DTSevaluations/project.Rmd', 
                  output_file = paste(project.data$name,"html", sep = "."), 
                  output_dir = evaluation.path)
#### end RMarkdown for project evaluations #################################################


setwd(start.wd)