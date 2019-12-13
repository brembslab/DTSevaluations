#####################################################################################################################################
################## R-script to read YAML DTS project files, visualize and statistically evaluate data. Reports in HTML ##############
#####################################################################################################################################

rm(list=ls()) #clean memory
gc()          #collect garbage

library(ggplot2)
library(tidyr)
library(dygraphs)
library(grid)
library(reshape2)
library(plyr)
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
library(dabestr)
library(zoo)
library(tidyverse)
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

#hcollecting essential data for statistics and plots
NofGroups = lengths(project.data["resources"])                                   #get number of experimental groups
signif = project.data[["statistics"]][["significance-levels"]]                   #get significance levels
groupnames <- unlist(sapply(project.data[["resources"]], function(x) x["name"])) #get a vector with all group names
priorval = project.data[["statistics"]][["priors"]]                              #get priors for FPR calculation
twogroupstats <- project.data[["statistics"]][["two.groups"]][["data"]]==1       #etermine if statistics for two groups are required
wil <- project.data[["statistics"]][["single.groups"]][["data"]]==1              #determine if we need to do single tests
learningscore = project.data[["statistics"]][["learning-score"]][["data"]]       #get the PI that is going to be tested

#what kind of experiment are we dealing with? Default is torquemeter
if (exists('type', where=project.data$experiment)){ExpType = project.data$experiment$type} else ExpType = "Torquemeter"
if (ExpType=="Torquemeter" || ExpType=="torquemeter"){FlyBehavior="Torque"} else {FlyBehavior="Platform Position"}

### Initialize empty lists where data are collected
grouped.poshistos <- list()   #Arena position histograms for group in a list of length NofPeriods
grouped.PIprofiles <- list()  #PIProfile data frames in a list of length NofGroups
grouped.periods <- list()     #Period designs in a list of length NofGroups
grouped.spectra <- list()     #Power spectra in a list of length NofGroups
grouped.flyhistos <- list()   #Fly behavior histograms for group in a list of length NofPeriods

exp_groups <- list()    #Individual fly names in each group for display in project evaluation
grouped.OMdata <-list() #Averaged optomotor data traces for each group
grouped.OMparams <-list() #Extracted optomotor parameters for each group

for(x in 1:NofGroups)
{
  grp_title = project.data[["resources"]][[x]][["title"]] #collect title of the group
  grp_description = project.data[["resources"]][[x]][["description"]] #collect description of the group
  xml_list = paste(project.path, project.data[["resources"]][[x]][["data"]], sep = "/") #create list of file names
  if(!exists("samplesizes")) {samplesizes = length(project.data[["resources"]][[x]][["data"]])} else samplesizes[x] = length(project.data[["resources"]][[x]][["data"]]) #samplesizes

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
    real_sample_rate = as.numeric(as.character(singleflydata[[11]]))
    down_sample_rate = as.numeric(as.character(singleflydata[[12]]))
    
    ##extract fly meta-data
    fly <- singleflydata[[3]]
    flyname = fly$name[1]
    
    ##extract experiment meta-data
    experimenter <- singleflydata[[2]]
    experiment <- singleflydata[[4]]

    ##extract the rawdata
    rawdata <- singleflydata[[9]]
    flyrange = singleflydata[[10]]
    traces <- singleflydata[[13]]
    #calculate max fly values for axes when plotting 
    maxfly = c(-round_any(max(abs(flyrange)), 100, f=ceiling),round_any(max(abs(flyrange)), 100, f=ceiling))
    
    #create/empty plot lists
    poshistos <- list()
    flyhistos <- list()
    
#### call RMarkdown for single fly evaluations ###############################################
    rmarkdown::render(paste(start.wd,"/single_fly.Rmd", sep=""),                         ######
                      output_file = paste(flyname,"descr_anal.html", sep="_"),            ######
                      output_dir = evaluation.path)                                      ######
#### end RMarkdown for single fly evaluations ################################################
    
    ##move PIs to multi-experiment data.frame
    if(l>1){
      PIprofile <- rbind2(PIprofile, as.vector(t(sequence$lambda)))
    }
    
    ##add period data to grouped data
    grouped.data[[l]] <- period.data

    xml_list[[l]] = paste('<a href="',flyname,'_descr_anal.html">', flyname,'</a>', sep = '')  #create link to each single fly evaluation HTML document to be used in project evaluation

  } #for number of flies in xml_list

  exp_groups[[x]] <- c(grp_title, grp_description, xml_list) #add name and description and file links to dataframe to be used in project evaluation document
  
  # derive means and SDs for optomotor data in the group and collect extracted OM parameters
if(any(grepl("optomotor", sequence$type)==TRUE)){
  OMdata$means=rowMeans(OMdata[-1])
  OMdata$sd=rowSds(OMdata[-1])
  OMdata$group=project.data[["resources"]][[x]][["name"]]
  grouped.OMdata[[x]] <- OMdata #save optomotor data to groupwise list
  rm(OMdata) #remove the optomotor data frame so it can be generated again for the next group
  OMparams$group=project.data[["resources"]][[x]][["name"]]
  grouped.OMparams[[x]] <- OMparams #save extracted optomotor parameters to groupwise list
  rm(OMparams) #remove the optomotor parameters dataframe so it can be generated again for the next group
}
  

  ########### plot rawdata graphs for all experiments #####################
  
  ##pool all data by period
  
  pooled.data<-list()
  
  for(i in 1:NofPeriods)
  {
    period.data<-data.frame()
    for (l in 1:length(xml_list)) 
      {
        period.data <- rbind(period.data, grouped.data[[l]][[i]])
      }
    pooled.data[[i]] <- period.data
  } #for number of periods
  
  ## plot pooled position and fly histograms by period ##
  
  for(i in 1:NofPeriods)
  {
    temp<-pooled.data[[i]]
    
    #fly
    flyhistos[[i]] <- ggplot(data=temp, aes_string(temp$fly)) +
      geom_histogram(binwidth = 3, fill = sequence$histocolor[i]) +
      labs(x=paste(FlyBehavior, "[arb units]"), y="frequency") +
      xlim(maxfly) +
      ggtitle(paste("Period", i))
    
    #position
    if(sequence$type[i]=="fs" || sequence$type[i]=="color")
    {
      poshistos[[i]] <- ggplot(data=temp, aes_string(temp$a_pos)) +
        geom_histogram(binwidth=10, fill = sequence$histocolor[i]) +
        labs(x="arena position [arb units]", y="frequency") +
        xlim(-1800,1800) +
        ggtitle(paste("Period", i))
    }
  }
  
  ## pool all fly and position data into single data.frame
  
  all.data <- do.call(rbind, pooled.data)
  
  
  ## plot pooled histograms for all flies over all periods
  
  #fly behavior
  flyhistos[[NofPeriods+1]] <- ggplot(data=all.data, aes_string(all.data$fly)) + 
    geom_histogram(binwidth=3) + 
    labs(x=paste(FlyBehavior, "[arb units]"), y="frequency") + 
    xlim(maxfly) +
    ggtitle("Pooled Behavior Histogram")
  
  #position (if there are fs periods)
  if ('fs' %in% sequence$type || 'color' %in% sequence$type) {
  poshistos[[NofPeriods+1]] <- ggplot(data=all.data, aes_string(all.data$a_pos)) + 
    geom_histogram(binwidth=10) +
    labs(x="position [arb units]", y="frequency") + 
    xlim(-1800,1800) +
    ggtitle("Pooled Position Histogram")
  }
  
  ## collect data for superimposed position histograms from two groups
  if (NofGroups==2 & ('fs' %in% sequence$type || 'color' %in% sequence$type))
  {
    if(x==1){
      histo1 <- data.frame(pooled.data[[learningscore]][["a_pos"]])
      histo1$v2 = groupnames[x]
      #colnames(histo1)=c("v1","v2")
      } else {
        histo2 <- data.frame(pooled.data[[learningscore]][["a_pos"]])
        histo2$v2 = groupnames[x]
       # colnames(histo2)=c("v1","v2")
      }
    supHistos <- rbind(histo1,histo2)
    colnames(supHistos) = c("a_pos", "group")
  }

} else stop("You have selected files with differing metadata. Please check your DTS files for consistency!")

###Collect the data from each group in their respective lists and empty the variables

#Period sequence design meta-data
grouped.periods[[x]] = periods

#fly Histograms
grouped.flyhistos[[x]] = flyhistos #add fly histograms to list of grouped histograms
flyhistos <- list() #empty fly histograms

#Position Histograms
grouped.poshistos[[x]] = poshistos #add position histograms to list of grouped position histograms
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
rmarkdown::render(paste(start.wd,"/project.Rmd", sep=""), 
                  output_file = paste(project.data$experiment$name,"html", sep = "."), 
                  output_dir = evaluation.path)
#### end RMarkdown for project evaluations #################################################


setwd(start.wd)