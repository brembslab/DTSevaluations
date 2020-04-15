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
library(questionr)
library(data.table)

## source the script with the functions needed for analysis
source("readXMLdatafile.R")
source("DTS_plotfunctions.R")

start.wd = getwd()

## read single YAML project file
project.file <- file.choose()
project.path = dirname(project.file)
project.data<-yaml.load_file(project.file)

#measure the runtime of the whole analysis 
start_time <- Sys.time() #Records the system time at the start of the analysis

#progressbar
totalflies <- length(paste(project.path, unlist(do.call("rbind", lapply(project.data$resources, '[', 4))), sep = "/"))#gets the number of total flies

#make sure all flies have the identical experimental design and find out which don't
xml_list = paste(project.path, unlist(do.call("rbind", lapply(project.data$resources, '[', 4))), sep = "/") #create list of all file names
offending_metanames <- MultiFlyDataVerification(xml_list)
if(!is_null(offending_metanames)) stop("You have selected files with non-equal metadata. Please check the file(s) above for consistency!", cat("Error! File(s) with differing metadata: ", offending_metanames, sep = "\n")) 

#check for duplicates in the raw data and report any occurrences
offending_behavnames <- MultiFlyDuplicateCheck(xml_list)
if(!is.null(offending_behavnames)) stop("There are duplicates in the raw data!", cat("Error! List of duplicate file(s): ", offending_behavnames, sep = "\n"))


#make sure the evaluations are written in a subfolder of the data folder
evaluation.path = paste(project.path,"evaluations", sep = "/")
dir.create(evaluation.path, showWarnings = FALSE)
setwd(evaluation.path)

#collecting essential data for statistics and plots
NofGroups = unname(lengths(project.data["resources"]))                                         #get number of experimental groups
groupnames <- unlist(sapply(project.data[["resources"]], function(x) x["name"]))               #get a vector with all group names
groupdescriptions <- unlist(sapply(project.data[["resources"]], function(x) x["description"])) #get a vector with all group names
signif = project.data[["statistics"]][["significance-levels"]]                                 #get significance levels
priorval = project.data[["statistics"]][["priors"]]                                            #get priors for FPR calculation
twogroupstats <- project.data[["statistics"]][["two.groups"]][["data"]]==1                     #etermine if statistics for two groups are required
wil <- project.data[["statistics"]][["single.groups"]][["data"]]==1                            #determine if we need to do single tests
learningscore = project.data[["statistics"]][["learning-score"]][["data"]]                     #get the PI that is going to be tested

#what kind of experiment are we dealing with? Default is torquemeter
if (exists('type', where=project.data$experiment)){ExpType = project.data$experiment$type} else ExpType = "Torquemeter"
if (ExpType=="Torquemeter" || ExpType=="torquemeter"){FlyBehavior="Torque"} else {FlyBehavior="Platform Position"}

### Initialize empty lists where data are collected
grouped.poshistos <- list()   #Arena position histograms for group in a list of length NofPeriods
grouped.PIprofiles <- list()  #PIProfile data frames in a list of length NofGroups
grouped.Categories <- list()  #For saving categories within groups
grouped.PIcombined <- list()  #For categorical color coding of PIs
grouped.periods <- list()     #Period designs in a list of length NofGroups
grouped.spectra <- list()     #Power spectra in a list of length NofGroups
grouped.flyhistos <- list()   #Fly behavior histograms for group in a list of length NofPeriods

exp_groups <- list()              #Individual fly names in each group for display in project evaluation
grouped.OMdata <-list()           #Averaged optomotor data traces for each group
grouped.OMparams <-list()         #Extracted optomotor parameters for each group
grouped.OMdataBefore <-list()     #Averaged optomotor data traces for each group at start of experiment
grouped.OMparamsBefore <-list()   #Extracted optomotor parameters for each group at start of experiment
grouped.OMdataAfter <-list()      #Averaged optomotor data traces for each group at end of experiment
grouped.OMparamsAfter <-list()    #Extracted optomotor parameters for each group at end of experiment

#create dataframes for dwelling data
dwelldata = dwellplots = dwelltimes = grouped.dwell = list()
dwellvector = NA 
flies = 0 #initialize progress bar

for(x in 1:NofGroups)
{
  #gather necessary data and variables
  grp_title = project.data[["resources"]][[x]][["title"]] #collect title of the group
  grp_description = groupdescriptions[x] #collect description of the group
  xml_list = paste(project.path, project.data[["resources"]][[x]][["data"]], sep = "/") #create list of file names
  if(!exists("samplesizes")) {samplesizes = length(project.data[["resources"]][[x]][["data"]])} else samplesizes[x] = length(project.data[["resources"]][[x]][["data"]]) #get samplesizes
  
  #create/empty lists for collecting all single fly data by period
  period.data <- list()     #data grouped by period
  grouped.data <- list()    #total data grouped
  speclist <- list()        #spectograms
  
  ##progressbar
  #if we are done with the first group, the totalflies get recalculated by subtracting the number of flies in the first group(s). 
  if(x>1){
    flies[x] = length(xml_list)
    flies = sum(flies)
    totalflies = totalflies - flies
  }
  
  #start actually evaluating
  for (l in 1:length(xml_list)) 
  {
    #progress bar
    if (exists("starttime")){iter_time = round((Sys.time()-starttime), 2)} else iter_time = 20  #calculates the iteration time
    if (exists("progressbar")){dev.off()} else 1-1 #deletes the previous plot. If not, this will generate [l] number of plots in the end. If the plot does not exist it gives an error message, hence the 1-1
    progress = round(l*(100/totalflies)) #calculates the progress in percentage
    if(x>1){  #uses this function to calculate the progress if we are past the first group
      progress = round((l+totalflies)*(100/(totalflies+flies))) 
    }
    
    esttime = (Sys.time() + (iter_time * (totalflies-l))) #estimated finish time, based on the last iteration and the number of flies left
    progressbar = barplot(progress, 
                 col = "grey", ylab = "% progress", 
                 ylim=c(0,100), axes = FALSE) #set axis to 100 and then removes it
    axis(2, seq(0,100,50), las=2) #sets the axis ticks and rotates them to a horizontal position
    axis(2, seq(0,100,25), las=2) #sets the axis ticks  
    title(xlab= paste("Iteration time: \n", iter_time, "sec"), line=-9, cex.lab=1.2)
    title((paste("Est. finish time",substring(esttime, 12))), line = -8, cex.lab=1.2)
    text(progressbar,16, paste(progress, "% completed \n flies left", (1+totalflies-l))) #adds the percentage as text and the number of flies left
    starttime = Sys.time() #sets the start time until it reaches this point in the next iteration. 1st iteration is hardcoded to 35 seconds
    
    #load current fly name
    xml_name=xml_list[[l]]
    
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
    
    #create/empty the dataframe for dwellmeans
    if(l==1){
      dwellmeans = list()
      dwellmeans$unpunished <- dwellmeans$punished <- data.frame(matrix(ncol = NofPeriods))}
    
    #### call RMarkdown for single fly evaluations ###############################################
    rmarkdown::render(paste(start.wd,"/single_fly.Rmd", sep=""),                         ######
                      output_file = paste(flyname,"descr_anal.html", sep="_"),            ######
                      output_dir = evaluation.path)                                      ######
    #### end RMarkdown for single fly evaluations ################################################
    
    ##move PIs and categories to multi-experiment data.frames
    if(l>1){
      PIprofile <- rbind2(PIprofile, as.vector(t(sequence$lambda)))      #PIs in one dataframe
      Categories <- rbind2(Categories, as.vector(t(sequence$category)))  #and the categories in another dataframe
    }
    
    ##add period data to grouped data
    grouped.data[[l]] <- period.data
    xml_list[[l]] = paste('<a href="',flyname,'_descr_anal.html">', flyname,'</a>', sep = '')  #create link to each single fly evaluation HTML document to be used in project evaluation
  } #for number of flies in xml_list - from here on group evaluations
  
  exp_groups[[x]] <- c(grp_title, grp_description, xml_list) #add name and description and file links to dataframe to be used in project evaluation document
  
  # derive means and SDs for optomotor data in the group and collect extracted OM parameters
  if(any(grepl("optomotor", sequence$type)==TRUE)){
  }
  
  if(any(grepl("optomotor", sequence$type)==TRUE)){    ###determine if there are optomotor periods
    if (any(!grepl("optomotor", sequence$type)==TRUE)){   ###if there are non-optomotor periods...
      if (grepl("optomotor", sequence$type[1]) & grepl("optomotor", tail(sequence$type, 1))){ ###...and the opto periods are in the beginning and the end
        ##then we have before/after optomotor data
        #before
        OMdataBefore$means=rowMeans(OMdataBefore[-1])
        OMdataBefore$sd=rowSds(OMdataBefore[-1])
        OMdataBefore$group=project.data[["resources"]][[x]][["name"]]
        grouped.OMdataBefore[[x]] <- OMdataBefore #save optomotor data to groupwise list
        rm(OMdataBefore) #remove the optomotor data frame so it can be generated again for the next group
        OMparamsBefore$group=project.data[["resources"]][[x]][["name"]]
        grouped.OMparamsBefore[[x]] <- OMparamsBefore #save extracted optomotor parameters to groupwise list
        rm(OMparamsBefore) #remove the optomotor parameters dataframe so it can be generated again for the next group
        #after
        OMdataAfter$means=rowMeans(OMdataAfter[-1])
        OMdataAfter$sd=rowSds(OMdataAfter[-1])
        OMdataAfter$group=project.data[["resources"]][[x]][["name"]]
        grouped.OMdataAfter[[x]] <- OMdataAfter #save optomotor data to groupwise list
        rm(OMdataAfter) #remove the optomotor data frame so it can be generated again for the next group
        OMparamsAfter$group=project.data[["resources"]][[x]][["name"]]
        grouped.OMparamsAfter[[x]] <- OMparamsAfter #save extracted optomotor parameters to groupwise list
        rm(OMparamsAfter) #remove the optomotor parameters dataframe so it can be generated again for the next group
      }
    } else {
      OMdata$means=rowMeans(OMdata[-1])
      OMdata$sd=rowSds(OMdata[-1])
      OMdata$group=project.data[["resources"]][[x]][["name"]]
      grouped.OMdata[[x]] <- OMdata #save optomotor data to groupwise list
      rm(OMdata) #remove the optomotor data frame so it can be generated again for the next group
      OMparams$group=project.data[["resources"]][[x]][["name"]]
      grouped.OMparams[[x]] <- OMparams #save extracted optomotor parameters to groupwise list
      rm(OMparams) #remove the optomotor parameters dataframe so it can be generated again for the next group
    }
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
  
  
  ## pool all fly and position data into single data.frame
  
  all.data <- do.call(rbind, pooled.data)
  
  
  ## if we have two groups, collect data for superimposed histograms for either fly behavior or position
  if (NofGroups==2){
    if('yt' %in% sequence$type || 'sw' %in% sequence$type) #for yt or sw experiments, collect fly data
    {
      if(x==1){
        histo1 <- data.frame(pooled.data[[learningscore]][["fly"]])
        histo1$v2 = groupnames[x]
        colnames(histo1)=c("fly","group")
      } else {
        histo2 <- data.frame(pooled.data[[learningscore]][["fly"]])
        histo2$v2 = groupnames[x]
        colnames(histo2)=c("fly","group")
        supHistos <- rbind(histo1,histo2) #make dataframe with fly data from both groups and group name as factor
      }
    } else if ('fs' %in% sequence$type || 'color' %in% sequence$type) #for fs or color experiments, collect arena position data and fold them to 0..90°
    {
      if(x==1){
        histo1 <- data.frame(pooled.data[[learningscore]][["a_pos"]])
        histo1$v2 = groupnames[x]
        colnames(histo1)=c("a_pos","group")
        histo1$a_pos = abs(histo1$a_pos)/10    #fold position data over to look at 180° equivalent fixation and bring into degree range
        histo1$a_pos[histo1$a_pos>90] = -histo1$a_pos[histo1$a_pos>90]+180 #fold anything larger than 90° to 0..90°
      } else {
        histo2 <- data.frame(pooled.data[[learningscore]][["a_pos"]])
        histo2$v2 = groupnames[x]
        colnames(histo2)=c("a_pos","group")
        histo2$a_pos = abs(histo2$a_pos)/10  #fold position data over to look at 180° equivalent fixation and bring into degree range
        histo2$a_pos[histo2$a_pos>90] = -histo2$a_pos[histo2$a_pos>90]+180 #fold anything larger than 90° to 0..90°
        supHistos <- rbind(histo1,histo2)  #make dataframe with position data from both groups and group name as factor
      }
    }  
    
  }
  
  
  ########### Collect the data from each group in their respective lists and empty the variables ######################
  
  ###Period sequence design meta-data
  grouped.periods[[x]] = periods
  
  ###fly Histograms
  grouped.flyhistos[[x]] = flyhistos #add fly histograms to list of grouped histograms
  flyhistos <- list() #empty fly histograms
  
  ###Position Histograms
  grouped.poshistos[[x]] = poshistos #add position histograms to list of grouped position histograms
  poshistos <- list() #empty list of position histograms
  
  
  ###Prepare PIs for plotting and statistical analysis###
  
  PIs <- !all(is.na(sequence$lambda)) ###determine if there are any PIs to be plotted
  
  #PIprofiles for statistical analysis (PIs alone, periods as column names)
  colnames(PIprofile) <- sprintf("PI%d", 1:NofPeriods)    #make colnames in PIprofile
  grouped.PIprofiles[[x]] = PIprofile                     #add PIprofile to list of grouped PIs
  PIprofile <- PIprofile[colSums(!is.na(PIprofile)) > 0]  #remove empty columns for combining with categories
  
  #Categories for printing categorical colors (periods as column names)
  colnames(Categories) <- sprintf("PI%d", 1:NofPeriods)     #make colnames in Categories
  grouped.Categories[[x]] = Categories                      #add Categories to list of grouped Categories
  Categories <- Categories[colSums(!is.na(Categories)) > 0] #remove empty columns
  
  # Add column names
  colnames(dwellmeans$punished) <- colnames(dwellmeans$unpunished) <- sprintf("PI%d", 1:NofPeriods)     #make colnames in dwellmeans
  grouped.dwell[[x]] = dwellmeans #Merge single fly dwell data to grouped
  
  #PCombine categories with PIs for plotting (melted, periods as id-variable)
  if (PIs)
  {
    PIcombined <- melt(Categories, measure.vars = names(Categories), variable.name = "period", value.name = "category") #melt data frame to create a variable with periods as id values
    PIcombined["PIs"] = melt(PIprofile)$value                 #combine the categories with the PIs
    grouped.PIcombined[[x]] = PIcombined                      #add PIcombined to list of grouped PIs and categories (for plotting)
    rm(PIcombined)                                            #delete for use in next group
  }

  #Remove items for reuse in the next group
  rm(PIprofile, Categories, dwellmeans)

  
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

###########################################################
######## project plots and statistical evaluations ########
###########################################################

###### Plots ######

###generate important variables for later plotting and annotation
colorrange = project.data[["statistics"]][["color-range"]]
boxcolors = c(colorrange[1:NofGroups])
boxes<-c(1:NofGroups)

#create new dataframes for the chosen PI learningscore values
if(PIs & !is.null(learningscore)){
  PIstat <- list()
  CatStat <- list()
  for(x in 1:NofGroups){
    PIstat[[x]] <- grouped.PIprofiles[[x]][[learningscore]]
    CatStat[[x]] <- grouped.Categories[[x]][[learningscore]]
  }
  PIstat <- as.data.frame(t(plyr::ldply(PIstat, rbind)))                            #convert PI list to data.frame
  colnames(PIstat) <- unlist(sapply(project.data[["resources"]], '[', 'name'))      #add group names as column names to PIstat
  CatStat <-  as.data.frame(t(plyr::ldply(CatStat, rbind)))                         #convert list of categories to data.frame
  colnames(CatStat) <- unlist(sapply(project.data[["resources"]], '[', 'name'))     #add group names as column names to CatStat
  
  #compute standard deviations
  SDs<-as.numeric(apply(PIstat, 2, function(x) sd(na.omit(x))))
  
  #combine PIstat and CatStat for plotting learningscores
  PIstatCombined <- melt(CatStat, measure.vars = names(CatStat), variable.name = "group", value.name = "category") #melt categories into dataframe with group as id-variable
  PIstatCombined["PIs"] = melt(PIstat)$value                                        #combine the categories with the PIs
  PIstatCombined = na.omit(PIstatCombined)                                          #delete NA rows
}


###### if there are more than two groups, try to pool some data into two groups
PooledGroups=FALSE

if(NofGroups>2 & length(unique(groupdescriptions))==2){
  PooledGroups=TRUE #we have several groups, but only one control and one experimental group
  
  #find out which group belongs to which pool
  pool1=unname(groupnames[which(sapply(project.data[["resources"]], function(x) x["description"])==unique(groupdescriptions)[1])])
  pool2=unname(groupnames[which(sapply(project.data[["resources"]], function(x) x["description"])==unique(groupdescriptions)[2])])
  
  #create two new dataframe (one melted one not) with the pooled groups
  #melted df
  PIstatPooled=PIstatCombined #create copy of many group dataframe
  PIstatPooled$group=gsub(x = PIstatPooled$group, pattern = paste(pool1, collapse = "|"), replacement = unique(groupdescriptions)[1]) #rename the ones from the first pool
  PIstatPooled$group=gsub(x = PIstatPooled$group, pattern = paste(pool2, collapse = "|"), replacement = unique(groupdescriptions)[2]) #rename the ones from the second pool
  #unmelted df
  PIprofilePooled=dcast(PIstatPooled[c("group", "PIs")], PIs~...)[,2:3] #create new dataframe for PIs in the pooled groups this doesn't work with unequal samplesizes: as.data.frame(lapply(dcast(PIstatPooled[c("group", "PIs")], PIs~...), na.omit))
  samplesizesPooled=colSums(!is.na(PIprofilePooled))  #find the new samplesizes for the different groups
  
  #create table with pooled groups for later plotting
  sq <- seq(max(length(pool1), length(pool2)))
  PooledGroups <- data.frame(pool1[sq], pool2[sq])
  colnames(PooledGroups)=unique(groupdescriptions)
}

###### continue for all projects with two groups

#### call RMarkdown for project evaluations ################################################
rmarkdown::render(paste(start.wd,"/project.Rmd", sep=""), 
                  output_file = paste(project.data$experiment$name,"html", sep = "."), 
                  output_dir = evaluation.path)
#### end RMarkdown for project evaluations #################################################

Runtime = round(((Sys.time() - start_time)), 3) #Subtracts the endtime with the starttime to get the total analysis time
print(paste0("Runtime per fly was ", ((Runtime)*60)/totalflies, " seconds", ", in total ", round(Runtime, 3), " minutes")) #prints the time per fly and the total time
setwd(start.wd)