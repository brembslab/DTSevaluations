#######################################################################################################################
################## R-script to read single XML raw data files, visualize and evaluate data. Reports in HTML ###########
#######################################################################################################################

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
library(genefilter)
library(seewave)
library(lubridate)
library(rmarkdown)
library(markdown)
library(knitr)
library(zoo)
library(tidyverse)
library(questionr)
library(data.table)
library(DescTools)
library(magick)
library(reactable)


## source the script with the functions needed for analysis
source("readXMLdatafile.R")
source("DTS_plotfunctions.R")

start.wd = getwd()

## read single YAML project file
xml_name <- file.choose()
project.path = dirname(xml_name)

#make sure the evaluations are written in a subfolder of the data folder
evaluation.path = paste(project.path,"evaluations", sep = "/")
dir.create(evaluation.path, showWarnings = FALSE)
setwd(evaluation.path)

#create/set variables to remain compatible with single_fly.Rmd
period.data <- list()     #data grouped by period
grouped.data <- list()    #total data grouped
speclist <- list()        #spectograms
l=1                       #set fly counter to 1
#create dataframes for dwelling data
dwelldata = dwellplots = grouped.dwell = list()


#start evaluating

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
    ExpType <- singleflydata[[14]]
    #calculate max fly values for axes when plotting 
    maxfly = c(-round_any(max(abs(flyrange)), 100, f=ceiling),round_any(max(abs(flyrange)), 100, f=ceiling))
    
    #create/empty plot lists
    poshistos <- list()
    flyhistos <- list()

    Dwell = any(c("yt","color","sw","fs") %in% sequence$type) ###determine if dwelling times should be calculated
    if (Dwell & l==1){
        dwellmeans = list()
        dwellmeans$unpunished <- dwellmeans$punished <- data.frame(matrix(ncol = NofPeriods))
    }

    nonOMperiods=which(!grepl("optomotor", sequence$type)==TRUE) #vector containing period numbers for non-optomotor periods
    
        
#what kind of experiment are we dealing with? Default is torquemeter
if (tolower(ExpType=="torquemeter")){FlyBehavior="Torque"} else {FlyBehavior="Platform Position"}
    
    
#### call RMarkdown for single fly evaluations ###############################################
    rmarkdown::render(paste(start.wd,"/single_fly.Rmd", sep=""),                         ######
                      output_file = paste(flyname,"descr_anal.html", sep="_"),            ######
                      output_dir = evaluation.path)                                      ######
#### end RMarkdown for single fly evaluations ################################################

setwd(start.wd)