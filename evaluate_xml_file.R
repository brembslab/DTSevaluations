#######################################################################################################################
################## R-script to read single XML raw data files, visualize and evaluate data. Reports in HTML ###########
#######################################################################################################################

setwd(dirname(parent.frame(2)$ofile)) #set working directory to source file location (allows loading of function files)

## source libraries and functions needed for analysis
source("include/includes.R")
source("include/importfunctions.R")
source("include/plotfunctions.R")
start.wd=getwd()

## read a single XML data file
xml_name <- file.choose()
dataset.path = dirname(xml_name)

#make sure the evaluations are written in a subfolder of the data folder
evaluation.path = paste(dataset.path,"evaluations", sep = "/")
dir.create(evaluation.path, showWarnings = FALSE)

#create/set variables to remain compatible with single_fly.Rmd
period.data <- list()     #data grouped by period
grouped.data <- list()    #total data grouped
speclist <- list()        #spectograms
l=1                       #set fly counter to 1
#create dataframes for dwelling data
dwelldata = dwellplots = grouped.dwell = list()

##### read the data with the corresponding function #######
singleflydata <- flyDataImport(xml_name)

#extract single fly data
source("include/extractsingleflydata.R")

#what kind of experiment are we dealing with? Default is torquemeter
ExpType <- singleflydata[[14]]
if (tolower(ExpType=="torquemeter")){FlyBehavior="Torque"} else {FlyBehavior="Platform Position"}
    
    
#### call RMarkdown for single fly evaluations ###############################################
    rmarkdown::render(paste(start.wd,"/rmarkdown/single_fly.Rmd", sep=""),              ######
                      output_file = paste(flyname,"descr_anal.html", sep="_"),          ######
                      output_dir = evaluation.path)                                     ######
#### end RMarkdown for single fly evaluations ################################################

setwd(start.wd)